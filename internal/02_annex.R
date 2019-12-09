library(haven)
library(dplyr)
library(pryr)
library(tidyr)
library(ggplot2)
library(BART)
library(survey)
library(twang)

library(foreach)

# impute_mu <- function(x) {x[is.na(x)] <- mean(x, na.rm = TRUE); x}

Lupu_Peisakhin <- read_dta("data/3. Lupu_Peisakhin_Crimea_data.dta")

summary(Lupu_Peisakhin)

tatar_g1 <- Lupu_Peisakhin %>% filter(generation == 1) %>%
  mutate(region2 = (region_origin==2), region3 = (region_origin==3), region4 = (region_origin==4),
         dest2 = (destination==2), dest3 = (destination==3),
         trust_g1 = trustCT - trustRU, victim_g1 = vicrussia, fear_g1 = fear) %>%
  dplyr::select(family, kulak:otherprop_pre, violence,  trust_g1:fear_g1)

summary(tatar_g1)

tatar_g2 <- Lupu_Peisakhin %>% filter(generation == 2) %>%
  mutate(trust_g2 = trustCT - trustRU, victim_g2 = vicrussia, fear_g2 = fear) %>%
  dplyr::select(family, trust_g2:fear_g2) %>%
  group_by(family) %>%
  summarise(trust_g2 = mean(trust_g2, na.rm = TRUE),
            victim_g2 = mean(victim_g2, na.rm = TRUE),
            fear_g2 = mean(fear_g2, na.rm = TRUE))

tatar_g3 <- Lupu_Peisakhin %>% filter(generation == 3) %>%
  mutate(trust_g3 = trustCT - trustRU, victim_g3 = vicrussia, fear_g3 = fear) %>%
  dplyr::select(family, trust_g3:fear_g3, chechens, annex)

dim(tatar_g1)
dim(tatar_g2)
dim(tatar_g3)

tatar <- tatar_g3 %>%
  left_join(tatar_g1, by = "family") %>%
  left_join(tatar_g2, by = "family") %>%
  # mutate_at(vars(trust_g3:fear_g3, trust_g2:fear_g2, trust_g1:fear_g1), funs(na = as.numeric(is.na(.)))) %>%
  # mutate_at(vars(trust_g3:fear_g3, trust_g2:fear_g2, trust_g1:fear_g1), impute_mu) %>%
  drop_na(violence,
          annex,
          kulak, prosoviet_pre, religiosity_pre,
          trust_g3:fear_g3, trust_g2:fear_g2, trust_g1:fear_g1) %>%
  mutate(violence = as.numeric(violence >= 1))

names(tatar)
dim(tatar)

# quantile(tatar$violence, c(0.25, 0.75))

#############################

x <- c("kulak", "prosoviet_pre", "religiosity_pre",
       "house_pre", "land_pre", "orchard_pre",
       "animals_pre", "carriage_pre", "otherprop_pre")
a <- "violence"
l <- c("trust_g1", "victim_g1", "fear_g1")
m <- c("trust_g2", "victim_g2", "fear_g2")
n <- c("trust_g3", "victim_g3", "fear_g3")
y <- "annex"

# outcome models

# formula_te <- as.formula(paste(y, " ~ ", paste(c(x, a), collapse= "+")))
# formula_aly <- as.formula(paste(y, " ~ ", paste(c(x, a, l), collapse= "+")))
# formula_almy <- as.formula(paste(y, " ~ ", paste(c(x, a, l, m), collapse= "+")))
# lm(formula_te, data = tatar)

K <- 500
N <- nrow(tatar)

cl <- parallel::makeCluster(25)
doParallel::registerDoParallel(cl)

out_all <- foreach(i = seq(1, K), .combine = rbind, .packages = c("dplyr", "BART"), .errorhandling="remove") %dopar% {

  cat("iteration ", i, "\n")

  # bootstrapped data

  tatar_bt <- tatar

  # block bootstrap at the family level
  # lookup <- split(1:nrow(tatar_bt), tatar$family)
  # fam <- names(lookup)
  # fam_boot <- sample(fam, size = length(fam), replace = TRUE)
  # tatar_bt <- tatar_bt[unlist(fam_boot), ]

  tatar_bt <- sample_n(tatar, size = N, replace = TRUE)

  treat <- (tatar_bt[[a]]==1)
  Y <- tatar_bt[[y]]

  # ps model

  formula_ps <- as.formula(paste(a, " ~ ", paste(x, collapse= "+")))

  psmod <- glm(formula_ps, data = data.frame(tatar_bt), family = binomial(link = "logit"))
  pscore <- fitted(psmod)

  tatar_bt <- tatar_bt %>%
    mutate(pscore = pscore) %>%
    mutate(ipw = violence * mean(violence)/pscore +
             (1-violence) * (1 - mean(violence))/(1-pscore))

  ipw0 <- tatar_bt$ipw[!treat]
  ipw1 <- tatar_bt$ipw[treat]

  # pretreatment confounders matrices
  X <- tatar_bt[, c(x), drop = FALSE] %>% as.matrix()
  X0 <- X[!treat, , drop = FALSE]
  X1 <- X[treat, , drop = FALSE]

  # bart with A

  A0 <- A1 <- A <- tatar_bt[, c(x, a), drop = FALSE] %>% as.matrix()
  A0[, a] <- 0; A1[, a] <-1;

  bart_A <- pbart(x.train = A, y.train = Y)

  bart_A_0 <- predict(bart_A, A0)[["prob.test.mean"]]
  bart_A_1 <- predict(bart_A, A1)[["prob.test.mean"]]

  bart_te <- mean(bart_A_1 - bart_A_0)

  # bart with A and L

  AL <- tatar_bt[, c(x, a, l), drop = FALSE] %>% as.matrix()
  AL10 <- AL[!treat, , drop = FALSE]; AL10[, a] <-1;
  AL01 <- AL[treat, , drop = FALSE]; AL01[, a] <-0;

  bart_AL <- pbart(x.train = AL, y.train = Y)

  bart_AL_10 <- predict(bart_AL, AL10)[["prob.test.mean"]]
  bart_AL_01 <- predict(bart_AL, AL01)[["prob.test.mean"]]

  bart_ay_amy_any <- weighted.mean(bart_AL_10 - bart_A_0[!treat], w = ipw0)
  bart_ay_amy_any2 <- weighted.mean(bart_A_1[treat] - bart_AL_01, w = ipw1)

  # pure imputation

  bart_AL_supp <- wbart(x.train = X0, y.train = bart_AL_10 - bart_A_0[!treat], x.test = X)
  bart_AL_supp2 <- wbart(x.train = X1, y.train = bart_A_1[treat] - bart_AL_01, x.test = X)

  bart_ay_amy_any_pi <- bart_AL_supp$yhat.test.mean %>% mean()
  bart_ay_amy_any2_pi <- bart_AL_supp2$yhat.test.mean %>% mean()

  # bart with A, L, and M

  ALM <- tatar_bt[, c(x, a, l, m), drop = FALSE] %>% as.matrix()
  ALM10 <- ALM[!treat, , drop = FALSE]; ALM10[, a] <-1;
  ALM01 <- ALM[treat, , drop = FALSE]; ALM01[, a] <-0;

  bart_ALM <- pbart(x.train = ALM, y.train = Y)

  bart_ALM_10 <- predict(bart_ALM, ALM10)[["prob.test.mean"]]
  bart_ALM_01 <- predict(bart_ALM, ALM01)[["prob.test.mean"]]

  bart_ay_any <- weighted.mean(bart_ALM_10 - bart_A_0[!treat], w = ipw0)
  bart_ay_any2 <- weighted.mean(bart_A_1[treat] - bart_ALM_01, w = ipw1)

  # pure imputation

  bart_ALM_supp <- wbart(x.train = X0, y.train = bart_ALM_10 - bart_A_0[!treat], x.test = X)
  bart_ALM_supp2 <- wbart(x.train = X1, y.train = bart_A_1[treat] - bart_ALM_01, x.test = X)

  bart_ay_any_pi <- bart_ALM_supp$yhat.test.mean %>% mean()
  bart_ay_any2_pi <- bart_ALM_supp2$yhat.test.mean %>% mean()

  # bart with A, L, M and N

  ALMN <- tatar_bt[, c(x, a, l, m, n), drop = FALSE] %>% as.matrix()
  ALMN10 <- ALMN[!treat, , drop = FALSE]; ALMN10[, a] <-1;
  ALMN01 <- ALMN[treat, , drop = FALSE]; ALMN01[, a] <-0;

  bart_ALMN <- pbart(x.train = ALMN, y.train = Y)

  bart_ALMN_10 <- predict(bart_ALMN, ALMN10)[["prob.test.mean"]]
  bart_ALMN_01 <- predict(bart_ALMN, ALMN01)[["prob.test.mean"]]

  bart_ay <- weighted.mean(bart_ALMN_10 - bart_A_0[!treat], w = ipw0)
  bart_ay2 <- weighted.mean(bart_A_1[treat] - bart_ALMN_01, w = ipw1)

  # pure imputation

  bart_ALMN_supp <- wbart(x.train = X0, y.train = bart_ALMN_10 - bart_A_0[!treat], x.test = X)
  bart_ALMN_supp2 <- wbart(x.train = X1, y.train = bart_A_1[treat] - bart_ALMN_01, x.test = X)

  bart_ay_pi <- bart_ALMN_supp$yhat.test.mean %>% mean()
  bart_ay2_pi <- bart_ALMN_supp2$yhat.test.mean %>% mean()

  # construct path-specific effects

  bart_any <- bart_ay_any - bart_ay
  bart_any2 <- bart_ay_any2 - bart_ay2

  bart_amy <- bart_ay_amy_any - bart_ay_any
  bart_amy2 <- bart_ay_amy_any2 - bart_ay_any2

  bart_aly <- bart_te - bart_ay_amy_any
  bart_aly2 <- bart_te - bart_ay_amy_any2

  out <- c(bart_te, bart_ay, bart_aly, bart_amy, bart_any)
  out2 <- c(bart_te, bart_ay2, bart_aly2, bart_amy2, bart_any2)

  bart_any_pi <- bart_ay_any_pi - bart_ay_pi
  bart_any2_pi <- bart_ay_any2_pi - bart_ay2_pi

  bart_amy_pi <- bart_ay_amy_any_pi - bart_ay_any_pi
  bart_amy2_pi <- bart_ay_amy_any2_pi - bart_ay_any2_pi

  bart_aly_pi <- bart_te - bart_ay_amy_any_pi
  bart_aly2_pi <- bart_te - bart_ay_amy_any2_pi

  out_pi <- c(bart_te, bart_ay_pi, bart_aly_pi, bart_amy_pi, bart_any_pi)
  out2_pi <- c(bart_te, bart_ay2_pi, bart_aly2_pi, bart_amy2_pi, bart_any2_pi)

  c(out, out2, out_pi, out2_pi)

}

parallel::stopCluster(cl)

out <- out_all[,1:5]
out2 <- out_all[,6:10]
out_pi <- out_all[,11:15]
out2_pi <- out_all[,16:20]

lower <- function(x) quantile(x, 0.025)
upper <- function(x) quantile(x, 0.975)

tatar_decomp <- data.frame(out) %>%
  `names<-`(c("te", "ay", "aly", "amy", "any")) %>%
  summarise_all(list(est = mean, lower = lower, upper = upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "measure")) %>%
  spread(measure, value) %>%
  mutate(estimand = factor(estimand, levels = rev(c("te", "ay", "aly", "amy", "any")),
                           labels = rev(c("Total Effect", "Direct Effect",
                                          "via G1 Identity", "via G2 Identity", "via G3 Identity"))))

tatar_decomp2 <- data.frame(out2) %>%
  `names<-`(c("te", "ay2", "aly2", "amy2", "any2")) %>%
  summarise_all(list(est = mean, lower = lower, upper = upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "measure")) %>%
  spread(measure, value) %>%
  mutate(estimand = factor(estimand, levels = rev(c("te", "ay2", "aly2", "amy2", "any2")),
                           labels = rev(c("Total Effect", "Direct Effect",
                                          "via G1 Identity", "via G2 Identity", "via G3 Identity"))))

tatar_decomp_pi <- data.frame(out_pi) %>%
  `names<-`(c("te", "ay", "aly", "amy", "any")) %>%
  summarise_all(list(est = mean, lower = lower, upper = upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "measure")) %>%
  spread(measure, value) %>%
  mutate(estimand = factor(estimand, levels = rev(c("te", "ay", "aly", "amy", "any")),
                           labels = rev(c("Total Effect", "Direct Effect",
                                          "via G1 Identity", "via G2 Identity", "via G3 Identity"))))

tatar_decomp2_pi <- data.frame(out2_pi) %>%
  `names<-`(c("te", "ay2", "aly2", "amy2", "any2")) %>%
  summarise_all(list(est = mean, lower = lower, upper = upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "measure")) %>%
  spread(measure, value) %>%
  mutate(estimand = factor(estimand, levels = rev(c("te", "ay2", "aly2", "amy2", "any2")),
                           labels = rev(c("Total Effect", "Direct Effect",
                                          "via G1 Identity", "via G2 Identity", "via G3 Identity"))))

save(tatar_decomp, tatar_decomp2, tatar_decomp_pi, tatar_decomp2_pi, file = "data/annex_bart.RData")
