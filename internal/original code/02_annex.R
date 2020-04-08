# rm(list = ls())

library(haven)
library(dplyr)
library(pryr)
library(tidyr)
library(ggplot2)
library(BART)
library(survey)
library(twang)

# impute_mu <- function(x) {x[is.na(x)] <- mean(x, na.rm = TRUE); x}

nan2na <- function(x) {x[is.nan(x)] <- NA; x}

Lupu_Peisakhin <- read_dta("internal/Lupu-Peisakhin/3. Lupu_Peisakhin_Crimea_data.dta")

summary(Lupu_Peisakhin)

tatar_g1 <- Lupu_Peisakhin %>% dplyr::filter(generation == 1) %>%
  mutate(region2 = (region_origin==2), region3 = (region_origin==3), region4 = (region_origin==4),
         dest2 = (destination==2), dest3 = (destination==3),
         trust_g1 = trustCT - trustRU, victim_g1 = vicrussia, fear_g1 = fear) %>%
  dplyr::select(family, kulak:otherprop_pre, violence,  trust_g1:fear_g1)

summary(tatar_g1)

tatar_g2 <- Lupu_Peisakhin %>% dplyr::filter(generation == 2) %>%
  mutate(trust_g2 = trustCT - trustRU, victim_g2 = vicrussia, fear_g2 = fear) %>%
  dplyr::select(family, trust_g2:fear_g2) %>%
  group_by(family) %>%
  summarise(trust_g2 = mean(trust_g2, na.rm = TRUE),
            victim_g2 = mean(victim_g2, na.rm = TRUE),
            fear_g2 = mean(fear_g2, na.rm = TRUE))

tatar_g3 <- Lupu_Peisakhin %>% dplyr::filter(generation == 3) %>%
  mutate(trust_g3 = trustCT - trustRU, victim_g3 = vicrussia, fear_g3 = fear) %>%
  dplyr::select(family, trust_g3:fear_g3, chechens, annex)

dim(tatar_g1)
dim(tatar_g2)
dim(tatar_g3)

tatar <- tatar_g3 %>%
  left_join(tatar_g1, by = "family") %>%
  left_join(tatar_g2, by = "family") %>%
  mutate_at(vars(trust_g3:fear_g3, trust_g2:fear_g2, trust_g1:fear_g1), nan2na) %>%
  # mutate_at(vars(trust_g3:fear_g3, trust_g2:fear_g2, trust_g1:fear_g1), funs(na = as.numeric(is.na(.)))) %>%
  # mutate_at(vars(trust_g3:fear_g3, trust_g2:fear_g2, trust_g1:fear_g1), impute_mu) %>%
  drop_na(violence,
          annex
          #, kulak, prosoviet_pre, religiosity_pre,
          # trust_g3:fear_g3, trust_g2:fear_g2, trust_g1:fear_g1
  ) %>%
  mutate(violence = as.numeric(violence >= 1))

saveRDS(tatar, "internal/tatar.rds")

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

psform_X <- as.formula(paste(a, " ~ ", paste(x, collapse= "+")))
psform_XL <- as.formula(paste(a, " ~ ", paste(c(x, l), collapse= "+")))
psform_XLM <- as.formula(paste(a, " ~ ", paste(c(x, l, m), collapse= "+")))
psform_XLMN <- as.formula(paste(a, " ~ ", paste(c(x, l, m, n), collapse= "+")))

K <- 500
N <- nrow(tatar)
out3 <- out2 <- out1 <- matrix(NA, nrow = K, ncol = 8)

for(i in seq(1, K)){

  cat("iteration ", i, "\n")

  # bootstrapped data

  # block bootstrap at the family level
  # lookup <- split(1:nrow(tatar_bt), tatar$family)
  # fam <- names(lookup)
  # fam_boot <- sample(fam, size = length(fam), replace = TRUE)
  # tatar_bt <- tatar_bt[unlist(fam_boot), ]

  # tatar_bt <- tatar

  tatar_bt <- sample_n(tatar, size = N, replace = TRUE)

  treat <- (tatar_bt[[a]]==1)
  Y <- tatar_bt[[y]]

  # ps model given X

  psmod_X <- ps(psform_X, data = data.frame(tatar_bt), n.trees = 2000, stop.method = "es.mean")

  ipw1 <- mean(treat)/psmod_X$ps$es.mean.ATE
  ipw0 <- (1 - mean(treat))/(1 - psmod_X$ps$es.mean.ATE)

  odds_X <- psmod_X$ps$es.mean.ATE/(1 - psmod_X$ps$es.mean.ATE)

  # odds[A|X, L] with GBM

  psmod_XL <- ps(psform_XL, data = data.frame(tatar_bt), n.trees = 2000, stop.method = "es.mean")

  odds_XL <- psmod_XL$ps$es.mean.ATE/(1 - psmod_XL$ps$es.mean.ATE)

  # odds[A|X, L, M] with GBM

  psmod_XLM <- ps(psform_XLM, data = data.frame(tatar_bt), n.trees = 2000, stop.method = "es.mean")

  odds_XLM <- psmod_XLM$ps$es.mean.ATE/(1 - psmod_XLM$ps$es.mean.ATE)

  # odds[A|X, L, M, N] with GBM

  psmod_XLMN <- ps(psform_XLMN, data = data.frame(tatar_bt), n.trees = 2000, stop.method = "es.mean")

  odds_XLMN <- psmod_XLMN$ps$es.mean.ATE/(1 - psmod_XLMN$ps$es.mean.ATE)

  # weighting estimates of counterfactual means

  wt_y1000 <- odds_X/odds_XLMN * ipw1
  wt_y1001 <- odds_X/odds_XLM * ipw1
  wt_y1011 <- odds_X/odds_XL * ipw1
  wt_y1111 <- ipw1

  wt_Ey1000 <- weighted.mean(Y[treat], wt_y1000[treat])
  wt_Ey1001 <- weighted.mean(Y[treat], wt_y1001[treat])
  wt_Ey1011 <- weighted.mean(Y[treat], wt_y1011[treat])
  wt_Ey1111 <- weighted.mean(Y[treat], wt_y1111[treat])

  wt_y0111 <- odds_XLMN/odds_X * ipw0
  wt_y0110 <- odds_XLM/odds_X * ipw0
  wt_y0100 <- odds_XL/odds_X * ipw0
  wt_y0000 <- ipw0

  wt_Ey0111 <- weighted.mean(Y[!treat], wt_y0111[!treat])
  wt_Ey0110 <- weighted.mean(Y[!treat], wt_y0110[!treat])
  wt_Ey0100 <- weighted.mean(Y[!treat], wt_y0100[!treat])
  wt_Ey0000 <- weighted.mean(Y[!treat], wt_y0000[!treat])

  # pretreatment confounders matrices
  X <- tatar_bt[, c(x), drop = FALSE] %>% as.matrix()
  X0 <- X[!treat, , drop = FALSE]
  X1 <- X[treat, , drop = FALSE]

  # E[Y|A, X] with BART

  A0000 <- A1111 <- A <- tatar_bt[, c(x, a), drop = FALSE] %>% as.matrix()
  A0000[, a] <- 0; A1111[, a] <- 1;

  bart_A <- pbart(x.train = A, y.train = Y, printevery=10000L)

  hyb_Ey0000 <- imp_Ey0000 <- predict(bart_A, A0000)[["prob.test.mean"]] %>% mean()
  hyb_Ey1111 <- imp_Ey1111 <- predict(bart_A, A1111)[["prob.test.mean"]] %>% mean()

  # E[Y|A, X, L] with BART

  AL <- tatar_bt[, c(x, a, l), drop = FALSE] %>% as.matrix()
  AL1011 <- AL[!treat, , drop = FALSE]; AL1011[, a] <-1;
  AL0100 <- AL[treat, , drop = FALSE]; AL0100[, a] <-0;

  bart_AL <- pbart(x.train = AL, y.train = Y, printevery=10000L)

  imp_y1011 <- predict(bart_AL, AL1011)[["prob.test.mean"]]
  imp_y0100 <- predict(bart_AL, AL0100)[["prob.test.mean"]]

  bart_y1011 <- wbart(x.train = X0, y.train = imp_y1011, x.test = X)
  bart_y0100 <- wbart(x.train = X1, y.train = imp_y0100, x.test = X)

  imp_Ey1011 <- bart_y1011$yhat.test.mean %>% mean()
  imp_Ey0100 <- bart_y0100$yhat.test.mean %>% mean()

  hyb_Ey1011 <- weighted.mean(imp_y1011, w = ipw0[!treat])
  hyb_Ey0100 <- weighted.mean(imp_y0100, w = ipw1[treat])

  # E[Y|A, X, L, M] with BART

  ALM <- tatar_bt[, c(x, a, l, m), drop = FALSE] %>% as.matrix()
  ALM1001 <- ALM[!treat, , drop = FALSE]; ALM1001[, a] <-1;
  ALM0110 <- ALM[treat, , drop = FALSE]; ALM0110[, a] <-0;

  bart_ALM <- pbart(x.train = ALM, y.train = Y, printevery=10000L)

  imp_y1001 <- predict(bart_ALM, ALM1001)[["prob.test.mean"]]
  imp_y0110 <- predict(bart_ALM, ALM0110)[["prob.test.mean"]]

  bart_y1001 <- wbart(x.train = X0, y.train = imp_y1001, x.test = X)
  bart_y0110 <- wbart(x.train = X1, y.train = imp_y0110, x.test = X)

  imp_Ey1001 <- bart_y1001$yhat.test.mean %>% mean()
  imp_Ey0110 <- bart_y0110$yhat.test.mean %>% mean()

  hyb_Ey1001 <- weighted.mean(imp_y1001, w = ipw0[!treat])
  hyb_Ey0110 <- weighted.mean(imp_y0110, w = ipw1[treat])

  # E[Y|A, X, L, M] with BART

  ALMN <- tatar_bt[, c(x, a, l, m, n), drop = FALSE] %>% as.matrix()
  ALMN1000 <- ALMN[!treat, , drop = FALSE]; ALMN1000[, a] <-1;
  ALMN0111 <- ALMN[treat, , drop = FALSE]; ALMN0111[, a] <-0;

  bart_ALMN <- pbart(x.train = ALMN, y.train = Y, printevery=10000L)

  imp_y1000 <- predict(bart_ALMN, ALMN1000)[["prob.test.mean"]]
  imp_y0111 <- predict(bart_ALMN, ALMN0111)[["prob.test.mean"]]

  bart_y1000 <- wbart(x.train = X0, y.train = imp_y1000, x.test = X)
  bart_y0111 <- wbart(x.train = X1, y.train = imp_y0111, x.test = X)

  imp_Ey1000 <- bart_y1000$yhat.test.mean %>% mean()
  imp_Ey0111 <- bart_y0111$yhat.test.mean %>% mean()

  hyb_Ey1000 <- weighted.mean(imp_y1000, w = ipw0[!treat])
  hyb_Ey0111 <- weighted.mean(imp_y0111, w = ipw1[treat])

  # store estimates in holders

  out1[i, ] <- c(wt_Ey0000, wt_Ey1000, wt_Ey1001, wt_Ey1011, wt_Ey0100, wt_Ey0110, wt_Ey0111, wt_Ey1111)
  out2[i, ] <- c(imp_Ey0000, imp_Ey1000, imp_Ey1001, imp_Ey1011, imp_Ey0100, imp_Ey0110, imp_Ey0111, imp_Ey1111)
  out3[i, ] <- c(hyb_Ey0000, hyb_Ey1000, hyb_Ey1001, hyb_Ey1011, hyb_Ey0100, hyb_Ey0110, hyb_Ey0111, hyb_Ey1111)

}

lower <- function(x) quantile(x, 0.025)
upper <- function(x) quantile(x, 0.975)

tatar_decomp_wt <- data.frame(out1) %>%
  `names<-`(c("Ey0000", "Ey1000", "Ey1001", "Ey1011", "Ey0100", "Ey0110", "Ey0111", "Ey1111")) %>%
  transmute(ate_typeI = Ey1111 - Ey0000, ay_typeI = Ey1000 - Ey0000, any_typeI = Ey1001 - Ey1000,
            amy_typeI = Ey1011 - Ey1001, aly_typeI = Ey1111 - Ey1011,
            ate_typeII = Ey1111 - Ey0000, ay_typeII = Ey1111 - Ey0111, any_typeII = Ey0111 - Ey0110,
            amy_typeII = Ey0110 - Ey0100, aly_typeII = Ey0100 - Ey0000) %>%
  summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "decomp", "measure")) %>%
  spread(measure, value) %>%
  mutate(estimand = factor(estimand, levels = rev(c("ate", "ay", "aly", "amy", "any")),
                           labels = rev(c("Total Effect", "Direct Effect",
                                          "via G1 Identity", "via G2 Identity", "via G3 Identity"))))

tatar_decomp_imp <- data.frame(out2) %>%
  `names<-`(c("Ey0000", "Ey1000", "Ey1001", "Ey1011", "Ey0100", "Ey0110", "Ey0111", "Ey1111")) %>%
  transmute(ate_typeI = Ey1111 - Ey0000, ay_typeI = Ey1000 - Ey0000, any_typeI = Ey1001 - Ey1000,
            amy_typeI = Ey1011 - Ey1001, aly_typeI = Ey1111 - Ey1011,
            ate_typeII = Ey1111 - Ey0000, ay_typeII = Ey1111 - Ey0111, any_typeII = Ey0111 - Ey0110,
            amy_typeII = Ey0110 - Ey0100, aly_typeII = Ey0100 - Ey0000) %>%
  summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "decomp", "measure")) %>%
  spread(measure, value) %>%
  mutate(estimand = factor(estimand, levels = rev(c("ate", "ay", "aly", "amy", "any")),
                           labels = rev(c("Total Effect", "Direct Effect",
                                          "via G1 Identity", "via G2 Identity", "via G3 Identity"))))

tatar_decomp_hyb <- data.frame(out3) %>%
  `names<-`(c("Ey0000", "Ey1000", "Ey1001", "Ey1011", "Ey0100", "Ey0110", "Ey0111", "Ey1111")) %>%
  transmute(ate_typeI = Ey1111 - Ey0000, ay_typeI = Ey1000 - Ey0000, any_typeI = Ey1001 - Ey1000,
            amy_typeI = Ey1011 - Ey1001, aly_typeI = Ey1111 - Ey1011,
            ate_typeII = Ey1111 - Ey0000, ay_typeII = Ey1111 - Ey0111, any_typeII = Ey0111 - Ey0110,
            amy_typeII = Ey0110 - Ey0100, aly_typeII = Ey0100 - Ey0000) %>%
  summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "decomp", "measure")) %>%
  spread(measure, value) %>%
  mutate(estimand = factor(estimand, levels = rev(c("ate", "ay", "aly", "amy", "any")),
                           labels = rev(c("Total Effect", "Direct Effect",
                                          "via G1 Identity", "via G2 Identity", "via G3 Identity"))))

save.image("internal/annex_out.RData")
