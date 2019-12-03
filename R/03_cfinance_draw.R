library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(BART)

expit <- function(x) exp(x)/(1+exp(x))

lawton <- read_dta("data/lawton.dta")

cfinance <- lawton %>%
  dplyr::select(id_, frame, nodspeec, nodinter, dspeec, dinter, mdspeec, mdinter,
                year, age, sex, ethnic, knowdum, ondum, pid, ideology,
                speech, interests, impspe, impinter, mfopinio) %>%
  filter(frame %in% c(0, 1)) %>%
  mutate(ipw = 1)

nrow(cfinance)
summary(cfinance)

# mean(cfinance$strikeo[cfinance$frame==1]) - mean(cfinance$strikeo[cfinance$frame==0])
# lm(formula_te, data = cfinance)

x <- c("year", "age", "sex", "ethnic", "knowdum", "ondum", "pid", "ideology")
a <- "frame"
l <- c("impspe", "impinter")
m <- c("speech", "interests")
y <- "mfopinio"

formula_te <- as.formula(paste(y, " ~ ", paste(a, collapse= "+")))
# formula_te <- as.formula(paste(y, " ~ ", paste(c(x, a), collapse= "+")))

summary(lm(formula_te, data = cfinance))

formula_aly <- as.formula(paste(y, " ~ ", paste(c(x, a, l), collapse= "+")))
formula_almy <- as.formula(paste(y, " ~ ", paste(c(x, a, l, m), collapse= "+")))

K <- 500
N <- nrow(cfinance)

cl <- parallel::makeCluster(25)
doParallel::registerDoParallel(cl)

out_all <- foreach(i = seq(1, K), .combine = rbind, .packages = c("dplyr", "BART"), .errorhandling="remove") %dopar% {

  cat("iteration ", i, "\n")

  cfinance_bt <- cfinance
  cfinance_bt <- sample_n(cfinance, size = N, replace = TRUE)

  treat <- (cfinance_bt[[a]]==1)
  Y <- cfinance_bt[[y]]
  ipw0 <- cfinance_bt$ipw[!treat]
  ipw1 <- cfinance_bt$ipw[treat]

  # bart with A

  # A0 <- A1 <- A <- model.matrix(formula_te, data = cfinance_bt)[, -1, drop = FALSE]
  # A0[, a] <- 0; A1[, a] <-1;

  # bart_A <- pbart(x.train = A, y.train = Y, printevery=10000L)
  # bart_A_0 <- predict(bart_A, A0)[["prob.test.mean"]]
  # bart_A_1 <- predict(bart_A, A1)[["prob.test.mean"]]

  bart_A_0 <- bart_A_1 <- cfinance_bt[[y]]

  bart_te <- mean(bart_A_1[treat]) - mean(bart_A_0[!treat])

  # bart with A and L

  AL <- cfinance_bt[, c(x, a, l), drop = FALSE] %>% as.matrix()
  AL10 <- AL[!treat, , drop = FALSE]; AL10[, a] <-1;
  AL01 <- AL[treat, , drop = FALSE]; AL01[, a] <-0;

  bart_AL <- wbart(x.train = AL, y.train = Y, printevery=10000L)

  # bart_AL_10 <- predict(bart_AL, AL10) %>% apply(2, mean)
  # bart_AL_01 <- predict(bart_AL, AL01) %>% apply(2, mean)

  mu_bart_AL_10 <- predict(bart_AL, AL10, dodraws = FALSE)
  mu_bart_AL_01 <- predict(bart_AL, AL01, dodraws = FALSE)

  sigma <- mean(bart_AL$sigma)

  bart_AL_10 <- mu_bart_AL_10 + rnorm(length(mu_bart_AL_10), mean = 0, sd = sigma)
  bart_AL_01 <- mu_bart_AL_01 + rnorm(length(mu_bart_AL_01), mean = 0, sd = sigma)

  bart_ay_amy <- weighted.mean(bart_AL_10 - bart_A_0[!treat], w = ipw0)
  bart_ay_amy2 <- weighted.mean(bart_A_1[treat] - bart_AL_01, w = ipw1)

  # bart with A, L, and M

  ALM <- cfinance_bt[, c(x, a, l, m), drop = FALSE] %>% as.matrix()
  ALM10 <- ALM[!treat, , drop = FALSE]; ALM10[, a] <-1;
  ALM01 <- ALM[treat, , drop = FALSE]; ALM01[, a] <-0;

  bart_ALM <- wbart(x.train = ALM, y.train = Y, printevery=10000L)

  mu_bart_ALM_10 <- predict(bart_ALM, ALM10, dodraws = FALSE)
  mu_bart_ALM_01 <- predict(bart_ALM, ALM01, dodraws = FALSE)

  sigma <- mean(bart_ALM$sigma)

  bart_ALM_10 <- mu_bart_ALM_10 + rnorm(length(mu_bart_ALM_10), mean = 0, sd = sigma)
  bart_ALM_01 <- mu_bart_ALM_01 + rnorm(length(mu_bart_ALM_01), mean = 0, sd = sigma)

  bart_ay_amy <- weighted.mean(bart_AL_10 - bart_A_0[!treat], w = ipw0)
  bart_ay_amy2 <- weighted.mean(bart_A_1[treat] - bart_AL_01, w = ipw1)

  bart_ay <- weighted.mean(bart_ALM_10 - bart_A_0[!treat], w = ipw0)
  bart_ay2 <- weighted.mean(bart_A_1[treat] - bart_ALM_01, w = ipw1)

  # construct path-specific effects

  bart_amy <- bart_ay_amy - bart_ay
  bart_amy2 <- bart_ay_amy2 - bart_ay2

  bart_aly <- bart_te - bart_ay_amy
  bart_aly2 <- bart_te - bart_ay_amy2

  bart_almy <- bart_te - bart_ay
  bart_almy2 <- bart_te - bart_ay2

  out <- c(bart_te, bart_ay, bart_amy, bart_aly, bart_almy)
  out2 <- c(bart_te, bart_ay2, bart_amy2, bart_aly2, bart_almy2)

  c(out, out2)

}

stopCluster(cl)

out <- out_all[,1:5]
out2 <- out_all[,6:10]

lower <- function(x) quantile(x, 0.025)
upper <- function(x) quantile(x, 0.975)

cfinance_decomp_draw <- data.frame(out) %>%
  `names<-`(c("te", "ay", "amy", "aly", "almy")) %>%
  summarise_all(list(est = mean, lower = lower, upper = upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "measure")) %>%
  spread(measure, value) %>%
  mutate(estimand = factor(estimand, levels = rev(c("te", "ay", "amy", "aly", "almy")),
                           labels = rev(c("Total Effect", "Direct Effect",
                                          "via Importance", "via Content", "via Content + Importance"))))

cfinance_decomp2_draw <- data.frame(out2) %>%
  `names<-`(c("te", "ay2", "amy2", "aly2", "almy2")) %>%
  summarise_all(list(est = mean, lower = lower, upper = upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "measure")) %>%
  spread(measure, value) %>%
  mutate(estimand = factor(estimand, levels = rev(c("te", "ay2", "amy2", "aly2", "almy2")),
                           labels = rev(c("Total Effect", "Direct Effect",
                                          "via Importance", "via Content", "via Content + Importance"))))

save(cfinance_decomp_draw, cfinance_decomp2_draw, file = "data/cfinance_bart_draw.RData")
