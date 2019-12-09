library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(BART)

library(foreach)

expit <- function(x) exp(x)/(1+exp(x))

CCES10_public <- read_dta("data/CCES10_public.dta")

summary(CCES10_public$strikeo)

peace <- CCES10_public %>%
  dplyr::select(caseid, post, democ, strike, strikef, strikeo, threatc, cost, successc, immoral,
                ally, trade, h1, i1, p1, e1, r1, male, white, age, ed4) %>%
  mutate(ipw = 1)

# mean(peace$strikeo[peace$democ==1]) - mean(peace$strikeo[peace$democ==0])
# lm(formula_te, data = peace)

x <- c("ally", "trade", "h1", "i1", "p1", "e1", "r1", "male", "white", "age", "ed4")
a <- "democ"
l <- c("threatc", "cost", "successc")
m <- "immoral"
y <- "strikeo"

# formula_te <- as.formula(paste(y, " ~ ", paste(c(x, a), collapse= "+")))
formula_aly <- as.formula(paste(y, " ~ ", paste(c(x, a, l), collapse= "+")))
formula_almy <- as.formula(paste(y, " ~ ", paste(c(x, a, l, m), collapse= "+")))
formula_te <- as.formula(paste(y, " ~ ", paste(a, collapse= "+")))

K <- 500
N <- nrow(peace)

cl <- parallel::makeCluster(25)
doParallel::registerDoParallel(cl)

out_all <- foreach(i = seq(1, K), .combine = rbind, .packages = c("dplyr", "BART"), .errorhandling="remove") %dopar% {

  cat("iteration ", i, "\n")

  peace_bt <- peace
  peace_bt <- sample_n(peace, size = N, replace = TRUE)

  treat <- (peace_bt[[a]]==1)
  Y <- peace_bt[[y]]
  ipw0 <- peace_bt$ipw[!treat]
  ipw1 <- peace_bt$ipw[treat]

  # bart with A

  # A0 <- A1 <- A <- model.matrix(formula_te, data = peace_bt)[, -1, drop = FALSE]
  # A0[, a] <- 0; A1[, a] <-1;

  # bart_A <- pbart(x.train = A, y.train = Y, printevery=10000L)
  #
  # bart_A_0 <- predict(bart_A, A0)[["prob.test.mean"]]
  # bart_A_1 <- predict(bart_A, A1)[["prob.test.mean"]]

  bart_A_0 <- bart_A_1 <- peace_bt[[y]]

  bart_te <- mean(bart_A_1[treat]) - mean(bart_A_0[!treat])

  # bart with A and L

  AL <- model.matrix(formula_aly, data = peace_bt)[, -1, drop = FALSE]
  AL10 <- AL[!treat, , drop = FALSE]; AL10[, a] <-1;
  AL01 <- AL[treat, , drop = FALSE]; AL01[, a] <-0;

  bart_AL <- pbart(x.train = AL, y.train = Y, printevery=10000L)

  bart_AL_10 <- predict(bart_AL, AL10)[["prob.test.mean"]]
  bart_AL_01 <- predict(bart_AL, AL01)[["prob.test.mean"]]

  bart_ay_amy <- weighted.mean(bart_AL_10 - bart_A_0[!treat], w = ipw0)
  bart_ay_amy2 <- weighted.mean(bart_A_1[treat] - bart_AL_01, w = ipw1)

  # bart with A, L, and M

  ALM <- model.matrix(formula_almy, data = peace_bt)[, -1, drop = FALSE]
  ALM10 <- ALM[!treat, , drop = FALSE]; ALM10[, a] <-1;
  ALM01 <- ALM[treat, , drop = FALSE]; ALM01[, a] <-0;

  bart_ALM <- pbart(x.train = ALM, y.train = Y, printevery=10000L)

  bart_ALM_10 <- predict(bart_ALM, ALM10)[["prob.test.mean"]]
  bart_ALM_01 <- predict(bart_ALM, ALM01)[["prob.test.mean"]]

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

parallel::stopCluster(cl)

out <- out_all[,1:5]
out2 <- out_all[,6:10]

lower <- function(x) quantile(x, 0.025)
upper <- function(x) quantile(x, 0.975)

peace_decomp <- data.frame(out) %>%
  `names<-`(c("te", "ay", "amy", "aly", "almy")) %>%
  summarise_all(list(est = mean, lower = lower, upper = upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "measure")) %>%
  spread(measure, value) %>%
  mutate(estimand = factor(estimand, levels = rev(c("te", "ay", "amy", "aly", "almy")),
                           labels = rev(c("Total Effect", "Direct Effect",
                                          "via Morality", "via Costs/Benefits", "via Costs/Benefits + Morality"))))

peace_decomp2 <- data.frame(out2) %>%
  `names<-`(c("te", "ay2", "amy2", "aly2", "almy2")) %>%
  summarise_all(list(est = mean, lower = lower, upper = upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "measure")) %>%
  spread(measure, value) %>%
  mutate(estimand = factor(estimand, levels = rev(c("te", "ay2", "amy2", "aly2", "almy2")),
                           labels = rev(c("Total Effect", "Direct Effect",
                                          "via Morality", "via Costs/Benefits", "via Costs/Benefits + Morality"))))

save(peace_decomp, peace_decomp2, file = "data/peace_bart.RData")
