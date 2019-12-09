rm(list = ls())

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(BART)
library(foreign)

expit <- function(x) exp(x)/(1+exp(x))

# tmp_fun <- function(x){
#   x <- as.character(x)
#   x[x == "Dont know"] <- NA
#   x[x == "Very important consideration" | x == "Very important reason"] <- "5"
#   x[x == "Not at all important consideration" | x == "Not at all important reason"] <- "1"
#   as.numeric(as.character(factor(x)))
# }
# 
# slothuus <- read.spss("Slothuus/slothuus.sav", to.data.frame = TRUE)
# 
# summary(slothuus, maxsum = 10)
# 
# welfare <- slothuus %>%
#   dplyr::select(frame, v22, v23, v24, v25, v26, # importance
#                 v27, v28, v29, v30, v31, v32, v33, v34, v35, v36, # content
#                 v21) %>%
#   mutate_at(vars(v22:v36), tmp_fun) %>%
#   mutate(v21 = ifelse(v21 == "Dont know", NA, v21)) %>%
#   filter(frame %in% c("Job frame", "Poor frame")) %>%
#   mutate(ipw = 1, frame = as.numeric(frame)-1)
# 
# nrow(welfare)
# summary(welfare, maxsum = 10)

load("Imai-Yamamoto/PA-ImaiYamamoto.RData")

welfare <- Slothuus %>% filter(!is.na(Y)) %>%
  mutate(gender1 = as.numeric(factor(gender, levels = c(0, 1)))-1,
         educ1 = ifelse(educmiss==TRUE, NA, educ),
         polint1 = ifelse(polintmiss==TRUE, NA, polint),
         ideo1 = ifelse(ideomiss==TRUE, NA, ideo),
         value1 = as.numeric(factor(value, levels = c(0, "extreme")))-1,
         know1 = as.numeric(factor(know, levels = c("low", "mid", "high"))),
         ipw = 1)

##########################################

x <- c("gender1", "educ1", "polint1", "ideo1", "know1", "value1")
a <- "ttt"
l <- c("W1", "W2")
m <- c("M1","M2","M3","M4","M5")
y <- "Y"

formula_te <- as.formula(paste(y, " ~ ", paste(a, collapse= "+")))
# formula_te <- as.formula(paste(y, " ~ ", paste(c(x, a), collapse= "+")))

summary(lm(formula_te, data = welfare))

formula_aly <- as.formula(paste(y, " ~ ", paste(c(x, a, l), collapse= "+")))
formula_almy <- as.formula(paste(y, " ~ ", paste(c(x, a, l, m), collapse= "+")))

K <- 500
N <- nrow(welfare)

out <- matrix(NA, nrow = K, ncol = 5)
out2 <- out

for(i in seq(1, K)){
  
  cat("iteration ", i, "\n")
  
  welfare_bt <- welfare
  welfare_bt <- sample_n(welfare, size = N, replace = TRUE)
  
  treat <- (welfare_bt[[a]]==1)
  Y <- welfare_bt[[y]]
  ipw0 <- welfare_bt$ipw[!treat]
  ipw1 <- welfare_bt$ipw[treat]
  
  # bart with A
  
  # A0 <- A1 <- A <- model.matrix(formula_te, data = welfare_bt)[, -1, drop = FALSE]
  # A0[, a] <- 0; A1[, a] <-1;
  
  # bart_A <- pbart(x.train = A, y.train = Y, printevery=10000L)
  # bart_A_0 <- predict(bart_A, A0)[["prob.test.mean"]]
  # bart_A_1 <- predict(bart_A, A1)[["prob.test.mean"]]
  
  bart_A_0 <- bart_A_1 <- welfare_bt[[y]]
  
  bart_te <- mean(bart_A_1[treat]) - mean(bart_A_0[!treat])
  
  # bart with A and L
  
  AL <- welfare_bt[, c(x, a, l), drop = FALSE] %>% as.matrix()
  AL10 <- AL[!treat, , drop = FALSE]; AL10[, a] <-1;
  AL01 <- AL[treat, , drop = FALSE]; AL01[, a] <-0; 
  
  bart_AL <- wbart(x.train = AL, y.train = Y, printevery=10000L)
  
  bart_AL_10 <- predict(bart_AL, AL10) %>% apply(2, mean)
  bart_AL_01 <- predict(bart_AL, AL01) %>% apply(2, mean)
  
  bart_ay_amy <- weighted.mean(bart_AL_10 - bart_A_0[!treat], w = ipw0)
  bart_ay_amy2 <- weighted.mean(bart_A_1[treat] - bart_AL_01, w = ipw1)
  
  # bart with A, L, and M
  
  ALM <- welfare_bt[, c(x, a, l, m), drop = FALSE] %>% as.matrix()
  ALM10 <- ALM[!treat, , drop = FALSE]; ALM10[, a] <-1;
  ALM01 <- ALM[treat, , drop = FALSE]; ALM01[, a] <-0; 
  
  bart_ALM <- wbart(x.train = ALM, y.train = Y, printevery=10000L)
  
  bart_ALM_10 <- predict(bart_ALM, ALM10) %>% apply(2, mean)
  bart_ALM_01 <- predict(bart_ALM, ALM01) %>% apply(2, mean)
  
  bart_ay <- weighted.mean(bart_ALM_10 - bart_A_0[!treat], w = ipw0)
  bart_ay2 <- weighted.mean(bart_A_1[treat] - bart_ALM_01, w = ipw1)
  
  # construct path-specific effects
  
  bart_amy <- bart_ay_amy - bart_ay
  bart_amy2 <- bart_ay_amy2 - bart_ay2
  
  bart_aly <- bart_te - bart_ay_amy
  bart_aly2 <- bart_te - bart_ay_amy2
  
  bart_almy <- bart_te - bart_ay
  bart_almy2 <- bart_te - bart_ay2
  
  out[i, ] <- c(bart_te, bart_ay, bart_amy, bart_aly, bart_almy)
  out2[i, ] <- c(bart_te, bart_ay2, bart_amy2, bart_aly2, bart_almy2)
  
}

lower <- function(x) quantile(x, 0.025)
upper <- function(x) quantile(x, 0.975)

welfare_decomp <- data.frame(out) %>%
  `names<-`(c("te", "ay", "amy", "aly", "almy")) %>%
  summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "measure")) %>%
  spread(measure, value) %>%
  mutate(estimand = factor(estimand, levels = rev(c("te", "ay", "amy", "aly", "almy")),
                           labels = rev(c("Total Effect", "Direct Effect",
                                          "via Importance", "via Content", "via Content + Importance"))))

welfare_decomp2 <- data.frame(out2) %>%
  `names<-`(c("te", "ay2", "amy2", "aly2", "almy2")) %>%
  summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "measure")) %>%
  spread(measure, value) %>%
  mutate(estimand = factor(estimand, levels = rev(c("te", "ay2", "amy2", "aly2", "almy2")),
                           labels = rev(c("Total Effect", "Direct Effect",
                                          "via Importance", "via Content", "via Content + Importance"))))

save.image("welfare_bart.RData")
