rm(list = ls())

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(BART)
library(foreign)
library(twang)

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
#                 v27, v28, v29, v30, v31, v32, v33, v34, v35, v36, # Beliefs
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
         know1 = as.numeric(factor(know, levels = c("low", "mid", "high"))))

##########################################

x <- c("gender1", "educ1", "polint1", "ideo1", "know1", "value1")
a <- "ttt"
l <- c("W1", "W2")
m <- c("M1","M2","M3","M4","M5")
y <- "Y"

psform_X <- as.formula(paste(a, " ~ ", paste(x, collapse= "+")))
psform_XL <- as.formula(paste(a, " ~ ", paste(c(x, l), collapse= "+")))
psform_XLM <- as.formula(paste(a, " ~ ", paste(c(x, l, m), collapse= "+")))

K <- 500
N <- nrow(welfare)
out3 <- out2 <- out1 <- matrix(NA, nrow = K, ncol = 6)

for(i in seq(1, K)){
  
  cat("iteration ", i, "\n")
  
  # welfare_bt <- welfare
  welfare_bt <- sample_n(welfare, size = N, replace = TRUE)
  
  treat <- (welfare_bt[[a]]==1)
  Y <- welfare_bt[[y]]
  
  # estimates of counterfactual means: Ey000 and Ey111
  
  wt_Ey000 <- imp_Ey000 <- mean(welfare_bt[[y]][!treat])
  wt_Ey111 <- imp_Ey111 <- mean(welfare_bt[[y]][treat])

  # P[A=0|X] = P[A=0] by design
  
  ipw0 <- rep(1, N); ipw1 <- rep(1, N)
  
  # odds[A|X] = odds[A] by design
  
  odds_X <- mean(treat)/(1 - mean(treat))
  
  # odds[A|X, L] with GBM
  
  psmod_XL <- ps(psform_XL, data = data.frame(welfare_bt), n.trees = 2000, stop.method = "es.mean")
  
  odds_XL <- psmod_XL$ps$es.mean.ATE/(1-psmod_XL$ps$es.mean.ATE)
  
  # odds[A|X, L, M] with GBM
  
  psmod_XLM <- ps(psform_XLM, data = data.frame(welfare_bt), n.trees = 2000, stop.method = "es.mean")
  
  odds_XLM <- psmod_XLM$ps$es.mean.ATE/(1-psmod_XLM$ps$es.mean.ATE)
  
  # weighting estimates of counterfactual means
  
  wt_y100 <- odds_X/odds_XLM * ipw1
  wt_y101 <- odds_X/odds_XL * ipw1
  
  wt_Ey100 <- weighted.mean(Y[treat], wt_y100[treat])
  wt_Ey101 <- weighted.mean(Y[treat], wt_y101[treat])
  
  wt_y011 <- odds_XLM/odds_X * ipw0
  wt_y010 <- odds_XL/odds_X * ipw0
  
  wt_Ey011 <- weighted.mean(Y[!treat], wt_y011[!treat])
  wt_Ey010 <- weighted.mean(Y[!treat], wt_y010[!treat])
  
  # E[Y|A, X, L] with BART
  
  AL <- welfare_bt[, c(x, a, l), drop = FALSE] %>% as.matrix()
  AL101 <- AL[!treat, , drop = FALSE]; AL101[, a] <-1;
  AL010 <- AL[treat, , drop = FALSE]; AL010[, a] <-0; 
  
  bart_AL <- wbart(x.train = AL, y.train = Y, printevery=10000L)
  
  imp_y101 <- predict(bart_AL, AL101) %>% colMeans()
  imp_y010 <- predict(bart_AL, AL010) %>% colMeans()
  
  imp_Ey101 <- weighted.mean(imp_y101, w = ipw0[!treat])
  imp_Ey010 <- weighted.mean(imp_y010, w = ipw1[treat])
  
  # E[Y|A, X, L, M] with BART
  
  ALM <- welfare_bt[, c(x, a, l, m), drop = FALSE] %>% as.matrix()
  ALM100 <- ALM[!treat, , drop = FALSE]; ALM100[, a] <-1;
  ALM011 <- ALM[treat, , drop = FALSE]; ALM011[, a] <-0; 
  
  bart_ALM <- wbart(x.train = ALM, y.train = Y, printevery=10000L)
  
  imp_y100 <- predict(bart_ALM, ALM100) %>% colMeans()
  imp_y011 <- predict(bart_ALM, ALM011) %>% colMeans()
  
  imp_Ey100 <- weighted.mean(imp_y100, w = ipw0[!treat])
  imp_Ey011 <- weighted.mean(imp_y011, w = ipw1[treat])
  
  # store estimates in holders
  
  out1[i, ] <- c(wt_Ey000, wt_Ey100, wt_Ey101, wt_Ey010, wt_Ey011, wt_Ey111)
  out2[i, ] <- c(imp_Ey000, imp_Ey100, imp_Ey101, imp_Ey010, imp_Ey011, imp_Ey111)
  
}

lower <- function(x) quantile(x, 0.025)
upper <- function(x) quantile(x, 0.975)

welfare_decomp_wt <- data.frame(out1) %>%
  `names<-`(c("Ey000", "Ey100", "Ey101", "Ey010", "Ey011", "Ey111")) %>%
  transmute(ate_typeI = Ey111 - Ey000, ay_typeI = Ey100 - Ey000, amy_typeI = Ey101 - Ey100,
            aly_typeI = Ey111 - Ey101, almy_typeI = aly_typeI + amy_typeI,
            ate_typeII = Ey111 - Ey000, ay_typeII = Ey111 - Ey011, amy_typeII = Ey011 - Ey010,
            aly_typeII = Ey010 - Ey000, almy_typeII = aly_typeII + amy_typeII) %>%
  summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "decomp", "measure")) %>%
  spread(measure, value) %>%
  mutate(estimand = factor(estimand, levels = rev(c("ate", "ay", "aly", "amy", "almy")),
                           labels = rev(c("Total Effect", "Direct Effect", "via Beliefs",
                                          "via Importance", "via Beliefs + Importance"))))

welfare_decomp_imp <- data.frame(out2) %>%
  `names<-`(c("Ey000", "Ey100", "Ey101", "Ey010", "Ey011", "Ey111")) %>%
  transmute(ate_typeI = Ey111 - Ey000, ay_typeI = Ey100 - Ey000, amy_typeI = Ey101 - Ey100,
            aly_typeI = Ey111 - Ey101, almy_typeI = aly_typeI + amy_typeI,
            ate_typeII = Ey111 - Ey000, ay_typeII = Ey111 - Ey011, amy_typeII = Ey011 - Ey010,
            aly_typeII = Ey010 - Ey000, almy_typeII = aly_typeII + amy_typeII) %>%
  summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "decomp", "measure")) %>%
  spread(measure, value) %>%
  mutate(estimand = factor(estimand, levels = rev(c("ate", "ay", "aly", "amy", "almy")),
                           labels = rev(c("Total Effect", "Direct Effect", "via Beliefs",
                                          "via Importance", "via Beliefs + Importance"))))

save.image("welfare_out.RData")
