rm(list = ls())

library(tidyverse)
library(paths)
library(BART)

load("internal/Imai-Yamamoto/PA-ImaiYamamoto.RData")

welfare <- Slothuus %>% drop_na(Y) %>%
  mutate(gender1 = as.numeric(factor(gender, levels = c(0, 1)))-1,
         educ1 = ifelse(educmiss==TRUE, NA, educ),
         polint1 = ifelse(polintmiss==TRUE, NA, polint),
         ideo1 = ifelse(ideomiss==TRUE, NA, ideo),
         value1 = as.numeric(factor(value, levels = c(0, "extreme")))-1,
         know1 = as.numeric(factor(know, levels = c("low", "mid", "high"))))

##########################################

x <- c("gender1", "educ1", "polint1", "ideo1", "know1", "value1")
a <- "ttt"
m1 <- c("W1", "W2")
m2 <- c("M1","M2","M3","M4","M5")
y <- "Y"

# constructing design matrices for fitting wbart models
Y <- welfare[[y]]

M0 <- as.matrix(welfare[, c(x, a)])
M1 <- as.matrix(welfare[, c(x, a, m1)])
M2 <- as.matrix(welfare[, c(x, a, m1, m2)])

wbart_m0 <- wbart(x.train = M0, y.train = Y)
wbart_m1 <- wbart(x.train = M1, y.train = Y)
wbart_m2 <- wbart(x.train = M2, y.train = Y)
wbart_ymodels <- list(wbart_m0, wbart_m1, wbart_m2)

# a = "ttt"; y = "Y"; models = wbart_ymodels; ps_model = NULL; data = welfare; nboot = 5; conf_level = 0.95

welfare_paths <- paths(a, y, wbart_ymodels, data = welfare,
                       parallel = "multicore", ncpus = 4, nboot = 500)

save.image("internal/welfare_paths.RData")

