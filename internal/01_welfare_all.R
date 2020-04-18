rm(list = ls())

# devtools::document()
library(paths)
library(BART)
library(gbm)

set.seed(01238)

welfare <- readRDS("internal/welfare.rds")

# variable names
x <- c("gender1", "educ1", "polint1", "ideo1", "know1", "value1")
a <- "ttt"
m1 <- c("W1", "W2")
m2 <- c("M1","M2","M3","M4","M5")
y <- "Y"
m <- list(m1, m2)

# formulas
form_m0 <- as.formula(paste0(y, "~", a))
form_m1 <- as.formula(paste0(y, "~", paste0(c(x, a, m1), collapse = "+")))
form_m2 <- as.formula(paste0(y, "~", paste0(c(x, a, m1, m2), collapse = "+")))

# baseline model for overall treatment effect
lm_m0 <- lm(form_m0, data = welfare)

# GLM outcome models
lm_m1 <- lm(form_m1, data = welfare)
lm_m2 <- lm(form_m2, data = welfare)
glm_ymodels <- list(lm_m0, lm_m1, lm_m2)

# GBM outcome models
gbm_m1 <- gbm(form_m1, data = welfare, distribution = "gaussian", interaction.depth = 3)
gbm_m2 <- gbm(form_m2, data = welfare, distribution = "gaussian", interaction.depth = 3)
gbm_ymodels <- list(lm_m0, gbm_m1, gbm_m2)

# BART outcome models
Y <- welfare[[y]]
M1 <- as.matrix(welfare[, c(x, a, m1)])
M2 <- as.matrix(welfare[, c(x, a, m1, m2)])
wbart_m1 <- wbart(x.train = M1, y.train = Y)
wbart_m2 <- wbart(x.train = M2, y.train = Y)
bart_ymodels <- list(lm_m0, wbart_m1, wbart_m2)

# causal paths analysis
welfare_glm <- paths(a, y, m, models = glm_ymodels, data = welfare,
                      parallel = "multicore", ncpus = 4, nboot = 1000)
saveRDS(welfare_glm, file = "internal/welfare_glm.rds")

welfare_gbm <- paths(a, y, m, models = gbm_ymodels, data = welfare,
                     parallel = "multicore", ncpus = 4, nboot = 1000)
saveRDS(welfare_gbm, file = "internal/welfare_gbm.rds")

welfare_bart <- paths(a, y, m, models = bart_ymodels, data = welfare,
                     parallel = "multicore", ncpus = 4, nboot = 1000)
saveRDS(welfare_bart, file = "internal/welfare_bart.rds")
