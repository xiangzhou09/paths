rm(list = ls())

# devtools::document()
library(paths)
library(BART)
library(gbm)

set.seed(01238)

tatar <- readRDS("internal/tatar.rds")

# variable names
x <- c("kulak", "prosoviet_pre", "religiosity_pre", "land_pre",
       "orchard_pre", "animals_pre", "carriage_pre", "otherprop_pre")
a <- "violence"
y <- "annex"
m1 <- c("trust_g1", "victim_g1", "fear_g1")
m2 <- c("trust_g2", "victim_g2", "fear_g2")
m3 <- c("trust_g3", "victim_g3", "fear_g3")
m <- list(m1, m2, m3)

# formulas
formula_m0 <- as.formula(paste0(y, "~", paste0(c(x, a), collapse = "+")))
formula_m1 <- as.formula(paste0(y, "~", paste0(c(x, a, m1), collapse = "+")))
formula_m2 <- as.formula(paste0(y, "~", paste0(c(x, a, m1, m2), collapse = "+")))
formula_m3 <- as.formula(paste0(y, "~", paste0(c(x, a, m1, m2, m3), collapse = "+")))
formula_ps <- as.formula(paste0(a, "~", paste0(x, collapse = "+")))

# GLM outcome models
glm_m0 <- glm(formula_m0, family = binomial("logit"), data = tatar)
glm_m1 <- glm(formula_m1, family = binomial("logit"), data = tatar)
glm_m2 <- glm(formula_m2, family = binomial("logit"), data = tatar)
glm_m3 <- glm(formula_m3, family = binomial("logit"), data = tatar)
glm_ymodels <- list(glm_m0, glm_m1, glm_m2, glm_m3)

# gbm outcome models
gbm_m0 <- gbm(formula_m0, data = tatar, distribution = "bernoulli", interaction.depth = 3)
gbm_m1 <- gbm(formula_m1, data = tatar, distribution = "bernoulli", interaction.depth = 3)
gbm_m2 <- gbm(formula_m2, data = tatar, distribution = "bernoulli", interaction.depth = 3)
gbm_m3 <- gbm(formula_m3, data = tatar, distribution = "bernoulli", interaction.depth = 3)
gbm_ymodels <- list(gbm_m0, gbm_m1, gbm_m2, gbm_m3)

# bart outcome models
Y <- tatar[[y]]
M0 <- as.matrix(tatar[, c(x, a)])
M1 <- as.matrix(tatar[, c(x, a, m1)])
M2 <- as.matrix(tatar[, c(x, a, m1, m2)])
M3 <- as.matrix(tatar[, c(x, a, m1, m2, m3)])
pbart_m0 <- pbart(x.train = M0, y.train = Y)
pbart_m1 <- pbart(x.train = M1, y.train = Y)
pbart_m2 <- pbart(x.train = M2, y.train = Y)
pbart_m3 <- pbart(x.train = M3, y.train = Y)
bart_ymodels <- list(pbart_m0, pbart_m1, pbart_m2, pbart_m3)

# propensity score models for treatment
glm_psmodel <- glm(formula_ps, family = binomial("logit"),  data = tatar)
gbm_psmodel <- gbm(formula_ps, data = tatar, distribution = "bernoulli", interaction.depth = 3)
ps_psmodel <- twang::ps(formula_ps, data = tatar, n.trees = 1000,
                        stop.method = "es.mean")

# causal paths analysis

tatar_glm <- paths(a, y, m, models = glm_ymodels, ps_model = glm_psmodel,
                   data = tatar, parallel = "multicore", ncpus = 4, nboot = 1000)
saveRDS(tatar_glm, file = "internal/tatar_glm.rds")

tatar_gbm <- paths(a, y, m, models = gbm_ymodels, ps_model = gbm_psmodel,
                   data = tatar, parallel = "multicore", ncpus = 4, nboot = 1000)
saveRDS(tatar_gbm, file = "internal/tatar_gbm.rds")

tatar_bart <- paths(a, y, m, models = bart_ymodels, ps_model = ps_psmodel,
                    data = tatar, parallel = "multicore", ncpus = 4, nboot = 1000)
saveRDS(tatar_bart, file = "internal/tatar_bart.rds")
