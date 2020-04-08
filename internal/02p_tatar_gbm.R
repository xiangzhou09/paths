rm(list = ls())

# devtools::document()
library(tidyverse)
library(paths)
library(gbm)

if(sink.number()>0) invisible(replicate(sink.number(), sink()))

tatar <- readRDS("internal/tatar.rds")
tatar <- as.data.frame(lapply(tatar, unclass))

# variable names
x <- c("kulak", "prosoviet_pre", "religiosity_pre", "house_pre", "land_pre",
       "orchard_pre", "animals_pre", "carriage_pre", "otherprop_pre")
a <- "violence"
y <- "annex"
m1 <- c("trust_g1", "victim_g1", "fear_g1")
m2 <- c("trust_g2", "victim_g2", "fear_g2")
m3 <- c("trust_g3", "victim_g3", "fear_g3")
m <- list(m1, m2, m3)

formula_m0 <- annex ~ kulak + prosoviet_pre + religiosity_pre + house_pre +
  land_pre + orchard_pre + animals_pre + carriage_pre + otherprop_pre + violence
formula_m1 <- update(formula_m0,    ~ . + trust_g1 + victim_g1 + fear_g1)
formula_m2 <- update(formula_m1,    ~ . + trust_g2 + victim_g2 + fear_g2)
formula_m3 <- update(formula_m2,    ~ . + trust_g3 + victim_g3 + fear_g3)
formula_ps <- violence ~ kulak + prosoviet_pre + religiosity_pre + house_pre +
  land_pre + orchard_pre + animals_pre + carriage_pre + otherprop_pre

gbm_m0 <- gbm(formula_m0, data = tatar, distribution = "bernoulli", interaction.depth = 3)
gbm_m1 <- gbm(formula_m1, data = tatar, distribution = "bernoulli", interaction.depth = 3)
gbm_m2 <- gbm(formula_m2, data = tatar, distribution = "bernoulli", interaction.depth = 3)
gbm_m3 <- gbm(formula_m3, data = tatar, distribution = "bernoulli", interaction.depth = 3)

gbm_ps <- twang::ps(formula_ps, data = as.data.frame(tatar),
                    n.trees = 2000, stop.method = "es.mean")
gbm_ymodels <- list(gbm_m0, gbm_m1, gbm_m2, gbm_m3)

# causal paths analysis
tatar_paths_gbm <- paths(a, y, m, models = gbm_ymodels, ps_model = gbm_ps,
                     data = tatar, parallel = "multicore", ncpus = 4, nboot = 500)

# sensitivity analysis
tatar_sens <- sens(tatar_paths_gbm, confounded = "M1", estimand = "via M1",
                   gamma_values = - seq(0, 0.5, 0.002),
                   eta_values = seq(-0.5, 0.5, 0.002))

# sensitivity analysis
tatar_sens_plot <- ggplot(data = tatar_sens$adjusted, aes(eta_k, gamma_k)) +
  geom_raster(aes(fill = reversed), show.legend = FALSE) +
  scale_fill_manual(values = c("white", "grey70")) +
  geom_contour(aes(z = value), colour = "black") +
  metR::geom_text_contour(aes(z = value), size = 5) +
  ylab(expression(paste("Effect of U on Regime Support (", gamma[1], ")"))) +
  xlab(expression(paste("Difference in the Prevalence of U between ",
                        "Treated and Untreated units (", eta[1], ")"))) +
  scale_x_continuous(position = "top") +
  theme_minimal(base_size = 14)

save.image("internal/tatar_paths_gbm.RData")
