rm(list = ls())

# devtools::document()
library(tidyverse)
library(paths)
library(BART)

if(sink.number()>0) invisible(replicate(sink.number(), sink()))

tatar <- readRDS("internal/tatar.rds")

# variable names
x <- c("kulak", "prosoviet_pre", "religiosity_pre", "house_pre", "land_pre",
       "orchard_pre", "animals_pre", "carriage_pre", "otherprop_pre")
a <- "violence"
y <- "annex"
m1 <- c("trust_g1", "victim_g1", "fear_g1")
m2 <- c("trust_g2", "victim_g2", "fear_g2")
m3 <- c("trust_g3", "victim_g3", "fear_g3")
m <- list(m1, m2, m3)

# design matrices for outcome models
Y <- tatar[[y]]
M0 <- as.matrix(tatar[, c(x, a), drop = FALSE])
M1 <- as.matrix(tatar[, c(x, a, m1), drop = FALSE])
M2 <- as.matrix(tatar[, c(x, a, m1, m2), drop = FALSE])
M3 <- as.matrix(tatar[, c(x, a, m1, m2, m3), drop = FALSE])

# outcome models with varying sets of mediators
pbart_m0 <- pbart(x.train = M0, y.train = Y)
pbart_m1 <- pbart(x.train = M1, y.train = Y)
pbart_m2 <- pbart(x.train = M2, y.train = Y)
pbart_m3 <- pbart(x.train = M3, y.train = Y)
pbart_ymodels <- list(pbart_m0, pbart_m1, pbart_m2, pbart_m3)

# models = wbart_ymodels; ps_model = NULL;
# data = tatar; nboot = 4; conf_level = 0.95

# causal paths analysis
tatar_paths <- paths(a, y, m, pbart_ymodels, ps_model = NULL, data = tatar,
                     parallel = "multicore", ncpus = 4, nboot = 5)

# sensitivity analysis
tatar_sens <- sens(tatar_paths, confounded = "M1", estimand = "via M1",
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

save.image("internal/tatar_paths.RData")
