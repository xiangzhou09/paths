rm(list = ls())

# devtools::document()
library(tidyverse)
library(paths)
library(BART)

if(sink.number()>0) invisible(replicate(sink.number(), sink()))

welfare <- readRDS("internal/welfare.rds")

# variable names
x <- c("gender1", "educ1", "polint1", "ideo1", "know1", "value1")
a <- "ttt"
m1 <- c("W1", "W2")
m2 <- c("M1","M2","M3","M4","M5")
y <- "Y"
m <- list(m1, m2)

# baseline model for overall treatment effect
lm_m0 <- lm(Y ~ ttt, data = welfare)

# outcome models with varying sets of mediators
Y <- welfare[[y]]
M1 <- as.matrix(welfare[, c(x, a, m1)])
M2 <- as.matrix(welfare[, c(x, a, m1, m2)])
wbart_m1 <- wbart(x.train = M1, y.train = Y)
wbart_m2 <- wbart(x.train = M2, y.train = Y)
wbart_ymodels <- list(lm_m0, wbart_m1, wbart_m2)

# a = "ttt"; y = "Y"; m = list(m1, m2);
# models = wbart_ymodels; ps_model = NULL;
# data = welfare; nboot = 4; conf_level = 0.95

# causal paths analysis
welfare_paths <- paths(a, y, m, models = wbart_ymodels, data = welfare,
                       parallel = "multicore", ncpus = 4, nboot = 5)

# sensitivity analysis
welfare_sens <- sens(welfare_paths, confounded = "M1", estimand = "direct",
                     gamma_values = seq(0, 2, 0.0025),
                     eta_values = seq(-0.5, 0.5, 0.002))

# sensitivity analysis plot
welfare_sens_plot <- ggplot(data = welfare_sens$adjusted, aes(eta_K, gamma_K)) +
  geom_raster(aes(fill = reversed), show.legend = FALSE) +
  scale_fill_manual(values = c("white", "grey70")) +
  geom_contour(aes(z = value), colour = "black") +
  metR::geom_text_contour(aes(z = value), size = 5) +
  ylab(expression(paste("Effect of U on Support for Welfare Reform (", gamma[2], ")"))) +
  xlab(expression(paste("Difference in the Prevalence of U between",
                        " Treated and Untreated Units (", eta[2], ")"))) +
  scale_x_continuous(position = "top") +
  theme_minimal(base_size = 14)

save.image("internal/welfare_paths.RData")
