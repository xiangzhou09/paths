
rm(list = ls())

library(haven)
library(dplyr)
library(pryr)
library(tidyr)
library(ggplot2)
library(BART)
library(survey)
library(twang)

# welfare reform example

load("welfare_out.RData")

# welfare_decomp <- data.frame(out) %>%
#   `names<-`(c("te", "ay", "amy", "aly", "almy")) %>%
#   summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
#   gather(measure, value) %>%
#   separate(measure, c("estimand", "measure")) %>%
#   spread(measure, value) %>%
#   mutate(decomp = "Type I Decomposition",
#          estimand = factor(estimand, levels = rev(c("te", "ay", "amy", "aly", "almy")),
#                            labels = rev(c("Total Effect", "Direct Effect",
#                                           "via Importance only", "via Content", "via Content + Importance"))))
# 
# sd(welfare$Y)

via_importance <- welfare_decomp_imp[welfare_decomp_imp$estimand == "via Importance" &
                                       welfare_decomp_imp$decomp == "typeI", "est"]

direct_effect <- welfare_decomp_imp[welfare_decomp_imp$estimand == "Direct Effect" &
                                      welfare_decomp_imp$decomp == "typeI", "est"]

gamma <- seq(0, 2, 0.0025)
eta <- seq(-0.5, 0.5, 0.0025)

input <- expand.grid(gamma = gamma, eta = eta) %>%
  mutate(value = direct_effect - gamma * eta, reversed = factor(value<0))

ggplot(data = input, aes(eta, gamma)) +
  geom_raster(aes(fill = reversed), show.legend = FALSE) +
  geom_contour(aes(z = value), colour = "black") +
  metR::geom_text_contour(aes(z = value), size = 5) + 
  scale_fill_manual(values = c(NA, "grey70")) +
  # geom_vline(xintercept = 0, linetype = 1) +
  # geom_hline(yintercept = 0, linetype = 1) +
  ylab(expression(paste("Effect of U on Support for Welfare Reform (", gamma[2], ")"))) +
  xlab(expression(paste("Difference in the Prevalence of U between Treated and Untreated Units (", eta[2], ")"))) +
  # scale_x_continuous(position = "top") +
  # scale_y_continuous(position = "right") +
  theme_minimal(base_size = 14)

ggsave("figure4_welfare_sens.png", width = 9, height = 6)

# crimea example

load("annex_out.RData")

via_G1 <- tatar_decomp_imp[tatar_decomp_imp$estimand == "via G1 Identity" &
                             tatar_decomp_imp$decomp == "typeI", "est"]

gamma <- - seq(0, 0.5, 0.002)
eta <- seq(-0.5, 0.5, 0.002)

input <- expand.grid(gamma = gamma, eta = eta) %>%
  mutate(value = via_G1 + gamma * eta, reversed = factor(value>0))

ggplot(data = input, aes(eta, gamma)) +
  geom_raster(aes(fill = reversed), show.legend = FALSE) +
  geom_contour(aes(z = value), colour = "black") +
  metR::geom_text_contour(aes(z = value), size = 5) + 
  scale_fill_manual(values = c(NA, "grey70")) +
  # geom_vline(xintercept = 0, linetype = 1) +
  # geom_hline(yintercept = 0, linetype = 1) +
  ylab(expression(paste("Effect of U on Regime Support (", gamma[1], ")"))) +
  xlab(expression(paste("Difference in the Prevalence of U between Treated and Untreated units (", eta[1], ")"))) +
  scale_x_continuous(position = "top") +
  # scale_y_continuous(position = "right") +
  theme_minimal(base_size = 14)

ggsave("figure7_annex_sens.png", width = 9, height = 6)

  
