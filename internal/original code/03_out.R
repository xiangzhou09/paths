
rm(list = ls())

library(haven)
library(dplyr)
library(pryr)
library(tidyr)
library(ggplot2)
library(BART)
library(survey)
library(twang)

# welfare reform

load("welfare_out.RData")

welfare_out <- welfare_decomp_imp %>%
  filter(! ((estimand == "Total Effect") & (decomp == "typeII")),
         !(estimand == "via Beliefs + Importance")) %>%
  mutate(decomp = factor(decomp, levels = c("typeI", "typeII"),
                         labels = c("Type I Decomposition", "Type II Decomposition")))

ggplot(welfare_out, aes(x = estimand, y = est)) +
  geom_pointrange(aes(shape = decomp, col = decomp, ymin = lower, ymax = upper),
                  position = position_dodge(width = -0.5), size = 1) +
  geom_vline(xintercept = 0, linetype = 2) +
  xlab("") +
  ylab("Estimates of Total and Path-Specific Effects") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("figure3_welfare.png", width = 8, height = 6)

# figure 6 annex

rm(list = ls())

load("annex_out.RData")

tatar_out <- bind_rows(mutate(tatar_decomp_imp, method = "Pure Imputation Estimator"),
                       mutate(tatar_decomp_hyb, method = "Imputation-based Weighting Estimator")) %>%
  filter(! ((estimand == "Total Effect") & (decomp == "typeII"))) %>%
  mutate(method = factor(method, levels = c("Pure Imputation Estimator", "Imputation-based Weighting Estimator"))) %>%
  mutate(estimand = factor(estimand, levels = c("Total Effect", "Direct Effect",
                                                "via G1 Identity", "via G2 Identity", "via G3 Identity"),
                           labels = c("Total\nEffect", "Direct\nEffect",
                                      "via\nG1 Identity", "via\nG2 Identity", "via\nG3 Identity"))) %>%
  mutate(decomp = factor(decomp, levels = c("typeI", "typeII"),
                         labels = c("Type I Decomposition", "Type II Decomposition")))

ggplot(tatar_out, aes(x = estimand, y = est)) +
  geom_pointrange(aes(shape = decomp, col = decomp, ymin = lower, ymax = upper),
                  position = position_dodge(width = 0.5), size = 1) +
  geom_vline(xintercept = 0, linetype = 2) +
  xlab("") +
  ylab("Estimates of Total and Path-Specific Effects") +
  # coord_flip() +
  facet_grid( ~ method) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.spacing.y = unit(-0.5, "lines"), legend.box = "vertical")

ggsave("figure6_annex.png", width = 9, height = 6)

