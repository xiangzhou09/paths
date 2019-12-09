
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

load("welfare_bart.RData")

welfare_decomp <- data.frame(out) %>%
  `names<-`(c("te", "ay", "amy", "aly", "almy")) %>%
  summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "measure")) %>%
  spread(measure, value) %>%
  mutate(decomp = "Type I Decomposition",
         estimand = factor(estimand, levels = rev(c("te", "ay", "amy", "aly", "almy")),
                           labels = rev(c("Total Effect", "Direct Effect",
                                          "via Importance only", "via Content", "via Content + Importance"))))

welfare_decomp2 <- data.frame(out2) %>%
  `names<-`(c("te", "ay2", "amy2", "aly2", "almy2")) %>%
  summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "measure")) %>%
  spread(measure, value) %>%
  mutate(decomp = "Type II Decomposition",
         estimand = factor(estimand, levels = rev(c("te", "ay2", "amy2", "aly2", "almy2")),
                           labels = rev(c("Total Effect", "Direct Effect",
                                          "via Importance only", "via Content", "via Content + Importance"))))

welfare_out <- bind_rows(welfare_decomp, welfare_decomp2) %>%
  filter(! ((estimand == "Total Effect") & (decomp == "Type II Decomposition")))

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

load("annex_bart_full.RData")

tatar_decomp <- data.frame(out) %>%
  `names<-`(c("te", "ay", "aly", "amy", "any")) %>%
  summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "measure")) %>%
  spread(measure, value) %>%
  mutate(decomp = "Type I Decomposition", method = "Imputation-based Weighting Estimator", 
         estimand = factor(estimand, levels = rev(c("te", "ay", "aly", "amy", "any")),
                           labels = rev(c("Total Effect", "Direct Effect",
                                          "via G1 Identity", "via G2 Identity", "via G3 Identity"))))

tatar_decomp2 <- data.frame(out2) %>%
  `names<-`(c("te", "ay2", "aly2", "amy2", "any2")) %>%
  summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "measure")) %>%
  spread(measure, value) %>%
  mutate(decomp = "Type II Decomposition", method = "Imputation-based Weighting Estimator", 
         estimand = factor(estimand, levels = rev(c("te", "ay2", "aly2", "amy2", "any2")),
                           labels = rev(c("Total Effect", "Direct Effect",
                                          "via G1 Identity", "via G2 Identity", "via G3 Identity")))) %>%
  filter(!(estimand == "Total Effect"))

tatar_decomp_pi <- data.frame(out_pi) %>%
  `names<-`(c("te", "ay", "aly", "amy", "any")) %>%
  summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "measure")) %>%
  spread(measure, value) %>%
  mutate(decomp = "Type I Decomposition", method = "Pure Imputation Estimator", 
         estimand = factor(estimand, levels = rev(c("te", "ay", "aly", "amy", "any")),
                           labels = rev(c("Total Effect", "Direct Effect",
                                          "via G1 Identity", "via G2 Identity", "via G3 Identity"))))

tatar_decomp2_pi <- data.frame(out2_pi) %>%
  `names<-`(c("te", "ay2", "aly2", "amy2", "any2")) %>%
  summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "measure")) %>%
  spread(measure, value) %>%
  mutate(decomp = "Type II Decomposition", method = "Pure Imputation Estimator", 
         estimand = factor(estimand, levels = rev(c("te", "ay2", "aly2", "amy2", "any2")),
                           labels = rev(c("Total Effect", "Direct Effect",
                                          "via G1 Identity", "via G2 Identity", "via G3 Identity")))) %>%
  filter(!(estimand == "Total Effect"))

tatar_out <- bind_rows(tatar_decomp, tatar_decomp2, tatar_decomp_pi, tatar_decomp2_pi) %>%
  mutate(method = factor(method, levels = c("Pure Imputation Estimator", "Imputation-based Weighting Estimator"))) %>%
  mutate(estimand = factor(estimand, levels = c("Total Effect", "Direct Effect",
                                                "via G1 Identity", "via G2 Identity", "via G3 Identity"),
                           labels = c("Total\nEffect", "Direct\nEffect",
                                      "via\nG1 Identity", "via\nG2 Identity", "via\nG3 Identity")))

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


# # campaign finance
# 
# load("cfinance_bart.RData")
# 
# cfinance_decomp <- data.frame(out) %>%
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
# cfinance_decomp2 <- data.frame(out2) %>%
#   `names<-`(c("te", "ay2", "amy2", "aly2", "almy2")) %>%
#   summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
#   gather(measure, value) %>%
#   separate(measure, c("estimand", "measure")) %>%
#   spread(measure, value) %>%
#   mutate(decomp = "Type II Decomposition",
#          estimand = factor(estimand, levels = rev(c("te", "ay2", "amy2", "aly2", "almy2")),
#                            labels = rev(c("Total Effect", "Direct Effect",
#                                           "via Importance only", "via Content", "via Content + Importance"))))
# 
# cfinance_out <- bind_rows(cfinance_decomp, cfinance_decomp2) %>%
#   filter(! ((estimand == "Total Effect") & (decomp == "Type II Decomposition")))
# 
# ggplot(cfinance_out, aes(x = estimand, y = est)) +
#   geom_pointrange(aes(shape = decomp, col = decomp, ymin = lower, ymax = upper),
#                   position = position_dodge(width = -0.5), size = 1) +
#   geom_vline(xintercept = 0, linetype = 2) +
#   xlab("") +
#   ylab("Estimates of Total and Path-Specific Effects") +
#   coord_flip() +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "bottom", legend.title = element_blank())
# 
# ggsave("figure3_cfinance.png", width = 8, height = 6)
# 
# # peace
# 
# load("peace_bart_full.RData")
# 
# peace_decomp <- data.frame(out) %>%
#   `names<-`(c("te", "ay", "amy", "aly", "almy")) %>%
#   summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
#   gather(measure, value) %>%
#   separate(measure, c("estimand", "measure")) %>%
#   spread(measure, value) %>%
#   mutate(decomp = "Type I Decomposition",
#          estimand = factor(estimand, levels = rev(c("te", "ay", "amy", "aly", "almy")),
#                            labels = rev(c("Total Effect", "Direct Effect",
#                                           "via Morality", "via Costs and Benefits", "via Costs and Benefits + Morality"))))
# 
# peace_decomp2 <- data.frame(out2) %>%
#   `names<-`(c("te", "ay2", "amy2", "aly2", "almy2")) %>%
#   summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
#   gather(measure, value) %>%
#   separate(measure, c("estimand", "measure")) %>%
#   spread(measure, value) %>%
#   mutate(decomp = "Type II Decomposition",
#          estimand = factor(estimand, levels = rev(c("te", "ay2", "amy2", "aly2", "almy2")),
#                            labels = rev(c("Total Effect", "Direct Effect",
#                                           "via Morality", "via Costs and Benefits", "via Costs and Benefits + Morality"))))
# 
# peace_out <- bind_rows(peace_decomp, peace_decomp2) %>%
#   filter(! ((estimand == "Total Effect") & (decomp == "Type II Decomposition")))
# 
# ggplot(peace_out, aes(x = estimand, y = est)) +
#   geom_pointrange(aes(shape = decomp, col = decomp, ymin = lower, ymax = upper),
#                   position = position_dodge(width = -0.5), size = 1) +
#   geom_vline(xintercept = 0, linetype = 2) +
#   xlab("") +
#   ylab("Estimates of Total and Path-Specific Effects") +
#   coord_flip() +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "bottom", legend.title = element_blank())
# 
# ggsave("figure3_peace.png", width = 8, height = 6)

# figure 6 chechens

# rm(list = ls())
# 
# load("chechens_bart_full.RData")
# 
# tatar_decomp <- data.frame(out) %>%
#   `names<-`(c("te", "ay", "aly", "amy", "any")) %>%
#   summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
#   gather(measure, value) %>%
#   separate(measure, c("estimand", "measure")) %>%
#   spread(measure, value) %>%
#   mutate(decomp = "Type I Decomposition", method = "Imputation-based Weighting Estimator", 
#          estimand = factor(estimand, levels = rev(c("te", "ay", "aly", "amy", "any")),
#                            labels = rev(c("Total Effect", "Direct Effect",
#                                           "via G1 Identity", "via G2 Identity", "via G3 Identity"))))
# 
# tatar_decomp2 <- data.frame(out2) %>%
#   `names<-`(c("te", "ay2", "aly2", "amy2", "any2")) %>%
#   summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
#   gather(measure, value) %>%
#   separate(measure, c("estimand", "measure")) %>%
#   spread(measure, value) %>%
#   mutate(decomp = "Type II Decomposition", method = "Imputation-based Weighting Estimator", 
#          estimand = factor(estimand, levels = rev(c("te", "ay2", "aly2", "amy2", "any2")),
#                            labels = rev(c("Total Effect", "Direct Effect",
#                                           "via G1 Identity", "via G2 Identity", "via G3 Identity")))) %>%
#   filter(!(estimand == "Total Effect"))
# 
# tatar_decomp_pi <- data.frame(out_pi) %>%
#   `names<-`(c("te", "ay", "aly", "amy", "any")) %>%
#   summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
#   gather(measure, value) %>%
#   separate(measure, c("estimand", "measure")) %>%
#   spread(measure, value) %>%
#   mutate(decomp = "Type I Decomposition", method = "Pure Imputation Estimator", 
#          estimand = factor(estimand, levels = rev(c("te", "ay", "aly", "amy", "any")),
#                            labels = rev(c("Total Effect", "Direct Effect",
#                                           "via G1 Identity", "via G2 Identity", "via G3 Identity"))))
# 
# tatar_decomp2_pi <- data.frame(out2_pi) %>%
#   `names<-`(c("te", "ay2", "aly2", "amy2", "any2")) %>%
#   summarise_all(list(est = ~mean, lower = ~lower, upper = ~upper)) %>%
#   gather(measure, value) %>%
#   separate(measure, c("estimand", "measure")) %>%
#   spread(measure, value) %>%
#   mutate(decomp = "Type II Decomposition", method = "Pure Imputation Estimator", 
#          estimand = factor(estimand, levels = rev(c("te", "ay2", "aly2", "amy2", "any2")),
#                            labels = rev(c("Total Effect", "Direct Effect",
#                                           "via G1 Identity", "via G2 Identity", "via G3 Identity")))) %>%
#   filter(!(estimand == "Total Effect"))
# 
# tatar_out <- bind_rows(tatar_decomp, tatar_decomp2, tatar_decomp_pi, tatar_decomp2_pi) %>%
#   mutate(method = factor(method, levels = c("Pure Imputation Estimator", "Imputation-based Weighting Estimator"))) %>%
#   mutate(estimand = factor(estimand, levels = c("Total Effect", "Direct Effect",
#                                                 "via G1 Identity", "via G2 Identity", "via G3 Identity"),
#                            labels = c("Total\nEffect", "Direct\nEffect",
#                                       "via\nG1 Identity", "via\nG2 Identity", "via\nG3 Identity")))
# 
# ggplot(tatar_out, aes(x = estimand, y = est)) +
#   geom_pointrange(aes(shape = decomp, col = decomp, ymin = lower, ymax = upper),
#                   position = position_dodge(width = 0.5), size = 1) +
#   geom_vline(xintercept = 0, linetype = 2) +
#   xlab("") +
#   ylab("Estimates of Total and Path-Specific Effects") +
#   # coord_flip() +
#   facet_grid( ~ method) +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "bottom", legend.title = element_blank(),
#         legend.spacing.y = unit(-0.5, "lines"), legend.box = "vertical")
# 
# ggsave("figure6_chechens.png", width = 9, height = 6)

