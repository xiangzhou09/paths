# rm(list = ls())
#
# library(haven)
# library(dplyr)
# library(pryr)
# library(tidyr)
# library(ggplot2)
# library(BART)
# library(survey)
# library(twang)
#
# # impute_mu <- function(x) {x[is.na(x)] <- mean(x, na.rm = TRUE); x}
#
# nan2na <- function(x) {x[is.nan(x)] <- NA; x}
#
# Lupu_Peisakhin <- read_dta("data-raw/Lupu-Peisakhin/3. Lupu_Peisakhin_Crimea_data.dta")
#
# summary(Lupu_Peisakhin)
#
# filter <- dplyr::filter
#
# tatar_g1 <- Lupu_Peisakhin %>% filter(generation == 1) %>%
#   mutate(region2 = (region_origin==2), region3 = (region_origin==3), region4 = (region_origin==4),
#          dest2 = (destination==2), dest3 = (destination==3),
#          trust_g1 = trustCT - trustRU, victim_g1 = vicrussia, fear_g1 = as.double(fear>0)) %>%
#   dplyr::select(family, kulak:otherprop_pre, violence,  trust_g1:fear_g1)
#
# summary(tatar_g1)
#
# tatar_g2 <- Lupu_Peisakhin %>% filter(generation == 2) %>%
#   mutate(trust_g2 = trustCT - trustRU, victim_g2 = vicrussia, fear_g2 = as.double(fear>0)) %>%
#   dplyr::select(family, trust_g2:fear_g2) %>%
#   group_by(family) %>%
#   summarise(trust_g2 = mean(trust_g2, na.rm = TRUE),
#             victim_g2 = mean(victim_g2, na.rm = TRUE),
#             fear_g2 = mean(fear_g2, na.rm = TRUE))
#
# tatar_g3 <- Lupu_Peisakhin %>% filter(generation == 3) %>%
#   mutate(trust_g3 = trustCT - trustRU, victim_g3 = vicrussia, fear_g3 = as.double(fear>0)) %>%
#   dplyr::select(family, trust_g3:fear_g3, chechens, annex)
#
# x <- c("kulak", "prosoviet_pre", "religiosity_pre", "land_pre",
#        "orchard_pre", "animals_pre", "carriage_pre", "otherprop_pre")
# a <- "violence"
# y <- "annex"
# m1 <- c("trust_g1", "victim_g1", "fear_g1")
# m2 <- c("trust_g2", "victim_g2", "fear_g2")
# m3 <- c("trust_g3", "victim_g3", "fear_g3")
#
# tatar <- tatar_g3 %>%
#   left_join(tatar_g1, by = "family") %>%
#   left_join(tatar_g2, by = "family") %>%
#   mutate_at(vars(trust_g3:fear_g3, trust_g2:fear_g2, trust_g1:fear_g1), nan2na) %>%
#   # mutate_at(vars(trust_g3:fear_g3, trust_g2:fear_g2, trust_g1:fear_g1), funs(na = as.numeric(is.na(.)))) %>%
#   # mutate_at(vars(trust_g3:fear_g3, trust_g2:fear_g2, trust_g1:fear_g1), impute_mu) %>%
#   drop_na(violence,
#           annex,
#           kulak, prosoviet_pre, religiosity_pre,
#           trust_g3:fear_g3, trust_g2:fear_g2, trust_g1:fear_g1,
#   ) %>%
#   mutate(violence = as.numeric(violence >= 1)) %>%
#   dplyr::select(c(x, a, m1, m2, m3, y)) %>%
#   lapply(unclass) %>%
#   as.data.frame()
#
# names(tatar)
# dim(tatar)
# class(tatar)
#
# use_data(tatar, overwrite = TRUE)
#
# load("data-raw/Imai-Yamamoto/PA-ImaiYamamoto.RData")
#
# x <- c("gender1", "educ1", "polint1", "ideo1", "know1", "value1")
# a <- "ttt"
# m1 <- c("W1", "W2")
# m2 <- c("M1","M2","M3","M4","M5")
# y <- "Y"
#
# welfare <- Slothuus %>% filter(!is.na(Y)) %>%
#   mutate(gender1 = as.numeric(factor(gender, levels = c(0, 1)))-1,
#          educ1 = ifelse(educmiss==TRUE, NA, educ),
#          polint1 = ifelse(polintmiss==TRUE, NA, polint),
#          ideo1 = ifelse(ideomiss==TRUE, NA, ideo),
#          value1 = as.numeric(factor(value, levels = c(0, "extreme")))-1,
#          know1 = as.numeric(factor(know, levels = c("low", "mid", "high")))) %>%
#   dplyr::select(c(x, a, m1, m2, y))
#
# usethis::use_data(welfare, overwrite = TRUE)
