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
# tatar_g1 <- Lupu_Peisakhin %>% filter(generation == 1) %>%
#   mutate(region2 = (region_origin==2), region3 = (region_origin==3), region4 = (region_origin==4),
#          dest2 = (destination==2), dest3 = (destination==3),
#          trust_g1 = trustCT - trustRU, victim_g1 = vicrussia, fear_g1 = fear) %>%
#   dplyr::select(family, kulak:otherprop_pre, violence,  trust_g1:fear_g1)
#
# summary(tatar_g1)
#
# tatar_g2 <- Lupu_Peisakhin %>% filter(generation == 2) %>%
#   mutate(trust_g2 = trustCT - trustRU, victim_g2 = vicrussia, fear_g2 = fear) %>%
#   dplyr::select(family, trust_g2:fear_g2) %>%
#   group_by(family) %>%
#   summarise(trust_g2 = mean(trust_g2, na.rm = TRUE),
#             victim_g2 = mean(victim_g2, na.rm = TRUE),
#             fear_g2 = mean(fear_g2, na.rm = TRUE))
#
# tatar_g3 <- Lupu_Peisakhin %>% filter(generation == 3) %>%
#   mutate(trust_g3 = trustCT - trustRU, victim_g3 = vicrussia, fear_g3 = fear) %>%
#   dplyr::select(family, trust_g3:fear_g3, chechens, annex)
#
# dim(tatar_g1)
# dim(tatar_g2)
# dim(tatar_g3)
#
# tatar <- tatar_g3 %>%
#   left_join(tatar_g1, by = "family") %>%
#   left_join(tatar_g2, by = "family") %>%
#   mutate_at(vars(trust_g3:fear_g3, trust_g2:fear_g2, trust_g1:fear_g1), nan2na) %>%
#   # mutate_at(vars(trust_g3:fear_g3, trust_g2:fear_g2, trust_g1:fear_g1), funs(na = as.numeric(is.na(.)))) %>%
#   # mutate_at(vars(trust_g3:fear_g3, trust_g2:fear_g2, trust_g1:fear_g1), impute_mu) %>%
#   drop_na(violence,
#           annex
#           # , kulak, prosoviet_pre, religiosity_pre,
#           # trust_g3:fear_g3, trust_g2:fear_g2, trust_g1:fear_g1
#   ) %>%
#   mutate(violence = as.numeric(violence >= 1))
#
# names(tatar)
# dim(tatar)
#
# use_data(tatar, overwrite = TRUE)
