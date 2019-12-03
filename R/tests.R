#### Test data: results from the paper

### EXAMPLE 1 ###

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(BART)
library(boot)

expit <- function(x) exp(x)/(1+exp(x))

CCES10_public <- read_dta("data/CCES10_public.dta")

summary(CCES10_public$strikeo)

peace <- CCES10_public %>%
  dplyr::select(caseid, post, democ, strike, strikef, strikeo, threatc, cost, successc, immoral,
                ally, trade, h1, i1, p1, e1, r1, male, white, age, ed4) %>%
  mutate(ipw = 1)

dat <- peace

x <- c("ally", "trade", "h1", "i1", "p1", "e1", "r1", "male", "white", "age", "ed4")
a <- "democ"
m1 <- c("threatc", "cost", "successc")
m2 <- "immoral"
y <- "strikeo"

formula_y <- as.formula(paste(y, " ~ ", paste(c(x, a, m1, m2), collapse= "+")))
formula_m2 <- as.formula(paste(y, " ~ ", paste(c(x, a, m1), collapse= "+")))
formula_m1 <- as.formula(paste(y, " ~ ", paste(c(x, a), collapse= "+")))

formulas <- list(formula_y, formula_m2, formula_m1)
models <- c("lm", "pbart", "glm")
models_args <- list(NULL, NULL, list(family = binomial(link = "logit")))

treat <- "democ"
sims <- R <- 20

formula_a <- as.formula(paste(a, " ~ ", paste(c(x, m1, m2), collapse = "+")))
model_a <- "glm"
model_a_arg <- list(family = binomial(link = "logit"))

#######

## Testing model_fit
model_fit(peace, list(formula_a), list(model_a), list(model_a_arg))

## Testing path_fun
paths_fun(data = peace,
         index = 1:nrow(peace),
         formulas = list(formula_y, formula_m2, formula_m1),
         models = c("pbart", "pbart", "pbart"),
         models_args = list(NULL,
                            NULL,
                            NULL),
         treat = "democ",
         outcome = "strikeo",
         conditional = FALSE,
         ps = TRUE,
         ps_formula = list(formula_a),
         ps_model = "glm",
         ps_model_args = list(list(binomial(link = "logit"))))

## Using formula
path_out <- paths(formulas = list(formula_y, formula_m2),
                  models = c("lm", "lm"),
                  models_args = list(list(x = TRUE, y = TRUE),
                                     list(model = TRUE, x = TRUE, y = TRUE)),
                  sims = 1000,
                  treat = "democ",
                  outcome = "strikeo",
                  data = peace)

path_out <- paths(formulas = list(formula_y, formula_m2, formula_m1),
                  models = c("lm", "lm", "lm"),
                  models_args = list(list(x = TRUE, y = TRUE),
                                     list(model = TRUE, x = TRUE, y = TRUE),
                                     NULL),
                  sims = 5,
                  treat = "democ",
                  outcome = "strikeo",
                  data = peace)

path_out <- paths(formulas = list(formula_y, formula_m2, formula_m1),
                  models = c("pbart", "pbart", "pbart"),
                  models_args = list(list(sparse = TRUE),
                                     list(theta = 0),
                                     NULL),
                  sims = 5,
                  treat = "democ",
                  outcome = "strikeo",
                  data = peace,
                  parallel = "multicore",
                  ncpus = 5)

path_out <- paths(formulas = list(formula_y, formula_m2, formula_m1),
                  models = c("lm", "lm", "lm"),
                  sims = 5,
                  treat = "democ",
                  outcome = "strikeo",
                  data = peace,
                  parallel = "multicore",
                  ncpus = 5,
                  ps = TRUE,
                  ps_formula = formula_a,
                  ps_model = "lm",
                  ps_model_args = list(NULL))

path_out <- paths(formulas = list(formula_y, formula_m2, formula_m1),
                  models = c("pbart", "pbart", "pbart"),
                  models_args = list(NULL,
                                     NULL,
                                     NULL),
                  conditional = TRUE,
                  sims = 500,
                  treat = "democ",
                  outcome = "strikeo",
                  data = peace,
                  parallel = "multicore",
                  ncpus = 25)

path_out <- paths(formulas = list(formula_y, formula_m2, formula_m1),
                  models = c("pbart", "pbart", "pbart"),
                  models_args = list(NULL,
                                     NULL,
                                     NULL),
                  conditional = TRUE,
                  sims = 500,
                  treat = "democ",
                  outcome = "strikeo",
                  data = peace,
                  parallel = "multicore",
                  ncpus = 25,
                  ps = TRUE,
                  ps_formula = formula_a)

# Model as in the paper
path_fun_out_peace <- paths_fun(data = peace,
                                index = 1:nrow(peace),
                                formulas = list(formula_y, formula_m2, formula_m1),
                                models = c("pbart", "pbart", "pbart"),
                                models_args = list(NULL,
                                                   NULL,
                                                   NULL),
                                conditional = FALSE,
                                treat = "democ",
                                outcome = "strikeo",
                                ps = FALSE)

path_out_peace <- paths(formulas = list(formula_y, formula_m2, formula_m1),
                  models = c("pbart", "pbart", "pbart"),
                  models_args = list(NULL,
                                     NULL,
                                     NULL),
                  conditional = FALSE,
                  sims = 500,
                  treat = "democ",
                  outcome = "strikeo",
                  data = peace,
                  parallel = "multicore",
                  ncpus = 25)

## Load output generated from paper code to compare
load("data/peace_bart.RData")
load("data/peace_bart_draw.RData")
summary(path_out_peace)

# output matches example
peace_decomp_draw
peace_decomp2_draw

# old example without random draw for imputation has smaller error
peace_decomp
peace_decomp2

plot(path_out_peace)
plot(path_out_peace, c("Morality", "Cost-Benefit Analysis"))


### EXAMPLE 2 ###

library(haven)
library(dplyr)
library(pryr)
library(tidyr)
library(ggplot2)
library(BART)
library(survey)
library(twang)

Lupu_Peisakhin <- read_dta("data/3. Lupu_Peisakhin_Crimea_data.dta")

summary(Lupu_Peisakhin)

tatar_g1 <- Lupu_Peisakhin %>% filter(generation == 1) %>%
  mutate(region2 = (region_origin==2), region3 = (region_origin==3), region4 = (region_origin==4),
         dest2 = (destination==2), dest3 = (destination==3),
         trust_g1 = trustCT - trustRU, victim_g1 = vicrussia, fear_g1 = fear) %>%
  dplyr::select(family, kulak:otherprop_pre, violence,  trust_g1:fear_g1)

summary(tatar_g1)

tatar_g2 <- Lupu_Peisakhin %>% filter(generation == 2) %>%
  mutate(trust_g2 = trustCT - trustRU, victim_g2 = vicrussia, fear_g2 = fear) %>%
  dplyr::select(family, trust_g2:fear_g2) %>%
  group_by(family) %>%
  summarise(trust_g2 = mean(trust_g2, na.rm = TRUE),
            victim_g2 = mean(victim_g2, na.rm = TRUE),
            fear_g2 = mean(fear_g2, na.rm = TRUE))

tatar_g3 <- Lupu_Peisakhin %>% filter(generation == 3) %>%
  mutate(trust_g3 = trustCT - trustRU, victim_g3 = vicrussia, fear_g3 = fear) %>%
  dplyr::select(family, trust_g3:fear_g3, chechens, annex)

dim(tatar_g1)
dim(tatar_g2)
dim(tatar_g3)

tatar <- tatar_g3 %>%
  left_join(tatar_g1, by = "family") %>%
  left_join(tatar_g2, by = "family") %>%
  # mutate_at(vars(trust_g3:fear_g3, trust_g2:fear_g2, trust_g1:fear_g1), funs(na = as.numeric(is.na(.)))) %>%
  # mutate_at(vars(trust_g3:fear_g3, trust_g2:fear_g2, trust_g1:fear_g1), impute_mu) %>%
  drop_na(violence,
          annex,
          kulak, prosoviet_pre, religiosity_pre,
          trust_g3:fear_g3, trust_g2:fear_g2, trust_g1:fear_g1
  ) %>%
  mutate(violence = as.numeric(violence >= 1))

x <- c("kulak", "prosoviet_pre", "religiosity_pre",
       "house_pre", "land_pre", "orchard_pre",
       "animals_pre", "carriage_pre", "otherprop_pre")
a <- "violence"
l <- c("trust_g1", "victim_g1", "fear_g1")
m <- c("trust_g2", "victim_g2", "fear_g2")
n <- c("trust_g3", "victim_g3", "fear_g3")
y <- "annex"

# outcome models

formula_y <- as.formula(paste(y, " ~ ", paste(c(x, a, l, m, n), collapse= "+")))
formula_m3 <- as.formula(paste(y, " ~ ", paste(c(x, a, l, m), collapse= "+")))
formula_m2 <- as.formula(paste(y, " ~ ", paste(c(x, a, l), collapse= "+")))
formula_m1 <- as.formula(paste(y, " ~ ", paste(c(x, a), collapse= "+")))
formula_a <- as.formula(paste(a, " ~ ", paste(c(x), collapse= "+")))

#######

## Test model_fit
data <- tatar[which(tatar$violence == 0),]

# EXPECT ERROR (TO PREVENT CRASH)
model_a_arg <- list(NULL)
model_yhat <- model_fit(data = data,
                        formulas = list(formula_a),
                        models = "pbart",
                        models_args = list(model_a_arg))

# EXPECT ERROR (REDUNDANT BUT JUST IN CASE)
model_a_arg <- list(NULL)
model_yhat <- model_fit(data = data,
                        formulas = list(formula_a),
                        models = "lm",
                        models_args = list(model_a_arg))

# EXPECT NO ERROR
model_a_arg <- NULL
model_yhat <- model_fit(data = data,
                        formulas = list(formula_a),
                        models = "pbart",
                        models_args = list(model_a_arg))

# EXPECT NO ERROR
model_a_arg <- list(theta = 0)
model_yhat <- model_fit(data = data,
                        formulas = list(formula_a),
                        models = "pbart",
                        models_args = list(model_a_arg))

# EXPECT ERROR
model_a_arg <- list(theta = 0, omega = 1)
model_yhat <- model_fit(data = data,
                        formulas = list(formula_a),
                        models = "pbart",
                        models_args = model_a_arg)

# EXPECT ERROR
model_a_arg <- list(sparse = FALSE, omega = 1)
model_yhat <- model_fit(data = data,
                        formulas = list(formula_a),
                        models = "pbart",
                        models_args = model_a_arg)


## Testing path_fun
#index <- sample(nrow(tatar), replace = TRUE)
index <- 1:nrow(tatar)
paths_fun(data = tatar,
          index = index,
          formulas = list(formula_y, formula_m3, formula_m2, formula_m1),
          models = c("pbart", "pbart", "pbart", "pbart"),
          models_args = list(NULL,
                             NULL,
                             NULL,
                             NULL),
          treat = "violence",
          outcome = "annex",
          conditional = FALSE,
          ps = TRUE,
          ps_formula = list(formula_a),
          ps_model = "glm",
          ps_model_args = list(list(binomial(link = "logit"))))

paths_fun(data = tatar,
          index = index,
          formulas = list(formula_y, formula_m3, formula_m2, formula_m1),
          models = c("pbart", "pbart", "pbart", "pbart"),
          models_args = list(NULL,
                             NULL,
                             NULL,
                             NULL),
          treat = "violence",
          outcome = "annex",
          conditional = TRUE,
          ps = FALSE)


## Testing boot

boot_out <- boot::boot(data = tatar,
                       statistic = paths_fun,
                       R = 20,
                       sim = "ordinary",
                       formulas = list(formula_y, formula_m3, formula_m2, formula_m1),
                       models = c("pbart", "lbart", "lm", "glm"),
                       models_args = list(NULL,
                                          NULL,
                                          NULL,
                                          list(binomial(link = "logit"))),
                       treat = "violence",
                       outcome = "annex",
                       conditional = TRUE,
                       ps = TRUE,
                       ps_formula = list(formula_a),
                       ps_model = "glm",
                       ps_model_args = list(list(binomial(link = "logit"))),
                       parallel = "multicore",
                       ncpus = 20)

## Model as in the paper << somehow still giving "invalid formula" error even though all the above is running smoothly
path_out_annex <- paths(formulas = list(formula_y, formula_m3, formula_m2, formula_m1),
                        models = c("pbart", "pbart", "pbart", "pbart"),
                        models_args = list(NULL,
                                           NULL,
                                           NULL,
                                           NULL),
                        conditional = TRUE,
                        ps = TRUE,
                        ps_formula = formula_a,
                        ps_model = "glm",
                        ps_model_args = list(family = binomial(link = "logit")),
                        sims = 500,
                        treat = "violence",
                        outcome = "annex",
                        data = tatar,
                        parallel = "multicore",
                        ncpus = 25)

summary(path_out_annex)

## Load output generated from paper code to compare
load("data/annex_bart.RData")
load("data/annex_bart_draw.RData")
summary(path_out_annex)
plot(path_out_annex)

tatar_decomp_draw
tatar_decomp2_draw

tatar_decomp
tatar_decomp2

tatar_decomp_pi_draw
tatar_decomp2_pi_draw

tatar_decomp_pi
tatar_decomp2_pi



### EXAMPLE 3 ###

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(BART)

expit <- function(x) exp(x)/(1+exp(x))

lawton <- read_dta("data/lawton.dta")

cfinance <- lawton %>%
  dplyr::select(id_, frame, nodspeec, nodinter, dspeec, dinter, mdspeec, mdinter,
                year, age, sex, ethnic, knowdum, ondum, pid, ideology,
                speech, interests, impspe, impinter, mfopinio) %>%
  filter(frame %in% c(0, 1)) %>%
  mutate(ipw = 1)

nrow(cfinance)
summary(cfinance)

# mean(cfinance$strikeo[cfinance$frame==1]) - mean(cfinance$strikeo[cfinance$frame==0])
# lm(formula_te, data = cfinance)

x <- c("year", "age", "sex", "ethnic", "knowdum", "ondum", "pid", "ideology")
a <- "frame"
l <- c("impspe", "impinter")
m <- c("speech", "interests")
y <- "mfopinio"


# formula_te <- as.formula(paste(y, " ~ ", paste(c(x, a), collapse= "+")))

summary(lm(formula_y, data = cfinance))
formula_m2 <- as.formula(paste(y, " ~ ", paste(c(x, a), collapse= "+")))
formula_m1 <- as.formula(paste(y, " ~ ", paste(c(x, a, l), collapse= "+")))
formula_y <- as.formula(paste(y, " ~ ", paste(c(x, a, l, m), collapse= "+")))

####

## Model as in the paper << somehow still giving "invalid formula" error even though all the above is running smoothly
path_out_cfinance <- paths(formulas = list(formula_y, formula_m2, formula_m1),
                        models = c("wbart", "wbart", "wbart"),
                        models_args = list(NULL,
                                           NULL,
                                           NULL),
                        conditional = FALSE,
                        ps = FALSE,
                        sims = 500,
                        treat = "frame",
                        outcome = "mfopinio",
                        data = cfinance,
                        parallel = "multicore",
                        ncpus = 25)

summary(path_out_cfinance)

## Load output generated from paper code to compare
load("data/cfinance_bart.RData")
load("data/cfinance_bart_draw.RData")
summary(path_out_cfinance)

cfinance_decomp_draw
cfinance_decomp2_draw

cfinance_decomp
cfinance_decomp2
