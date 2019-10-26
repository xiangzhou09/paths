library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(BART)
library(boot)

expit <- function(x) exp(x)/(1+exp(x))

CCES10_public <- read_dta("CCES10_public.dta")

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

model_fit(peace, list(formula_a), list(model_a_arg), isLm = FALSE, isGlm = TRUE, isBart = FALSE)

#### Test data: results from the paper

## Example 1

load("peace_bart_full.RData")

peace_decomp <- data.frame(out) %>%
  `names<-`(c("te", "ay", "amy", "aly", "almy")) %>%
  summarise_all(list(est = mean, lower = lower, upper = upper)) %>%
  gather(measure, value) %>%
  separate(measure, c("estimand", "measure")) %>%
  spread(measure, value) %>%
  mutate(decomp = "Type I Decomposition",
         estimand = factor(estimand, levels = rev(c("te", "ay", "amy", "aly", "almy")),
                           labels = rev(c("Total Effect", "Direct Effect",
                                          "via Morality", "via Costs and Benefits", "via Costs and Benefits + Morality"))))

#######
## Testing path_fun
paths_fun(data = peace,
         index = 1:nrow(peace),
         formulas = list(formula_y, formula_m2, formula_m1),
         models_args = list(NULL,
                            NULL,
                            NULL),
         treat = "democ",
         outcome = "strikeo",
         conditional = FALSE,
         isLm = c(TRUE, TRUE, TRUE),
         isGlm = c(FALSE, FALSE, FALSE),
         isBart = c(FALSE, FALSE, FALSE),
         ps = TRUE,
         ps_formula = list(formula_a),
         ps_model_args = list(list(binomial(link = "logit"))),
         ps_isLm = FALSE,
         ps_isGlm = TRUE,
         ps_isBart = FALSE)

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

# Model as in the paper
path_out_peace <- paths(formulas = list(formula_y, formula_m2, formula_m1),
                  models = c("pbart", "pbart", "pbart"),
                  models_args = list(NULL,
                                     NULL,
                                     NULL),
                  sims = 500,
                  treat = "democ",
                  outcome = "strikeo",
                  data = peace,
                  parallel = "multicore",
                  ncpus = 20)

summary(path_out_peace)
peace_decomp

plot(path_out_peace)
plot(path_out_peace, c("Morality", "Cost-Benefit Analysis"))

