####################################################
# Example 1: Three BART models for three mediators
####################################################

# **For illustration purposes a small number of simulations are used**

data(tatar)

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


## Model as in Yamamoto & Zhou (2020)
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
                        sims = 100,
                        treat = "violence",
                        outcome = "annex",
                        data = tatar)

summary(path_out_annex)
