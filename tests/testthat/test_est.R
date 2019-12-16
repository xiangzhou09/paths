## Test if path_fun() is performing as intended

# Preparation for test
test_dat <- data.frame(y = rnorm(100),
                       a = rbinom(100, 1, prob = .5),
                       m1 = rnorm(100),
                       m2 = rnorm(100),
                       x = rnorm(100))

test_formula_m1 <- as.formula("y ~ a + x")
test_formula_m2 <- as.formula("y ~ a + m1 + x")
test_formula_y <- as.formula("y ~ a + m1 + m2 + x")
test_formula_a <- as.formula("a ~ x")

# Tests
test_that("path_fun returns objects with expected length", {
  ind <- 1:100

  paths_unconditional <- paths_fun(data = test_dat,
                                   index = ind,
                                   formulas = list(test_formula_y, test_formula_m2, test_formula_m1),
                                   models = c("lm", "lm", "lm"),
                                   models_args = list(NULL,
                                                      NULL,
                                                      NULL),
                                   treat = "a",
                                   outcome = "y",
                                   conditional = FALSE,
                                   ps = FALSE)

  paths_conditional_ipw <- paths_fun(data = test_dat,
                                   index = ind,
                                   formulas = list(test_formula_y, test_formula_m2, test_formula_m1),
                                   models = c("lm", "lm", "lm"),
                                   models_args = list(NULL,
                                                      NULL,
                                                      NULL),
                                   treat = "a",
                                   outcome = "y",
                                   conditional = TRUE,
                                   ps = TRUE,
                                   ps_formula = list(test_formula_a),
                                   ps_model = "glm",
                                   ps_model_args = list(list(binomial(link = "logit"))))

  paths_conditional_pi <- paths_fun(data = test_dat,
                                      index = ind,
                                      formulas = list(test_formula_y, test_formula_m2, test_formula_m1),
                                      models = c("lm", "lm", "lm"),
                                      models_args = list(NULL,
                                                         NULL,
                                                         NULL),
                                      treat = "a",
                                      outcome = "y",
                                      conditional = TRUE,
                                      ps = FALSE)

  expect_setequal(c(length(paths_unconditional),
                    length(paths_conditional_ipw),
                    length(paths_conditional_pi)),
                  7)

})
