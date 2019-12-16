## Test if utility functions are performing as intended

# Preparation for test
test_dat <- data.frame(y = rnorm(100),
                       a = rbinom(100, 1, prob = .5),
                       l = rnorm(100),
                       m = rnorm(100))
test_formula_a <- as.formula("y ~ a")
test_formula_m <- as.formula("y ~ a + m")
test_formula_l <- as.formula("y ~ a + m + l")

# Tests

test_that("model_fit returns lists of model objects", {

  object_a <- model_fit(data = test_dat,
                        formulas = list(test_formula_a),
                        models = "glm")
  object_am <- model_fit(data = test_dat,
                         formulas = list(test_formula_a,
                                         test_formula_m),
                         models = c("glm", "pbart"))
  object_aml <- model_fit(data = test_dat,
                          formulas = list(test_formula_a,
                                          test_formula_m,
                                          test_formula_l),
                          models = c("glm", "pbart", "lm"))

  expect_type(object_a, type = "list")
  expect_type(object_am, type = "list")
  expect_type(object_aml, type = "list")

  expect_length(object_a, 1)
  expect_length(object_am, 2)
  expect_length(object_aml, 3)

  expect_s3_class(object_aml[[1]], class = "glm")
  expect_s3_class(object_aml[[2]], class = "pbart")
  expect_s3_class(object_aml[[3]], class = "lm")

})

test_that("model_fit accepts lists of formulas for formulas", {

  # EXPECT NO ERROR
  expect_type(model_fit(data = test_dat,
                        formulas = list(test_formula_a),
                        models = "lm"),
              type = "list")

  # EXPECT NO ERROR -- c() counts as lists too
  expect_type(model_fit(data = test_dat,
                         formulas = c(test_formula_a,
                                      test_formula_m,
                                      test_formula_l),
                         models = c("lm", "lm", "lm")),
              type = "list")

  # EXPECT NO ERROR
  expect_type(model_fit(data = test_dat,
                        formulas = list(test_formula_a,
                                        test_formula_m,
                                        test_formula_l),
                        models = c("lm", "lm", "lm")),
              type = "list")

  # EXPECT ERROR
  expect_error(model_fit(data = test_dat,
                         formulas = test_formula_a,
                         models = "lm"),
               regexp = "must be supplied as a list")

})

test_that("model_fit accepts lists or NULL for models_args", {

  # EXPECT MESSAGE
  expect_message(model_fit(data = test_dat,
                        formulas = list(test_formula_a),
                        models = "lm"),
              regexp = "Argument 'models_args' is not supplied")


  # EXPECT NO ERROR
  expect_type(model_fit(data = test_dat,
                        formulas = list(test_formula_a),
                        models = "lm",
                        models_args = list(NULL)),
              type = "list")

  # EXPECT ERROR (TO PREVENT CRASH)
  expect_error(model_fit(data = test_dat,
                         formulas = list(test_formula_a),
                         models = "pbart",
                         models_args = list(list(NULL))),
               regexp = "No list of arguments .+ NULL")

  # EXPECT ERROR (REDUNDANT BUT NEEDED FOR CONSISTENCY WITH ABOVE)
  expect_error(model_fit(data = test_dat,
                         formulas = list(test_formula_a),
                         models = "lm",
                         models_args = list(list(NULL))),
               regexp = "No list of arguments .+ NULL")


  # EXPECT NO ERROR
  expect_type(model_fit(data = test_dat,
                        formulas = list(test_formula_a),
                        models = "lm",
                        models_args = list(list(theta = 0))),
              type = "list")

  # EXPECT ERROR
  expect_error(model_fit(data = test_dat,
                          formulas = list(test_formula_a),
                          models = "pbart",
                          models_args = list(theta = 0, omega = 1)),
               regexp = "must be a list containing lists")

  # EXPECT ERROR
  expect_error(model_fit(data = test_dat,
                          formulas = list(test_formula_a),
                          models = "pbart",
                          models_args = list(sparse = FALSE, omega = 1)),
               regexp = "must be a list containing lists")
})
