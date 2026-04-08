test_that("poissonGLM errors on non-data.frame input", {
  expect_error(
    poissonGLM(y ~ x1, data = "notadf"),
    "data must be a data frame"
  )
})

test_that("poissonGLM errors on non-formula input", {
  df <- data.frame(y = 1:5, x1 = rnorm(5))
  expect_error(
    poissonGLM("y ~ x1", data = df),
    "formula must be a formula object"
  )
})

# Shared test data
df_pois <- data.frame(
  y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
  x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
)

test_that("poissonGLM returns correct classes", {
  fit <- poissonGLM(y ~ x1, data = df_pois)
  expect_s3_class(fit, "poissonGLM")
  expect_s3_class(fit, "countGLMfit")
})

test_that("poissonGLM returns correct slot names", {
  fit <- poissonGLM(y ~ x1, data = df_pois)
  expect_named(fit, c("call", "model", "coefficients", "diagnostics", "aic"))
})

test_that("poissonGLM diagnostics has correct names", {
  fit <- poissonGLM(y ~ x1, data = df_pois)
  expect_named(fit$diagnostics, c("rqr", "dispersion_ratio", "plot"))
})

test_that("poissonGLM exp.coef matches manual glm exponentiated coefficients", {
  fit     <- poissonGLM(y ~ x1, data = df_pois)
  manual  <- exp(stats::coef(stats::glm(y ~ x1, data = df_pois,
                                         family = stats::poisson())))
  expect_equal(fit$coefficients$exp.coef, unname(manual), tolerance = 1e-6)
})

test_that("poissonGLM coefficients has correct columns", {
  fit <- poissonGLM(y ~ x1, data = df_pois)
  expect_named(fit$coefficients, c("term", "exp.coef", "lower.95", "upper.95"))
})

test_that("poissonGLM dispersion_ratio is numeric scalar", {
  fit <- poissonGLM(y ~ x1, data = df_pois)
  expect_type(fit$diagnostics$dispersion_ratio, "double")
  expect_length(fit$diagnostics$dispersion_ratio, 1L)
})

test_that("poissonGLM rqr has length equal to nrow(data)", {
  fit <- poissonGLM(y ~ x1, data = df_pois)
  expect_length(fit$diagnostics$rqr, nrow(df_pois))
})

test_that("poissonGLM diagnostic plot inherits gg", {
  fit <- poissonGLM(y ~ x1, data = df_pois)
  expect_s3_class(fit$diagnostics$plot, "gg")
})

test_that("poissonGLM aic is numeric scalar", {
  fit <- poissonGLM(y ~ x1, data = df_pois)
  expect_type(fit$aic, "double")
  expect_length(fit$aic, 1L)
})

test_that("print.poissonGLM does not error", {
  fit <- poissonGLM(y ~ x1, data = df_pois)
  expect_no_error(print(fit))
})

test_that("summary.poissonGLM returns a list", {
  fit <- poissonGLM(y ~ x1, data = df_pois)
  expect_type(summary(fit), "list")
})
