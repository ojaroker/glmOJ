test_that("negbinGLM errors on non-data.frame input", {
  expect_error(
    negbinGLM(y ~ x1, data = "notadf"),
    "data must be a data frame"
  )
})

test_that("negbinGLM errors on non-formula input", {
  df <- data.frame(y = 1:5, x1 = rnorm(5))
  expect_error(
    negbinGLM("y ~ x1", data = df),
    "formula must be a formula object"
  )
})

# Shared test data — some overdispersion to make NB sensible
df_nb <- data.frame(
  y  = c(0L, 1L, 10L, 3L, 25L, 0L, 2L, 14L, 1L, 8L),
  x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
)

# Fit once; small dataset may hit glm.nb iteration cap — suppress expected warnings
fit <- suppressWarnings(negbinGLM(y ~ x1, data = df_nb))

test_that("negbinGLM returns correct classes", {
  expect_s3_class(fit, "negbinGLM")
  expect_s3_class(fit, "countGLMfit")
})

test_that("negbinGLM returns correct slot names in correct order", {
  expect_named(fit, c("call", "model", "summary", "theta", "coefficients",
                       "diagnostics", "aic", "bic"))
})

test_that("negbinGLM diagnostics has correct names", {
  expect_named(fit$diagnostics, c("rqr", "dispersion_ratio", "plot", "r2_plot"))
})

test_that("negbinGLM exp.coef matches manual MASS::glm.nb exponentiated coefficients", {
  manual <- exp(stats::coef(suppressWarnings(MASS::glm.nb(y ~ x1, data = df_nb))))
  expect_equal(fit$coefficients$exp.coef, unname(manual), tolerance = 1e-6)
})

test_that("negbinGLM coefficients has correct columns", {
  expect_named(fit$coefficients, c("term", "exp.coef", "lower.95", "upper.95"))
})

test_that("negbinGLM theta is positive numeric", {
  expect_type(fit$theta, "double")
  expect_gt(fit$theta, 0)
})

test_that("negbinGLM dispersion_ratio is numeric scalar", {
  expect_type(fit$diagnostics$dispersion_ratio, "double")
  expect_length(fit$diagnostics$dispersion_ratio, 1L)
})

test_that("negbinGLM rqr has length equal to nrow(data)", {
  expect_length(fit$diagnostics$rqr, nrow(df_nb))
})

test_that("negbinGLM diagnostic plot inherits gg", {
  expect_s3_class(fit$diagnostics$plot, "gg")
})

test_that("negbinGLM r2_plot is a ggplot object", {
  expect_s3_class(fit$diagnostics$r2_plot, "ggplot")
})

test_that("negbinGLM summary is a list (glm.nb summary object)", {
  expect_type(fit$summary, "list")
})

test_that("negbinGLM aic is numeric scalar", {
  expect_type(fit$aic, "double")
  expect_length(fit$aic, 1L)
})

test_that("negbinGLM bic is numeric scalar", {
  expect_type(fit$bic, "double")
  expect_length(fit$bic, 1L)
})

test_that("negbinGLM bic >= aic (BIC penalises more heavily)", {
  expect_gte(fit$bic, fit$aic)
})

test_that("print.negbinGLM does not error", {
  expect_no_error(print(fit))
})

test_that("summary.negbinGLM returns a list", {
  expect_type(summary(fit), "list")
})
