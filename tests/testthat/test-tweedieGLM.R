test_that("tweedieGLM errors on non-data.frame input", {
  expect_error(
    tweedieGLM(y ~ x1, data = "notadf"),
    "data must be a data frame"
  )
})

test_that("tweedieGLM errors on non-formula input", {
  df <- data.frame(y = c(0, 1.5, 2.3, 0, 3.1), x1 = rnorm(5))
  expect_error(
    tweedieGLM("y ~ x1", data = df),
    "formula must be a formula object"
  )
})

# Shared test data — non-negative semi-continuous with exact zeros
set.seed(42)
df_tw <- data.frame(
  y  = c(0, 0, 1.5, 3.2, 5.8, 0, 0.9, 4.1, 0, 2.7,
         0, 1.1, 0, 3.8, 0, 2.2, 0, 0, 4.9, 1.6),
  x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7,
         -0.5, 0.9, 1.3, -0.3, 0.6, -1.2, 0.1, 2.1, -0.7, 0.4)
)

# Fit once; small dataset may hit iteration caps — suppress expected warnings
fit <- suppressWarnings(tweedieGLM(y ~ x1, data = df_tw))

test_that("tweedieGLM returns correct classes", {
  expect_s3_class(fit, "tweedieGLM")
  expect_s3_class(fit, "countGLMfit")
})

test_that("tweedieGLM returns correct slot names in correct order", {
  expect_named(fit, c("call", "model", "summary", "phi", "p",
                      "coefficients", "diagnostics", "aic", "bic"))
})

test_that("tweedieGLM diagnostics has correct names", {
  expect_named(fit$diagnostics,
               c("rqr", "dispersion_ratio", "plot", "r2_plot", "zi_test"))
})

test_that("tweedieGLM coefficients has correct columns", {
  expect_named(fit$coefficients,
               c("term", "exp.coef", "lower.95", "upper.95", "p.value", "stars"))
})

test_that("tweedieGLM coefficients exp.coef values are positive", {
  expect_true(all(fit$coefficients$exp.coef > 0))
})

test_that("tweedieGLM CIs bracket the point estimate", {
  expect_true(all(fit$coefficients$lower.95 <= fit$coefficients$exp.coef))
  expect_true(all(fit$coefficients$exp.coef <= fit$coefficients$upper.95))
})

test_that("tweedieGLM phi is positive numeric scalar", {
  expect_type(fit$phi, "double")
  expect_length(fit$phi, 1L)
  expect_true(is.na(fit$phi) || fit$phi > 0)
})

test_that("tweedieGLM p is numeric scalar (NA or in (1,2))", {
  expect_type(fit$p, "double")
  expect_length(fit$p, 1L)
  if (!is.na(fit$p)) {
    expect_gt(fit$p, 1)
    expect_lt(fit$p, 2)
  }
})

test_that("tweedieGLM dispersion_ratio is numeric scalar", {
  expect_type(fit$diagnostics$dispersion_ratio, "double")
  expect_length(fit$diagnostics$dispersion_ratio, 1L)
})

test_that("tweedieGLM rqr has length equal to nrow(data)", {
  expect_length(fit$diagnostics$rqr, nrow(df_tw))
})

test_that("tweedieGLM diagnostic plot inherits gg", {
  expect_s3_class(fit$diagnostics$plot, "gg")
})

test_that("tweedieGLM r2_plot is a ggplot object", {
  expect_s3_class(fit$diagnostics$r2_plot, "ggplot")
})

test_that("tweedieGLM summary is a list", {
  expect_type(fit$summary, "list")
})

test_that("tweedieGLM aic is numeric scalar", {
  expect_type(fit$aic, "double")
  expect_length(fit$aic, 1L)
})

test_that("tweedieGLM bic is numeric scalar", {
  expect_type(fit$bic, "double")
  expect_length(fit$bic, 1L)
})

test_that("tweedieGLM bic >= aic (BIC penalises more heavily)", {
  expect_gte(fit$bic, fit$aic)
})

test_that("tweedieGLM zi_test is a list with correct names", {
  expect_type(fit$diagnostics$zi_test, "list")
  expect_named(fit$diagnostics$zi_test, c("detected", "p_value", "plot"))
})

test_that("tweedieGLM zi_test$detected is logical", {
  expect_type(fit$diagnostics$zi_test$detected, "logical")
})

test_that("tweedieGLM zi_test$p_value is numeric in [0, 1]", {
  p <- fit$diagnostics$zi_test$p_value
  expect_type(p, "double")
  expect_gte(p, 0)
  expect_lte(p, 1)
})

test_that("tweedieGLM zi_test$plot is a ggplot", {
  expect_s3_class(fit$diagnostics$zi_test$plot, "ggplot")
})

test_that("print.tweedieGLM does not error", {
  expect_no_error(suppressWarnings(print(fit)))
})

test_that("summary.tweedieGLM returns a list", {
  expect_type(summary(fit), "list")
})

test_that("tweedieGLM assessZeroInflation = FALSE gives NULL zi_test", {
  fit_nozi <- suppressWarnings(
    tweedieGLM(y ~ x1, data = df_tw, assessZeroInflation = FALSE)
  )
  expect_null(fit_nozi$diagnostics$zi_test)
})

test_that("tweedieGLM model slot is a glmmTMB object", {
  expect_s3_class(fit$model, "glmmTMB")
})

# --- Degenerate power parameter tests ---

test_that("tweedieGLM warns when p is at or near the boundary on integer data", {
  df_int <- data.frame(
    y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
    x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
  )
  # glmmTMB collapses p toward 1 on integer counts — should warn about degeneracy
  expect_warning(
    tweedieGLM(y ~ x1, data = df_int, assessZeroInflation = FALSE),
    "degenerate|boundary"
  )
})

test_that("tweedieGLM p is strictly in (1, 2) for semi-continuous data", {
  fit_cont <- suppressWarnings(tweedieGLM(y ~ x1, data = df_tw,
                                          assessZeroInflation = FALSE))
  expect_true(!is.na(fit_cont$p) && fit_cont$p > 1 && fit_cont$p < 2)
})
