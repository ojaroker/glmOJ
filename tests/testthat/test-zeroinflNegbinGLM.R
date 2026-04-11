test_that("zeroinflNegbinGLM errors on non-data.frame input", {
  expect_error(
    zeroinflNegbinGLM(y ~ x1, data = "notadf"),
    "data must be a data frame"
  )
})

test_that("zeroinflNegbinGLM errors on non-formula input", {
  df <- data.frame(y = c(0L, 0L, 1L, 2L, 0L), x1 = rnorm(5))
  expect_error(
    zeroinflNegbinGLM("y ~ x1", data = df),
    "formula must be a formula object"
  )
})

test_that("zeroinflNegbinGLM errors on invalid ziformula", {
  df <- data.frame(y = c(0L, 0L, 1L, 2L, 0L), x1 = rnorm(5))
  expect_error(
    zeroinflNegbinGLM(y ~ x1, data = df, ziformula = "~ 1"),
    "ziformula must be a formula or NULL"
  )
})

# Shared test data — many zeros + overdispersion
set.seed(42)
df_zinb <- data.frame(
  y  = c(0L, 0L, 0L, 1L, 20L, 0L, 15L, 0L, 1L, 0L,
         0L, 0L, 8L, 0L, 1L, 0L, 0L, 30L, 0L, 0L),
  x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7,
         -0.5, 0.9, 1.3, -0.3, 0.6, -1.2, 0.1, 2.1, -0.7, 0.4)
)

# Fit once; small dataset may hit iteration limits — suppress expected warnings
fit     <- suppressWarnings(zeroinflNegbinGLM(y ~ x1, data = df_zinb))
fit_zi1 <- suppressWarnings(zeroinflNegbinGLM(y ~ x1, data = df_zinb, ziformula = ~ 1))

test_that("zeroinflNegbinGLM returns correct classes", {
  expect_s3_class(fit, "zeroinflNegbinGLM")
  expect_s3_class(fit, "zeroinflGLMfit")
  expect_s3_class(fit, "countGLMfit")
})

test_that("zeroinflNegbinGLM returns correct slot names in correct order", {
  expect_named(fit, c("call", "model", "summary", "theta", "coefficients",
                       "diagnostics", "aic", "bic"))
})

test_that("zeroinflNegbinGLM coefficients is a list with count and zero", {
  expect_type(fit$coefficients, "list")
  expect_named(fit$coefficients, c("count", "zero"))
})

test_that("zeroinflNegbinGLM count coefficients has correct columns", {
  expect_named(fit$coefficients$count, c("term", "exp.coef", "lower.95", "upper.95", "p.value", "stars"))
})

test_that("zeroinflNegbinGLM zero coefficients has correct columns", {
  expect_named(fit$coefficients$zero, c("term", "exp.coef", "lower.95", "upper.95", "p.value", "stars"))
})

test_that("zeroinflNegbinGLM theta is positive numeric", {
  expect_type(fit$theta, "double")
  expect_gt(fit$theta, 0)
})

test_that("zeroinflNegbinGLM with ziformula = ~ 1 gives 1-row zero table", {
  expect_equal(nrow(fit_zi1$coefficients$zero), 1L)
})

test_that("zeroinflNegbinGLM diagnostics has correct names", {
  expect_named(fit$diagnostics, c("rqr", "dispersion_ratio", "plot", "r2_plot"))
})

test_that("zeroinflNegbinGLM dispersion_ratio is numeric scalar", {
  expect_type(fit$diagnostics$dispersion_ratio, "double")
  expect_length(fit$diagnostics$dispersion_ratio, 1L)
})

test_that("zeroinflNegbinGLM rqr has length equal to nrow(data)", {
  expect_length(fit$diagnostics$rqr, nrow(df_zinb))
})

test_that("zeroinflNegbinGLM diagnostic plot inherits gg", {
  expect_s3_class(fit$diagnostics$plot, "gg")
})

test_that("zeroinflNegbinGLM r2_plot is a ggplot object", {
  expect_s3_class(fit$diagnostics$r2_plot, "ggplot")
})

test_that("zeroinflNegbinGLM summary is a list", {
  expect_type(fit$summary, "list")
})

test_that("zeroinflNegbinGLM aic is numeric scalar", {
  expect_type(fit$aic, "double")
  expect_length(fit$aic, 1L)
})

test_that("zeroinflNegbinGLM bic is numeric scalar", {
  expect_type(fit$bic, "double")
  expect_length(fit$bic, 1L)
})

test_that("zeroinflNegbinGLM bic >= aic (BIC penalises more heavily)", {
  expect_gte(fit$bic, fit$aic)
})

test_that("print.zeroinflNegbinGLM does not error", {
  expect_no_error(print(fit))
})

test_that("summary.zeroinflNegbinGLM returns a list", {
  expect_type(summary(fit), "list")
})
