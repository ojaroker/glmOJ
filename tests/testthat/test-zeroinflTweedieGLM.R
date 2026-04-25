test_that("zeroinflTweedieGLM errors on non-data.frame input", {
  expect_error(
    zeroinflTweedieGLM(y ~ x1, data = "notadf"),
    "data must be a data frame"
  )
})

test_that("zeroinflTweedieGLM errors on non-formula input", {
  df <- data.frame(y = c(0, 0, 1.5, 2.3, 0), x1 = rnorm(5))
  expect_error(
    zeroinflTweedieGLM("y ~ x1", data = df),
    "formula must be a formula object"
  )
})

test_that("zeroinflTweedieGLM errors on invalid ziformula", {
  df <- data.frame(y = c(0, 0, 1.5, 2.3, 0), x1 = rnorm(5))
  expect_error(
    zeroinflTweedieGLM(y ~ x1, data = df, ziformula = "~ 1"),
    "ziformula must be a formula or NULL"
  )
})

# Shared test data — many zeros with positive semi-continuous values
set.seed(42)
df_zitw <- data.frame(
  y  = c(0, 0, 0, 1.5, 3.2, 0, 0.9, 0, 0, 2.7,
         0, 0, 4.1, 0, 1.2, 0, 0, 5.8, 0, 0),
  x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7,
         -0.5, 0.9, 1.3, -0.3, 0.6, -1.2, 0.1, 2.1, -0.7, 0.4)
)

# Fit once; small dataset may hit iteration limits — suppress expected warnings
fit     <- suppressWarnings(zeroinflTweedieGLM(y ~ x1, data = df_zitw))
fit_zi1 <- suppressWarnings(
  zeroinflTweedieGLM(y ~ x1, data = df_zitw, ziformula = ~ 1)
)

test_that("zeroinflTweedieGLM returns correct classes", {
  expect_s3_class(fit, "zeroinflTweedieGLM")
  expect_s3_class(fit, "zeroinflGLMfit")
  expect_s3_class(fit, "countGLMfit")
})

test_that("zeroinflTweedieGLM returns correct slot names in correct order", {
  expect_named(fit, c("call", "model", "summary", "phi", "p",
                      "coefficients", "diagnostics", "aic", "bic"))
})

test_that("zeroinflTweedieGLM coefficients is a list with count and zero", {
  expect_type(fit$coefficients, "list")
  expect_named(fit$coefficients, c("count", "zero"))
})

test_that("zeroinflTweedieGLM count coefficients has correct columns", {
  expect_named(fit$coefficients$count,
               c("term", "exp.coef", "lower.95", "upper.95", "p.value", "stars"))
})

test_that("zeroinflTweedieGLM zero coefficients has correct columns", {
  expect_named(fit$coefficients$zero,
               c("term", "exp.coef", "lower.95", "upper.95", "p.value", "stars"))
})

test_that("zeroinflTweedieGLM count exp.coef values are positive", {
  expect_true(all(fit$coefficients$count$exp.coef > 0))
})

test_that("zeroinflTweedieGLM zero exp.coef values are positive", {
  expect_true(all(fit$coefficients$zero$exp.coef > 0))
})

test_that("zeroinflTweedieGLM phi is positive numeric scalar", {
  expect_type(fit$phi, "double")
  expect_length(fit$phi, 1L)
  expect_true(is.na(fit$phi) || fit$phi > 0)
})

test_that("zeroinflTweedieGLM p is numeric scalar (NA or in (1,2))", {
  expect_type(fit$p, "double")
  expect_length(fit$p, 1L)
  if (!is.na(fit$p)) {
    expect_gt(fit$p, 1)
    expect_lt(fit$p, 2)
  }
})

test_that("zeroinflTweedieGLM with ziformula = ~ 1 gives 1-row zero table", {
  expect_equal(nrow(fit_zi1$coefficients$zero), 1L)
})

test_that("zeroinflTweedieGLM diagnostics has correct names", {
  expect_named(fit$diagnostics, c("rqr", "dispersion_ratio", "plot", "r2_plot"))
})

test_that("zeroinflTweedieGLM dispersion_ratio is numeric scalar", {
  expect_type(fit$diagnostics$dispersion_ratio, "double")
  expect_length(fit$diagnostics$dispersion_ratio, 1L)
})

test_that("zeroinflTweedieGLM rqr has length equal to nrow(data)", {
  expect_length(fit$diagnostics$rqr, nrow(df_zitw))
})

test_that("zeroinflTweedieGLM diagnostic plot inherits gg", {
  expect_s3_class(fit$diagnostics$plot, "gg")
})

test_that("zeroinflTweedieGLM r2_plot is a ggplot object", {
  expect_s3_class(fit$diagnostics$r2_plot, "ggplot")
})

test_that("zeroinflTweedieGLM summary is a list", {
  expect_type(fit$summary, "list")
})

test_that("zeroinflTweedieGLM aic is numeric scalar", {
  expect_type(fit$aic, "double")
  expect_length(fit$aic, 1L)
})

test_that("zeroinflTweedieGLM bic is numeric scalar", {
  expect_type(fit$bic, "double")
  expect_length(fit$bic, 1L)
})

test_that("zeroinflTweedieGLM bic >= aic (BIC penalises more heavily)", {
  expect_gte(fit$bic, fit$aic)
})

test_that("print.zeroinflTweedieGLM does not error", {
  expect_no_error(suppressWarnings(print(fit)))
})

test_that("summary.zeroinflTweedieGLM returns a list", {
  expect_type(summary(fit), "list")
})

test_that("coef.zeroinflGLMfit returns concatenated named vector by default for zeroinflTweedieGLM", {
  cc <- coef(fit)
  expect_type(cc, "double")
  expect_true(all(grepl("^(count|zero)_", names(cc))))
})

test_that("coef components return named numeric vectors for zeroinflTweedieGLM", {
  cc <- coef(fit, component = "count")
  cz <- coef(fit, component = "zero")
  expect_type(cc, "double")
  expect_type(cz, "double")
  expect_true("(Intercept)" %in% names(cc))
  expect_true("(Intercept)" %in% names(cz))
})

test_that("coef_table.zeroinflGLMfit returns count + zero data frames", {
  tab <- coef_table(fit)
  expect_named(tab, c("count", "zero"))
})

test_that("zeroinflTweedieGLM model slot is a glmmTMB object", {
  expect_s3_class(fit$model, "glmmTMB")
})
