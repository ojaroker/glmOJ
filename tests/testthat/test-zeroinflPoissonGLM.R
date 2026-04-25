test_that("zeroinflPoissonGLM errors on non-data.frame input", {
  expect_error(
    zeroinflPoissonGLM(y ~ x1, data = "notadf"),
    "data must be a data frame"
  )
})

test_that("zeroinflPoissonGLM errors on non-formula input", {
  df <- data.frame(y = c(0L, 0L, 1L, 2L, 0L), x1 = rnorm(5))
  expect_error(
    zeroinflPoissonGLM("y ~ x1", data = df),
    "formula must be a formula object"
  )
})

test_that("zeroinflPoissonGLM errors on invalid ziformula", {
  df <- data.frame(y = c(0L, 0L, 1L, 2L, 0L), x1 = rnorm(5))
  expect_error(
    zeroinflPoissonGLM(y ~ x1, data = df, ziformula = "~ 1"),
    "ziformula must be a formula or NULL"
  )
})

# Shared test data — many zeros to make ZI model sensible
set.seed(42)
df_zip <- data.frame(
  y  = c(0L, 0L, 0L, 1L, 2L, 0L, 3L, 0L, 1L, 0L,
         0L, 0L, 2L, 0L, 1L, 0L, 0L, 4L, 0L, 0L),
  x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7,
         -0.5, 0.9, 1.3, -0.3, 0.6, -1.2, 0.1, 2.1, -0.7, 0.4)
)

# Fit once; small dataset may hit iteration limits — suppress expected warnings
fit     <- suppressWarnings(zeroinflPoissonGLM(y ~ x1, data = df_zip))
fit_zi1 <- suppressWarnings(zeroinflPoissonGLM(y ~ x1, data = df_zip, ziformula = ~ 1))

test_that("zeroinflPoissonGLM returns correct classes", {
  expect_s3_class(fit, "zeroinflPoissonGLM")
  expect_s3_class(fit, "zeroinflGLMfit")
  expect_s3_class(fit, "countGLMfit")
})

test_that("zeroinflPoissonGLM returns correct slot names in correct order", {
  expect_named(fit, c("call", "model", "summary", "coefficients", "diagnostics", "aic", "bic"))
})

test_that("zeroinflPoissonGLM coefficients is a list with count and zero", {
  expect_type(fit$coefficients, "list")
  expect_named(fit$coefficients, c("count", "zero"))
})

test_that("zeroinflPoissonGLM count coefficients has correct columns", {
  expect_named(fit$coefficients$count, c("term", "exp.coef", "lower.95", "upper.95", "p.value", "stars"))
})

test_that("zeroinflPoissonGLM zero coefficients has correct columns", {
  expect_named(fit$coefficients$zero, c("term", "exp.coef", "lower.95", "upper.95", "p.value", "stars"))
})

test_that("zeroinflPoissonGLM with ziformula = ~ 1 gives 1-row zero table", {
  expect_equal(nrow(fit_zi1$coefficients$zero), 1L)
})

test_that("zeroinflPoissonGLM diagnostics has correct names", {
  expect_named(fit$diagnostics, c("rqr", "dispersion_ratio", "plot", "r2_plot"))
})

test_that("zeroinflPoissonGLM dispersion_ratio is numeric scalar", {
  expect_type(fit$diagnostics$dispersion_ratio, "double")
  expect_length(fit$diagnostics$dispersion_ratio, 1L)
})

test_that("zeroinflPoissonGLM rqr has length equal to nrow(data)", {
  expect_length(fit$diagnostics$rqr, nrow(df_zip))
})

test_that("zeroinflPoissonGLM diagnostic plot inherits gg", {
  expect_s3_class(fit$diagnostics$plot, "gg")
})

test_that("zeroinflPoissonGLM r2_plot is a ggplot object", {
  expect_s3_class(fit$diagnostics$r2_plot, "ggplot")
})

test_that("zeroinflPoissonGLM summary is a list", {
  expect_type(fit$summary, "list")
})

test_that("zeroinflPoissonGLM aic is numeric scalar", {
  expect_type(fit$aic, "double")
  expect_length(fit$aic, 1L)
})

test_that("zeroinflPoissonGLM bic is numeric scalar", {
  expect_type(fit$bic, "double")
  expect_length(fit$bic, 1L)
})

test_that("zeroinflPoissonGLM bic >= aic (BIC penalises more heavily)", {
  expect_gte(fit$bic, fit$aic)
})

test_that("print.zeroinflPoissonGLM does not error", {
  expect_no_error(print(fit))
})

test_that("summary.zeroinflPoissonGLM returns a list", {
  expect_type(summary(fit), "list")
})

test_that("coef.zeroinflGLMfit returns concatenated named vector by default", {
  cc <- coef(fit)
  expect_type(cc, "double")
  expect_true(all(grepl("^(count|zero)_", names(cc))))
})

test_that("coef.zeroinflGLMfit count/zero components return named numeric vectors", {
  cc <- coef(fit, component = "count")
  cz <- coef(fit, component = "zero")
  expect_type(cc, "double")
  expect_type(cz, "double")
  expect_true("(Intercept)" %in% names(cc))
  expect_true("(Intercept)" %in% names(cz))
})

test_that("coef_table.zeroinflGLMfit returns a list of count and zero data frames", {
  tab <- coef_table(fit)
  expect_named(tab, c("count", "zero"))
  expect_named(tab$count, c("term", "exp.coef", "lower.95", "upper.95", "p.value", "stars"))
  expect_named(tab$zero,  c("term", "exp.coef", "lower.95", "upper.95", "p.value", "stars"))
})
