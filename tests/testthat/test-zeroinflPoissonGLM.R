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

test_that("zeroinflPoissonGLM returns correct classes", {
  fit <- zeroinflPoissonGLM(y ~ x1, data = df_zip)
  expect_s3_class(fit, "zeroinflPoissonGLM")
  expect_s3_class(fit, "zeroinflGLMfit")
  expect_s3_class(fit, "countGLMfit")
})

test_that("zeroinflPoissonGLM returns correct slot names", {
  fit <- zeroinflPoissonGLM(y ~ x1, data = df_zip)
  expect_named(fit, c("call", "model", "coefficients", "diagnostics", "aic"))
})

test_that("zeroinflPoissonGLM coefficients is a list with count and zero", {
  fit <- zeroinflPoissonGLM(y ~ x1, data = df_zip)
  expect_type(fit$coefficients, "list")
  expect_named(fit$coefficients, c("count", "zero"))
})

test_that("zeroinflPoissonGLM count coefficients has correct columns", {
  fit <- zeroinflPoissonGLM(y ~ x1, data = df_zip)
  expect_named(fit$coefficients$count, c("term", "exp.coef", "lower.95", "upper.95"))
})

test_that("zeroinflPoissonGLM zero coefficients has correct columns", {
  fit <- zeroinflPoissonGLM(y ~ x1, data = df_zip)
  expect_named(fit$coefficients$zero, c("term", "exp.coef", "lower.95", "upper.95"))
})

test_that("zeroinflPoissonGLM with ziformula = ~ 1 gives 1-row zero table", {
  fit <- zeroinflPoissonGLM(y ~ x1, data = df_zip, ziformula = ~ 1)
  expect_equal(nrow(fit$coefficients$zero), 1L)
})

test_that("zeroinflPoissonGLM diagnostics has correct names", {
  fit <- zeroinflPoissonGLM(y ~ x1, data = df_zip)
  expect_named(fit$diagnostics, c("rqr", "dispersion_ratio", "plot"))
})

test_that("zeroinflPoissonGLM dispersion_ratio is numeric scalar", {
  fit <- zeroinflPoissonGLM(y ~ x1, data = df_zip)
  expect_type(fit$diagnostics$dispersion_ratio, "double")
  expect_length(fit$diagnostics$dispersion_ratio, 1L)
})

test_that("zeroinflPoissonGLM rqr has length equal to nrow(data)", {
  fit <- zeroinflPoissonGLM(y ~ x1, data = df_zip)
  expect_length(fit$diagnostics$rqr, nrow(df_zip))
})

test_that("zeroinflPoissonGLM diagnostic plot inherits gg", {
  fit <- zeroinflPoissonGLM(y ~ x1, data = df_zip)
  expect_s3_class(fit$diagnostics$plot, "gg")
})

test_that("print.zeroinflPoissonGLM does not error", {
  fit <- zeroinflPoissonGLM(y ~ x1, data = df_zip)
  expect_no_error(print(fit))
})

test_that("summary.zeroinflPoissonGLM returns a list", {
  fit <- zeroinflPoissonGLM(y ~ x1, data = df_zip)
  expect_type(summary(fit), "list")
})

test_that("coef.zeroinflGLMfit returns count table by default", {
  fit <- zeroinflPoissonGLM(y ~ x1, data = df_zip)
  cc  <- coef(fit)
  expect_named(cc, c("term", "exp.coef", "lower.95", "upper.95"))
})

test_that("coef.zeroinflGLMfit returns zero table when component = 'zero'", {
  fit <- zeroinflPoissonGLM(y ~ x1, data = df_zip)
  cz  <- coef(fit, component = "zero")
  expect_named(cz, c("term", "exp.coef", "lower.95", "upper.95"))
})
