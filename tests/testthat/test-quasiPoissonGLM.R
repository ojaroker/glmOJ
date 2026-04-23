test_that("quasiPoissonGLM errors on non-data.frame input", {
  expect_error(
    quasiPoissonGLM(y ~ x1, data = "notadf"),
    "data must be a data frame"
  )
})

test_that("quasiPoissonGLM errors on non-formula input", {
  df <- data.frame(y = 1:5, x1 = rnorm(5))
  expect_error(
    quasiPoissonGLM("y ~ x1", data = df),
    "formula must be a formula object"
  )
})

# Shared test data (same as poissonGLM tests)
df_qp <- data.frame(
  y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
  x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
)

fit <- suppressWarnings(quasiPoissonGLM(y ~ x1, data = df_qp))

test_that("quasiPoissonGLM returns correct classes", {
  expect_s3_class(fit, "quasiPoissonGLM")
  expect_s3_class(fit, "countGLMfit")
})

test_that("quasiPoissonGLM returns correct slot names in correct order", {
  expect_named(fit, c("call", "model", "summary", "phi", "coefficients",
                      "diagnostics", "aic", "bic"))
})

test_that("quasiPoissonGLM diagnostics has correct names", {
  expect_named(fit$diagnostics,
               c("rqr", "dispersion_ratio", "plot", "r2_plot"))
})

test_that("quasiPoissonGLM exp.coef matches Poisson exp coefs (same point estimates)", {
  manual <- exp(stats::coef(stats::glm(y ~ x1, data = df_qp,
                                        family = stats::poisson())))
  expect_equal(fit$coefficients$exp.coef, unname(manual), tolerance = 1e-6)
})

test_that("quasiPoissonGLM coefficients has correct columns", {
  expect_named(fit$coefficients,
               c("term", "exp.coef", "lower.95", "upper.95", "p.value", "stars"))
})

test_that("quasiPoissonGLM phi equals the dispersion_ratio", {
  expect_equal(fit$phi, fit$diagnostics$dispersion_ratio, tolerance = 1e-5)
})

test_that("quasiPoissonGLM aic and bic are NA", {
  expect_true(is.na(fit$aic))
  expect_true(is.na(fit$bic))
})

test_that("quasiPoissonGLM rqr length matches nrow(data)", {
  expect_length(fit$diagnostics$rqr, nrow(df_qp))
})

test_that("quasiPoissonGLM diagnostic plot inherits gg", {
  expect_s3_class(fit$diagnostics$plot, "gg")
})

test_that("quasiPoissonGLM r2_plot is a ggplot object", {
  expect_s3_class(fit$diagnostics$r2_plot, "ggplot")
})

test_that("print.quasiPoissonGLM does not error", {
  expect_no_error(print(fit))
})

test_that("summary.quasiPoissonGLM returns a list", {
  expect_type(summary(fit), "list")
})

test_that("quasiPoissonGLM SEs are inflated relative to Poisson when phi > 1", {
  # Build a dataset where the Poisson fit is clearly overdispersed
  set.seed(42)
  n <- 200
  x <- rnorm(n)
  # negative-binomial-ish counts → Poisson fit overdispersed
  y <- stats::rnbinom(n, size = 2, mu = exp(1 + 0.3 * x))
  dfo <- data.frame(y = y, x = x)

  q <- suppressWarnings(quasiPoissonGLM(y ~ x, data = dfo))
  p <- suppressWarnings(poissonGLM(y ~ x, data = dfo, assessZeroInflation = FALSE))

  # Point estimates identical
  expect_equal(q$coefficients$exp.coef, p$coefficients$exp.coef,
               tolerance = 1e-6)
  # Quasi CIs wider when phi > 1
  expect_gt(q$phi, 1)
  q_width <- q$coefficients$upper.95 - q$coefficients$lower.95
  p_width <- p$coefficients$upper.95 - p$coefficients$lower.95
  expect_true(all(q_width >= p_width - 1e-8))
})
