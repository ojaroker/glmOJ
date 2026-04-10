test_that("countGLM errors on non-data.frame input", {
  expect_error(
    countGLM(y ~ x1, data = "notadf"),
    "data must be a data frame"
  )
})

test_that("countGLM errors on non-formula input", {
  df <- data.frame(y = 1:5, x1 = rnorm(5))
  expect_error(
    countGLM("y ~ x1", data = df),
    "formula must be a formula object"
  )
})

test_that("countGLM errors on invalid ziformula", {
  df <- data.frame(y = c(0L, 0L, 1L, 2L, 0L), x1 = rnorm(5))
  expect_error(
    countGLM(y ~ x1, data = df, ziformula = "~ 1"),
    "ziformula must be a formula or NULL"
  )
})

# Shared test data
df_cglm <- data.frame(
  y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
  x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
)

# Fit once; countGLM fits all 4 models and small datasets often hit
# iteration limits for NB and zero-inflated optimisers — suppress expected warnings
result <- suppressWarnings(countGLM(y ~ x1, data = df_cglm))

test_that("countGLM returns class 'countGLM'", {
  expect_s3_class(result, "countGLM")
})

test_that("countGLM returns correct slot names", {
  expect_named(result, c("call", "fits", "aic_table", "bic_table", "best_model", "recommendation"))
})

test_that("countGLM best_model is one of the four valid names", {
  valid <- c("poisson", "negbin", "zeroinfl_poisson", "zeroinfl_negbin")
  expect_true(result$best_model %in% valid)
})

test_that("countGLM aic_table is a named numeric vector of length <= 4", {
  expect_type(result$aic_table, "double")
  expect_lte(length(result$aic_table), 4L)
  expect_false(is.null(names(result$aic_table)))
})

test_that("countGLM bic_table is a named numeric vector of length <= 4", {
  expect_type(result$bic_table, "double")
  expect_lte(length(result$bic_table), 4L)
  expect_false(is.null(names(result$bic_table)))
})

test_that("countGLM recommendation is a non-empty character string", {
  expect_type(result$recommendation, "character")
  expect_gt(nchar(result$recommendation), 0L)
})

test_that("countGLM each successful fit inherits countGLMfit", {
  for (nm in names(result$fits)) {
    expect_true(
      inherits(result$fits[[nm]], "countGLMfit"),
      label = paste("model", nm, "inherits countGLMfit")
    )
  }
})

test_that("print.countGLM does not error", {
  expect_no_error(print(result))
})

test_that("summary.countGLM does not error", {
  expect_no_error(suppressMessages(summary(result)))
})

test_that("countGLM aic_table is sorted ascending", {
  aics <- result$aic_table
  expect_equal(aics, sort(aics))
})
