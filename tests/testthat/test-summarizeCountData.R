# tests/testthat/test-summarizeCountData.R

# Shared test data
df <- data.frame(
  y = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
  x1 = rnorm(10),
  x2 = sample(c("A", "B"), 10, replace = TRUE),
  x3 = rnorm(10)
)

# --- Input validation ---

test_that("stops if no response in formula", {
  expect_error(summarizeCountData(~x1, df), "No response found")
})

test_that("stops if response is a matrix", {
  df_mat <- df
  df_mat$y <- matrix(1:20, nrow = 10)
  expect_error(summarizeCountData(y ~ x1, df_mat), "must be a vector")
})

test_that("stops if response has negative values", {
  df_neg <- df
  df_neg$y <- df_neg$y - 10L
  expect_error(summarizeCountData(y ~ x1, df_neg), "non-negative integer")
})

test_that("stops if response is non-integer numeric", {
  df_dbl <- df
  df_dbl$y <- df_dbl$y + 0.5
  expect_error(summarizeCountData(y ~ x1, df_dbl), "non-negative integer")
})

# --- Return structure ---

test_that("returns a list with correct names", {
  result <- summarizeCountData(y ~ x1, df)
  expect_named(result, c("summary", "counts", "plot"))
})

test_that("summary table has correct columns", {
  result <- summarizeCountData(y ~ x1, df)
  expect_named(
    result$summary,
    c("mean", "var", "var_mean_ratio", "n_zero", "n_total")
  )
})

test_that("summary table values are correct", {
  result <- summarizeCountData(y ~ x1, df)
  expect_equal(result$summary$mean, mean(df$y))
  expect_equal(result$summary$var, var(df$y))
  expect_equal(result$summary$var_mean_ratio, var(df$y) / mean(df$y))
  expect_equal(result$summary$n_zero, sum(df$y == 0))
  expect_equal(result$summary$n_total, nrow(df))
})

test_that("counts table has correct columns and types", {
  result <- summarizeCountData(y ~ x1, df)
  expect_named(result$counts, c("count", "freq"))
  expect_type(result$counts$count, "integer")
})

test_that("counts table covers all observed values", {
  result <- summarizeCountData(y ~ x1, df)
  expect_setequal(result$counts$count, unique(df$y))
})

# --- Plot output ---

test_that("plot is a ggplot object", {
  result <- summarizeCountData(y ~ x1, df)
  expect_s3_class(result$plot, "ggplot")
})

test_that("no predictors produces a ggplot", {
  result <- summarizeCountData(y ~ 1, df)
  expect_s3_class(result$plot, "ggplot")
})

test_that("one categorical predictor produces a ggplot", {
  result <- summarizeCountData(y ~ x2, df)
  expect_s3_class(result$plot, "ggplot")
})

test_that("two continuous predictors produces a ggplot", {
  result <- summarizeCountData(y ~ x1 + x3, df)
  expect_s3_class(result$plot, "ggplot")
})

test_that("two categorical predictors produces a ggplot", {
  df2 <- df
  df2$x3 <- sample(c("C", "D"), 10, replace = TRUE)
  result <- summarizeCountData(y ~ x2 + x3, df2)
  expect_s3_class(result$plot, "ggplot")
})

test_that("mixed predictors produces a ggplot", {
  result <- summarizeCountData(y ~ x1 + x2, df)
  expect_s3_class(result$plot, "ggplot")
})

test_that("3+ predictors warns and still returns a plot", {
  expect_warning(
    result <- summarizeCountData(y ~ x1 + x2 + x3, df),
    "More than 2 predictors"
  )
  expect_s3_class(result$plot, "ggplot")
})
