# tests/testthat/test-summarizeCountData.R

set.seed(42)

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

# Small dataset: few unique counts (<=10) — response treated as factor in pairs
df_small <- data.frame(
  y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
  x1 = rnorm(10),
  x2 = sample(c("A", "B"), 10, replace = TRUE),
  x3 = rnorm(10)
)

# Larger dataset: many unique counts (>10) — response treated as numeric in pairs
df_large <- data.frame(
  y  = as.integer(rpois(100, lambda = 10)),
  x1 = rnorm(100),
  x2 = sample(c("A", "B", "C"), 100, replace = TRUE),
  x3 = rnorm(100),
  x4 = sample(c("X", "Y"), 100, replace = TRUE)
)

# Dataset with NAs in predictors
df_na <- data.frame(
  y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
  x1 = c(NA, 1.2, -0.4, 0.8, NA, 2.0, 0.3, -0.9, 1.5, -0.2),
  x2 = c("A", NA, "B", "A", "B", NA, "A", "B", "A", "B")
)

df_offset <- data.frame(
  y = c(0L, 1L, 2L, 3L, 4L, 1L, 0L, 2L, 1L, 3L),
  x1 = rnorm(10),
  exposure = c(1, 2, 1, 4, 2, 1, 3, 2, 1, 5)
)

# Dataset with only zero counts
df_zeros <- data.frame(
  y  = rep(0L, 10),
  x1 = rnorm(10)
)

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("stops if no response in formula", {
  expect_error(summarizeCountData(~x1, df_small), "No response found")
})

test_that("stops if response is a matrix", {
  df_mat      <- df_small
  df_mat$y    <- matrix(1:20, nrow = 10)
  expect_error(summarizeCountData(y ~ x1, df_mat), "must be a vector")
})

test_that("stops if response has negative values", {
  df_neg   <- df_small
  df_neg$y <- df_neg$y - 10L
  expect_error(summarizeCountData(y ~ x1, df_neg), "non-negative integer")
})

test_that("stops if response is non-integer numeric", {
  df_dbl   <- df_small
  df_dbl$y <- df_dbl$y + 0.5
  expect_error(summarizeCountData(y ~ x1, df_dbl), "non-negative integer")
})

# ---------------------------------------------------------------------------
# Return structure
# ---------------------------------------------------------------------------

test_that("returns a list with correct names including pairs_plot", {
  result <- summarizeCountData(y ~ x1, df_small)
  expect_named(result, c("summary", "counts", "plot", "pairs_plot"))
})

test_that("summary table has correct columns", {
  result <- summarizeCountData(y ~ x1, df_small)
  expect_named(
    result$summary,
    c("mean", "var", "var_mean_ratio", "n_zero", "prop_zero", "n_total")
  )
})

test_that("offset formulas include exposure-aware summary columns", {
  result <- summarizeCountData(y ~ x1 + offset(log(exposure)), df_offset)
  expect_true(all(c("mean_exposure", "min_exposure", "max_exposure", "mean_rate") %in%
                    names(result$summary)))
  expect_equal(result$summary$mean_exposure, mean(df_offset$exposure))
  expect_equal(result$summary$min_exposure, min(df_offset$exposure))
  expect_equal(result$summary$max_exposure, max(df_offset$exposure))
  expect_equal(result$summary$mean_rate, mean(df_offset$y / df_offset$exposure))
})

test_that("summary table values are correct", {
  result <- summarizeCountData(y ~ x1, df_small)
  expect_equal(result$summary$mean,           mean(df_small$y))
  expect_equal(result$summary$var,            var(df_small$y))
  expect_equal(result$summary$var_mean_ratio, var(df_small$y) / mean(df_small$y))
  expect_equal(result$summary$n_zero,         sum(df_small$y == 0))
  expect_equal(result$summary$prop_zero,      sum(df_small$y == 0) / nrow(df_small))
  expect_equal(result$summary$n_total,        nrow(df_small))
})

test_that("counts table has correct columns and types", {
  result <- summarizeCountData(y ~ x1, df_small)
  expect_named(result$counts, c("count", "freq"))
  expect_type(result$counts$count, "integer")
})

test_that("counts table covers all observed values", {
  result <- summarizeCountData(y ~ x1, df_small)
  expect_setequal(result$counts$count, unique(df_small$y))
})

# ---------------------------------------------------------------------------
# Main plot output
# ---------------------------------------------------------------------------

test_that("no predictors produces a ggplot", {
  result <- summarizeCountData(y ~ 1, df_small)
  expect_s3_class(result$plot, "ggplot")
})

test_that("one continuous predictor produces a ggplot", {
  result <- summarizeCountData(y ~ x1, df_small)
  expect_s3_class(result$plot, "ggplot")
})

test_that("one categorical predictor produces a ggplot", {
  result <- summarizeCountData(y ~ x2, df_small)
  expect_s3_class(result$plot, "ggplot")
})

test_that("two continuous predictors produces a ggplot", {
  result <- summarizeCountData(y ~ x1 + x3, df_small)
  expect_s3_class(result$plot, "ggplot")
})

test_that("two categorical predictors produces a ggplot", {
  df2      <- df_small
  df2$x3   <- sample(c("C", "D"), 10, replace = TRUE)
  result   <- summarizeCountData(y ~ x2 + x3, df2)
  expect_s3_class(result$plot, "ggplot")
})

test_that("mixed (continuous + categorical) predictors produces a ggplot", {
  result <- summarizeCountData(y ~ x1 + x2, df_small)
  expect_s3_class(result$plot, "ggplot")
})

test_that("3+ predictors warns and still returns a ggplot", {
  expect_warning(
    result <- summarizeCountData(y ~ x1 + x2 + x3, df_small),
    "More than 2 predictors"
  )
  expect_s3_class(result$plot, "ggplot")
})

# ---------------------------------------------------------------------------
# pairs_plot output
# ---------------------------------------------------------------------------

test_that("pairs_plot is a ggmatrix (GGally) object", {
  result <- summarizeCountData(y ~ x1, df_small)
  expect_s3_class(result$pairs_plot, "ggmatrix")
})

test_that("pairs_plot works with no predictors", {
  result <- summarizeCountData(y ~ 1, df_small)
  expect_s3_class(result$pairs_plot, "ggmatrix")
})

test_that("pairs_plot works with one continuous predictor", {
  result <- summarizeCountData(y ~ x1, df_small)
  expect_s3_class(result$pairs_plot, "ggmatrix")
})

test_that("pairs_plot works with one categorical predictor", {
  result <- summarizeCountData(y ~ x2, df_small)
  expect_s3_class(result$pairs_plot, "ggmatrix")
})

test_that("pairs_plot works with two continuous predictors", {
  result <- summarizeCountData(y ~ x1 + x3, df_small)
  expect_s3_class(result$pairs_plot, "ggmatrix")
})

test_that("pairs_plot works with two categorical predictors", {
  df2    <- df_small
  df2$x3 <- sample(c("C", "D"), 10, replace = TRUE)
  result <- summarizeCountData(y ~ x2 + x3, df2)
  expect_s3_class(result$pairs_plot, "ggmatrix")
})

test_that("pairs_plot works with mixed predictors", {
  result <- summarizeCountData(y ~ x1 + x2, df_small)
  expect_s3_class(result$pairs_plot, "ggmatrix")
})

test_that("pairs_plot includes all predictors for 3+ predictor formulas", {
  expect_warning(
    result <- summarizeCountData(y ~ x1 + x2 + x3, df_small),
    "More than 2 predictors"
  )
  expect_s3_class(result$pairs_plot, "ggmatrix")
  # 4 variables: y, x1, x2, x3 → 4x4 matrix
  expect_equal(result$pairs_plot$nrow, 4L)
  expect_equal(result$pairs_plot$ncol, 4L)
})

test_that("pairs_plot includes all 5 variables for 4-predictor formula", {
  expect_warning(
    result <- summarizeCountData(y ~ x1 + x2 + x3 + x4, df_large),
    "More than 2 predictors"
  )
  expect_s3_class(result$pairs_plot, "ggmatrix")
  expect_equal(result$pairs_plot$nrow, 5L)
  expect_equal(result$pairs_plot$ncol, 5L)
})

# ---------------------------------------------------------------------------
# Response treated as factor vs numeric in pairs_plot
# ---------------------------------------------------------------------------

test_that("response with <=10 unique values is factor in pairs_plot data", {
  # df_small has 7 unique values (0,1,2,3,4,5) — should be factor
  result    <- summarizeCountData(y ~ x1, df_small)
  pairs_data <- result$pairs_plot$data
  expect_s3_class(pairs_data[[1]], "factor")
})

test_that("response with >10 unique values is numeric in pairs_plot data", {
  # df_large is drawn from Pois(10) so will have >10 unique values
  result     <- summarizeCountData(y ~ x1, df_large)
  pairs_data <- result$pairs_plot$data
  expect_true(is.numeric(pairs_data[[1]]))
})

# ---------------------------------------------------------------------------
# NAs in predictors
# ---------------------------------------------------------------------------

test_that("handles NAs in continuous predictor without error", {
  # model.frame drops rows with NA by default
  expect_no_error(summarizeCountData(y ~ x1, df_na))
})

test_that("handles NAs in categorical predictor without error", {
  expect_no_error(summarizeCountData(y ~ x2, df_na))
})

test_that("handles NAs in both predictors without error", {
  expect_no_error(summarizeCountData(y ~ x1 + x2, df_na))
})

test_that("summary n_total reflects rows after NA removal", {
  result <- summarizeCountData(y ~ x1, df_na)
  # x1 has 2 NAs, so 8 complete rows
  expect_equal(result$summary$n_total, 8L)
})

test_that("pairs_plot works with NA-containing data", {
  result <- summarizeCountData(y ~ x1, df_na)
  expect_s3_class(result$pairs_plot, "ggmatrix")
})

# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------

test_that("all-zero response works", {
  result <- summarizeCountData(y ~ x1, df_zeros)
  expect_equal(result$summary$n_zero, 10L)
  expect_equal(result$summary$mean, 0)
  expect_s3_class(result$plot, "ggplot")
  expect_s3_class(result$pairs_plot, "ggmatrix")
})

test_that("single observation raises no error", {
  df_one <- data.frame(y = 2L, x1 = 0.5)
  expect_no_error(summarizeCountData(y ~ x1, df_one))
})

test_that("large count values are handled correctly", {
  df_big <- data.frame(y = as.integer(c(0, 1000, 500, 2000, 750)), x1 = rnorm(5))
  result  <- summarizeCountData(y ~ x1, df_big)
  expect_equal(result$summary$mean, mean(df_big$y))
  expect_s3_class(result$pairs_plot, "ggmatrix")
})
