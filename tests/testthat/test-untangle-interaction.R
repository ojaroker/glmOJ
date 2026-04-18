skip_if_not_installed("emmeans")

set.seed(42)
n <- 120
df_un <- data.frame(
  x1    = rnorm(n),
  x2    = rnorm(n),
  cat_a = factor(sample(c("A", "B", "C"), n, TRUE)),
  cat_b = factor(sample(c("L", "H"), n, TRUE)),
  other = factor(sample(c("p", "q"), n, TRUE))
)
df_un$y <- rpois(
  n,
  exp(0.6 + 0.3 * df_un$x1 - 0.2 * df_un$x2 + 0.4 * df_un$x1 * df_un$x2 +
        ifelse(df_un$cat_a == "B", 0.4, 0))
)

test_that("untangle_interaction errors on unknown variables", {
  fit <- suppressWarnings(poissonGLM(y ~ x1 * x2, data = df_un))
  expect_error(
    untangle_interaction(fit, c("x1", "nope")),
    "Variable\\(s\\) not found"
  )
})

test_that("untangle_interaction errors when interaction is not length 2", {
  fit <- suppressWarnings(poissonGLM(y ~ x1 * x2, data = df_un))
  expect_error(
    untangle_interaction(fit, c("x1", "x2", "cat_a")),
    "exactly two variables"
  )
})

test_that("categorical x categorical returns renamed emmeans data frame and a plot", {
  fit <- suppressWarnings(poissonGLM(y ~ cat_a * cat_b + x1 + other, data = df_un))
  res <- suppressMessages(untangle_interaction(fit, c("cat_a", "cat_b")))
  expect_s3_class(res, "untangle_interaction")
  expect_equal(res$type, "categorical-categorical")
  expect_true(all(c("cat_a", "cat_b", "Mean", "Lower", "Upper") %in% names(res$emmeans)))
  expect_true("other" %in% names(res$emmeans))
  expect_s3_class(res$plot, "ggplot")
})

test_that("average.over drops a categorical column from the grid", {
  fit <- suppressWarnings(poissonGLM(y ~ cat_a * cat_b + x1 + other, data = df_un))
  res <- suppressMessages(
    untangle_interaction(fit, c("cat_a", "cat_b"), average.over = "other")
  )
  expect_false("other" %in% names(res$emmeans))
  expect_s3_class(res$plot, "ggplot")
})

test_that("cat x cat plot is suppressed with warning when facets > 8", {
  df2 <- df_un
  df2$f1 <- factor(sample(letters[1:3], nrow(df2), TRUE))
  df2$f2 <- factor(sample(letters[1:3], nrow(df2), TRUE))
  df2$f3 <- factor(sample(letters[1:2], nrow(df2), TRUE))
  fit <- suppressWarnings(
    poissonGLM(y ~ cat_a * cat_b + f1 + f2 + f3, data = df2)
  )
  expect_warning(
    res <- untangle_interaction(fit, c("cat_a", "cat_b")),
    "panels|exceed the limit of 8|> 8"
  )
  expect_null(res$plot)

  res_ok <- suppressMessages(
    untangle_interaction(fit, c("cat_a", "cat_b"), overridePlot = TRUE)
  )
  expect_s3_class(res_ok$plot, "ggplot")
})

test_that("categorical x continuous returns emtrends and (when few facets) a ggplot", {
  fit <- suppressWarnings(poissonGLM(y ~ cat_a * x1 + x2 + cat_b, data = df_un))
  res <- suppressMessages(untangle_interaction(fit, c("cat_a", "x1"), n = 50))
  expect_equal(res$type, "categorical-continuous")
  expect_true(all(c("cat_a", "Slope", "Lower", "Upper") %in% names(res$emtrends)))
  expect_false("emmeans_grid" %in% names(res))
  expect_s3_class(res$plot, "ggplot")
})

test_that("categorical x continuous without facets still builds a single plot", {
  fit <- suppressWarnings(poissonGLM(y ~ cat_a * x1, data = df_un))
  res <- suppressMessages(untangle_interaction(fit, c("cat_a", "x1"), n = 30))
  expect_s3_class(res$plot, "ggplot")
})

test_that("plot is suppressed with warning when facets > 8", {
  df2 <- df_un
  df2$f1 <- factor(sample(letters[1:3], nrow(df2), TRUE))
  df2$f2 <- factor(sample(letters[1:3], nrow(df2), TRUE))
  df2$f3 <- factor(sample(letters[1:2], nrow(df2), TRUE))
  fit <- suppressWarnings(
    poissonGLM(y ~ cat_a * x1 + f1 + f2 + f3, data = df2)
  )
  expect_warning(
    res <- untangle_interaction(fit, c("cat_a", "x1")),
    "exceed the limit of 8|> 8|panels"
  )
  expect_null(res$plot)
})

test_that("overridePlot = TRUE forces the plot even past the 8-facet limit", {
  df2 <- df_un
  df2$f1 <- factor(sample(letters[1:3], nrow(df2), TRUE))
  df2$f2 <- factor(sample(letters[1:3], nrow(df2), TRUE))
  df2$f3 <- factor(sample(letters[1:2], nrow(df2), TRUE))
  fit <- suppressWarnings(
    poissonGLM(y ~ cat_a * x1 + f1 + f2 + f3, data = df2)
  )
  res <- suppressMessages(
    untangle_interaction(fit, c("cat_a", "x1"), n = 25, overridePlot = TRUE)
  )
  expect_s3_class(res$plot, "ggplot")
})

test_that("continuous x continuous returns emtrends and (if available) JN", {
  fit <- suppressWarnings(poissonGLM(y ~ x1 * x2, data = df_un))
  res <- suppressMessages(untangle_interaction(fit, c("x1", "x2")))
  expect_equal(res$type, "continuous-continuous")
  expect_true(!is.null(res$emtrends_x1))
  expect_true(!is.null(res$emtrends_x2))
  expect_true(all(c("Slope", "Lower", "Upper") %in% names(res$emtrends_x1)))
  # low/medium/high = 3 rows each
  expect_equal(nrow(res$emtrends_x1), 3L)
  expect_equal(nrow(res$emtrends_x2), 3L)
})

test_that("standardized=TRUE uses -1,0,1 as the moderator grid", {
  df_std <- df_un
  df_std$z1 <- as.numeric(scale(df_std$x1))
  df_std$z2 <- as.numeric(scale(df_std$x2))
  fit <- suppressWarnings(poissonGLM(y ~ z1 * z2, data = df_std))
  res <- suppressMessages(
    untangle_interaction(fit, c("z1", "z2"), standardized = TRUE)
  )
  # moderator values should be exactly -1, 0, 1
  expect_equal(sort(unique(res$emtrends_z1$z2)), c(-1, 0, 1))
  expect_equal(sort(unique(res$emtrends_z2$z1)), c(-1, 0, 1))
  expect_match(res$interpretation, "standard")
})

test_that("countGLM input delegates to the best component model", {
  fit <- suppressWarnings(countGLM(y ~ x1 * x2 + cat_a, data = df_un, decide = "BIC"))
  expect_message(
    untangle_interaction(fit, c("x1", "x2")),
    "Using best model"
  )
})
