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

# Shared test data — low-count Poisson-ish data (few/no structural zeros)
df_cglm <- data.frame(
  y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
  x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
)

# Fit once; countGLM fits base models and possibly ZI counterparts —
# small datasets often hit iteration limits — suppress expected warnings
result <- suppressWarnings(countGLM(y ~ x1, data = df_cglm))

# All six valid model names (base + ZI counterparts)
valid_models <- c("poisson", "negbin", "tweedie",
                  "zeroinfl_poisson", "zeroinfl_negbin", "zeroinfl_tweedie")

test_that("countGLM returns class 'countGLM'", {
  expect_s3_class(result, "countGLM")
})

test_that("countGLM returns correct slot names", {
  expect_named(result, c("call", "fits", "aic_table", "bic_table",
                         "metric_table", "decide", "best_model", "recommendation",
                         "vif"))
})

test_that("countGLM best_model is one of the valid model names", {
  expect_true(result$best_model %in% valid_models)
})

test_that("countGLM aic_table is a named numeric vector of length <= 6", {
  expect_type(result$aic_table, "double")
  expect_lte(length(result$aic_table), 6L)
  expect_false(is.null(names(result$aic_table)))
})

test_that("countGLM bic_table is a named numeric vector of length <= 6", {
  expect_type(result$bic_table, "double")
  expect_lte(length(result$bic_table), 6L)
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

test_that("countGLM tweedie fit (if present) is a tweedieGLM", {
  tw <- result$fits[["tweedie"]]
  if (!is.null(tw)) {
    expect_s3_class(tw, "tweedieGLM")
    expect_s3_class(tw, "countGLMfit")
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

test_that("countGLM poisson fit has zi_test populated", {
  pois <- result$fits[["poisson"]]
  if (!is.null(pois)) {
    expect_type(pois$diagnostics$zi_test, "list")
    expect_named(pois$diagnostics$zi_test, c("detected", "p_value", "plot"))
  }
})

test_that("countGLM negbin fit has zi_test populated", {
  nb <- result$fits[["negbin"]]
  if (!is.null(nb)) {
    expect_type(nb$diagnostics$zi_test, "list")
    expect_named(nb$diagnostics$zi_test, c("detected", "p_value", "plot"))
  }
})

test_that("countGLM tweedie fit has zi_test populated", {
  tw <- result$fits[["tweedie"]]
  if (!is.null(tw)) {
    expect_type(tw$diagnostics$zi_test, "list")
    expect_named(tw$diagnostics$zi_test, c("detected", "p_value", "plot"))
  }
})

test_that("countGLM ZI models are only fitted when ZI is detected", {
  # For each ZI model present, the corresponding base model must have detected ZI
  zi_base_map <- c(
    zeroinfl_poisson = "poisson",
    zeroinfl_negbin  = "negbin",
    zeroinfl_tweedie = "tweedie"
  )
  for (zi_nm in names(zi_base_map)) {
    if (!is.null(result$fits[[zi_nm]])) {
      base_nm <- zi_base_map[[zi_nm]]
      base_zi <- result$fits[[base_nm]]$diagnostics$zi_test
      expect_true(
        isTRUE(base_zi$detected),
        label = paste(zi_nm, "should only appear when", base_nm, "detects ZI")
      )
    }
  }
})

# --- decide parameter tests ---

test_that("countGLM default decide is 'bic'", {
  expect_equal(result$decide, "bic")
})

test_that("countGLM metric_table is a named numeric vector", {
  expect_type(result$metric_table, "double")
  expect_false(is.null(names(result$metric_table)))
  expect_lte(length(result$metric_table), 6L)
})

test_that("countGLM decide = 'AIC' is accepted case-insensitively", {
  r <- suppressWarnings(countGLM(y ~ x1, data = df_cglm, decide = "AIC"))
  expect_equal(r$decide, "aic")
  expect_true(r$best_model %in% valid_models)
})

test_that("countGLM decide = 'loglik' selects a valid model", {
  r <- suppressWarnings(countGLM(y ~ x1, data = df_cglm, decide = "loglik"))
  expect_equal(r$decide, "loglik")
  expect_true(r$best_model %in% valid_models)
  # metric_table should be sorted descending (best = highest loglik first)
  mt <- r$metric_table
  expect_true(all(diff(mt) <= 0))
})

test_that("countGLM decide = 'McFadden' selects a valid model", {
  r <- suppressWarnings(countGLM(y ~ x1, data = df_cglm, decide = "McFadden"))
  expect_equal(r$decide, "mcfadden")
  expect_true(r$best_model %in% valid_models)
})

test_that("countGLM errors on invalid decide value", {
  expect_error(
    countGLM(y ~ x1, data = df_cglm, decide = "WAIC"),
    "`decide` must be one of"
  )
})

# --- ZI model conditional fitting test with heavily zero-inflated data ---

test_that("countGLM fits ZI models when ZI is detected in base models", {
  set.seed(99)
  n <- 40L
  df_zi <- data.frame(
    y  = c(rep(0L, 28L), rpois(12L, lambda = 4L)),
    x1 = rnorm(n)
  )
  r <- suppressWarnings(countGLM(y ~ x1, data = df_zi))

  # At least one base model should detect ZI for this heavily zero-inflated data
  base_zi_detected <- any(vapply(
    c("poisson", "negbin", "tweedie"),
    function(nm) {
      fit_nm <- r$fits[[nm]]
      !is.null(fit_nm) && isTRUE(fit_nm$diagnostics$zi_test$detected)
    },
    logical(1L)
  ))

  # If ZI was detected in any base model, at least one ZI counterpart should be fitted
  zi_models_fitted <- intersect(
    names(r$fits),
    c("zeroinfl_poisson", "zeroinfl_negbin", "zeroinfl_tweedie")
  )

  if (base_zi_detected) {
    expect_gt(length(zi_models_fitted), 0L)
  }

  # All fitted ZI models should inherit zeroinflGLMfit
  for (nm in zi_models_fitted) {
    expect_true(inherits(r$fits[[nm]], "zeroinflGLMfit"))
  }
})

# --- VIF tests ---

test_that("countGLM vif is NULL with a single predictor", {
  r <- suppressWarnings(countGLM(y ~ x1, data = df_cglm))
  expect_null(r$vif)
})

test_that("countGLM vif is a named numeric vector with two predictors", {
  df2 <- data.frame(
    y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
    x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7),
    x2 = c(0.1,  0.9, 0.4,  0.7, 0.2, 0.8,  0.5, 0.3,  0.6, 0.0)
  )
  r <- suppressWarnings(countGLM(y ~ x1 + x2, data = df2))
  expect_type(r$vif, "double")
  expect_named(r$vif)
  expect_length(r$vif, 2L)
  expect_true(all(r$vif >= 1))
})

test_that("countGLM vif excludes interaction terms from computation", {
  df2 <- data.frame(
    y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
    x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7),
    x2 = c(0.1,  0.9, 0.4,  0.7, 0.2, 0.8,  0.5, 0.3,  0.6, 0.0)
  )
  # VIF on x1 * x2 should still yield 2 entries (one per main effect), not 3
  r <- suppressWarnings(countGLM(y ~ x1 * x2, data = df2))
  expect_length(r$vif, 2L)
  expect_true(all(c("x1", "x2") %in% names(r$vif)))
})

test_that("countGLM vif warns when a predictor exceeds threshold of 5", {
  set.seed(42)
  n  <- 40L
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n, sd = 0.05)   # near-collinear
  df_mc <- data.frame(
    y  = rpois(n, lambda = exp(0.5 + 0.3 * x1)),
    x1 = x1,
    x2 = x2
  )
  expect_warning(
    countGLM(y ~ x1 + x2, data = df_mc),
    "High VIF detected"
  )
})

# --- Missing-value (NA) reporting tests ---

test_that("countGLM warns when predictor rows contain NAs", {
  df_na <- df_cglm
  df_na$x1[c(2L, 5L)] <- NA
  expect_warning(
    countGLM(y ~ x1, data = df_na),
    "2 row\\(s\\) contain missing values"
  )
})

test_that("countGLM warns when response rows contain NAs", {
  df_na <- df_cglm
  df_na$y[3L] <- NA
  expect_warning(
    countGLM(y ~ x1, data = df_na),
    "1 row\\(s\\) contain missing values"
  )
})

# --- Factor levels with non-standard characters ---

test_that("countGLM handles factors with special-character levels", {
  df_fac <- data.frame(
    y    = c(0L, 1L, 2L, 0L, 3L, 1L, 0L, 2L, 1L, 0L),
    x1   = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7),
    zone = factor(
      c("A/B", "(ctrl)", "A/B", "mixed use", "(ctrl)",
        "A/B", "mixed use", "(ctrl)", "mixed use", "A/B"),
      levels = c("A/B", "(ctrl)", "mixed use")
    )
  )
  expect_no_error(suppressWarnings(countGLM(y ~ x1 + zone, data = df_fac)))
})

test_that("countGLM vif handles backtick-quoted column names", {
  df_bt <- data.frame(
    y         = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
    `my var`  = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7),
    `x (2)`   = c(0.1,  0.9, 0.4,  0.7, 0.2, 0.8,  0.5, 0.3,  0.6, 0.0),
    check.names = FALSE
  )
  r <- suppressWarnings(countGLM(y ~ `my var` + `x (2)`, data = df_bt))
  expect_type(r$vif, "double")
  expect_length(r$vif, 2L)
})

# --- Degenerate Tweedie exclusion test ---

test_that("countGLM warns and excludes degenerate Tweedie on integer data", {
  # Collect all warnings during the countGLM run
  warns <- character(0L)
  r <- withCallingHandlers(
    countGLM(y ~ x1, data = df_cglm),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  # When Tweedie p collapses to the boundary, a degenerate warning fires and
  # Tweedie is removed from the comparison set
  if (any(grepl("degenerate|boundary", warns))) {
    expect_false("tweedie" %in% names(r$fits))
    expect_false("zeroinfl_tweedie" %in% names(r$fits))
    expect_true(r$best_model %in% c("poisson", "negbin",
                                    "zeroinfl_poisson", "zeroinfl_negbin"))
  }
})
