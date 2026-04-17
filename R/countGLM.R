#' Fit and compare count regression models
#'
#' Fits three base count regression models (Poisson, negative binomial, and
#' Tweedie), runs a DHARMa zero-inflation test on each, fits the corresponding
#' zero-inflated counterpart for any model where zero-inflation is detected,
#' then selects the best overall model by the metric given in `decide`.
#'
#' @param formula A model formula for the count component (e.g. `y ~ x1 + x2`).
#'   The response must be non-negative.
#' @param data A data frame containing the variables in `formula` (and
#'   `ziformula` if provided).
#' @param ziformula A one-sided formula for the zero-inflation component passed
#'   to [zeroinflPoissonGLM()], [zeroinflNegbinGLM()], and
#'   [zeroinflTweedieGLM()] when they are needed. When `NULL` (default), the
#'   same right-hand side as `formula` is used.
#' @param decide Character string specifying the model-selection criterion.
#'   One of `"BIC"` (default), `"AIC"`, `"LogLik"` (log-likelihood, higher
#'   is better), or `"McFadden"` (McFadden pseudo-R², higher is better).
#'   Matching is case-insensitive.
#' @param ... Additional arguments passed to each individual model fitter.
#'
#' @return An object of class `"countGLM"`, a list with:
#'   \describe{
#'     \item{`call`}{The matched call.}
#'     \item{`fits`}{A named list of successfully fitted model objects. Base
#'       models (`poisson`, `negbin`, `tweedie`) are always attempted.
#'       `zeroinfl_poisson` and `zeroinfl_negbin` are fitted only when the
#'       DHARMa zero-inflation test flags their base model (p < 0.05).
#'       `zeroinfl_tweedie` is **always** fitted alongside `tweedie` because
#'       glmmTMB's flexible dispersion makes the DHARMa test unreliable for
#'       that family. Any model that failed to converge is omitted. Base model
#'       fits include `diagnostics$zi_test` populated from the DHARMa test.}
#'     \item{`aic_table`}{A named numeric vector of AICs, sorted ascending.}
#'     \item{`bic_table`}{A named numeric vector of BICs, sorted ascending.}
#'     \item{`metric_table`}{A named numeric vector of the selection metric
#'       values (the criterion named by `decide`), sorted best-first.}
#'     \item{`decide`}{The normalised (lower-case) name of the selection
#'       criterion actually used.}
#'     \item{`best_model`}{Character name of the model selected by `decide`.}
#'     \item{`recommendation`}{A plain-language character string explaining
#'       the selection, including the criterion value, dispersion context, and
#'       zero-inflation test results.}
#'     \item{`vif`}{Named numeric vector of Variance Inflation Factors for the
#'       main-effect predictors in `formula` (interaction and polynomial terms
#'       are excluded). `NULL` when fewer than two main-effect predictors are
#'       present. A warning is issued for any VIF > 5.}
#'   }
#'
#' @details
#' **Workflow:** [countGLM()] fits Poisson, negative binomial, and Tweedie
#' base models. It then runs a DHARMa simulation test for zero-inflation on
#' each successful base model. For Poisson and negative binomial, the
#' zero-inflated counterpart is fitted only when zero-inflation is detected
#' (p < 0.05). For Tweedie, the zero-inflated counterpart is **always** fitted
#' alongside the base model: glmmTMB can absorb excess zeros by inflating the
#' dispersion parameter `phi`, which makes the DHARMa ZI test unreliable for
#' this family. All surviving models are compared by `decide`.
#'
#' **Model selection:** The model with the best value of `decide` is chosen.
#' For `"AIC"` and `"BIC"` the model with the *lowest* value wins; for
#' `"LogLik"` and `"McFadden"` the model with the *highest* value wins.
#' AIC and BIC are always computed and displayed regardless of `decide`.
#' When `decide = "McFadden"`, intercept-only null models are fitted for each
#' family to compute the pseudo-R².
#'
#' Individual models support `print()`, `summary()`, and `plot()`.
#'
#' @examples
#' df <- data.frame(
#'   y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
#'   x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
#' )
#' result <- suppressWarnings(countGLM(y ~ x1, data = df))      # default: BIC
#' result <- suppressWarnings(countGLM(y ~ x1, data = df, decide = "AIC"))
#' result <- suppressWarnings(countGLM(y ~ x1, data = df, decide = "McFadden"))
#' print(result)
#' summary(result)
#'
#' @seealso [poissonGLM()], [negbinGLM()], [tweedieGLM()],
#'   [zeroinflPoissonGLM()], [zeroinflNegbinGLM()], [zeroinflTweedieGLM()]
#' @export
countGLM <- function(formula, data, ziformula = NULL, decide = "BIC", ...) {
  stopifnot(
    "formula must be a formula object"    = inherits(formula, "formula"),
    "data must be a data frame"           = is.data.frame(data),
    "ziformula must be a formula or NULL" =
      is.null(ziformula) || inherits(ziformula, "formula")
  )

  decide_norm <- tolower(trimws(decide))
  if (!decide_norm %in% c("aic", "bic", "loglik", "mcfadden")) {
    stop(sprintf(
      '`decide` must be one of "AIC", "BIC", "LogLik", or "McFadden" (got "%s").',
      decide
    ))
  }

  # Report any rows that will be silently dropped due to missing values
  check_na_rows(formula, data, ziformula)

  # Detect integer-valued response: used to identify degenerate Tweedie fits
  # where glmmTMB pushes p to the boundary on integer count data.
  integer_response <- tryCatch({
    y_vals <- model.response(model.frame(formula, data))
    isTRUE(all(is.finite(y_vals) & y_vals == floor(y_vals)))
  }, error = function(e) FALSE)

  # VIF on main-effect terms only (avoids false positives from interaction terms)
  vif <- check_vif(formula, data)

  # ---------------------------------------------------------------------------
  # Step 1: Fit the three base models (ZI assessment handled below)
  # ---------------------------------------------------------------------------
  base_fits <- list(
    poisson = tryCatch(
      poissonGLM(formula, data, assessZeroInflation = FALSE, ...),
      error = function(e) e
    ),
    negbin = tryCatch(
      negbinGLM(formula, data, assessZeroInflation = FALSE, ...),
      error = function(e) e
    ),
    tweedie = tryCatch(
      tweedieGLM(formula, data, assessZeroInflation = FALSE, ...),
      error = function(e) e
    )
  )

  base_ok <- !vapply(base_fits, inherits, logical(1L), "error")

  # Exclude degenerate Tweedie fits on integer count data.
  # glmmTMB pushes the power parameter p toward the Poisson boundary (p = 1)
  # when fitting integer counts; the continuous Tweedie density artificially
  # maximises there.  Such fits are not meaningfully different from Poisson and
  # should not participate in model comparison.
  if (base_ok[["tweedie"]] && integer_response) {
    tw_p <- base_fits[["tweedie"]]$p
    if (!is.na(tw_p) && (tw_p <= 1.01 || tw_p >= 1.99)) {
      warning(
        paste0(
          sprintf("Tweedie fit converged to a degenerate power parameter (p = %.4f, ", tw_p),
          "at the boundary of (1, 2)). This typically occurs when glmmTMB is ",
          "applied to integer count data; the continuous Tweedie density ",
          "collapses toward the Poisson boundary. Tweedie (and its ",
          "zero-inflated counterpart) will be excluded from model comparison."
        ),
        call. = FALSE
      )
      base_ok[["tweedie"]] <- FALSE
    }
  }

  if (!any(base_ok)) {
    stop("All base model fits (Poisson, Negative Binomial, Tweedie) failed. Check your formula and data.")
  }

  # ---------------------------------------------------------------------------
  # Step 2: Run DHARMa zero-inflation test on each successful base model
  # ---------------------------------------------------------------------------
  base_labels <- list(
    poisson = "Poisson",
    negbin  = "Negative Binomial",
    tweedie = "Tweedie"
  )
  for (nm in names(base_fits)[base_ok]) {
    zi <- tryCatch(
      run_dharma_zi_test(
        base_fits[[nm]]$model,
        model_type = base_labels[[nm]]
      ),
      error = function(e) NULL
    )
    base_fits[[nm]]$diagnostics$zi_test <- zi
  }

  # ---------------------------------------------------------------------------
  # Step 3: Conditionally fit zero-inflated counterparts
  # ---------------------------------------------------------------------------
  zi_map <- list(
    poisson = "zeroinfl_poisson",
    negbin  = "zeroinfl_negbin",
    tweedie = "zeroinfl_tweedie"
  )
  zi_fitters <- list(
    zeroinfl_poisson = function() zeroinflPoissonGLM(formula, data, ziformula = ziformula, ...),
    zeroinfl_negbin  = function() zeroinflNegbinGLM(formula, data, ziformula = ziformula, ...),
    zeroinfl_tweedie = function() zeroinflTweedieGLM(formula, data, ziformula = ziformula, ...)
  )

  zi_fits <- list()
  for (nm in names(base_fits)[base_ok]) {
    zi_result <- base_fits[[nm]]$diagnostics$zi_test

    # Tweedie: always fit the ZI counterpart. glmmTMB can absorb excess zeros
    # by inflating phi, making the DHARMa ZI test unreliable for this family.
    # Poisson/NegBin: gate on DHARMa detection as usual.
    should_fit_zi <- (nm == "tweedie") || isTRUE(zi_result$detected)

    if (should_fit_zi) {
      zi_nm  <- zi_map[[nm]]
      result <- tryCatch(zi_fitters[[zi_nm]](), error = function(e) e)
      if (!inherits(result, "error")) {
        zi_fits[[zi_nm]] <- result
      }
    }
  }

  # Combine base and ZI fits; preserve canonical ordering
  all_fits_raw <- c(base_fits[base_ok], zi_fits)
  ordered_names <- c("poisson", "negbin", "tweedie",
                     "zeroinfl_poisson", "zeroinfl_negbin", "zeroinfl_tweedie")
  fits <- all_fits_raw[intersect(ordered_names, names(all_fits_raw))]

  if (length(fits) == 0L) {
    stop("All model fits failed. Check your formula and data.")
  }

  aics      <- vapply(fits, `[[`, numeric(1L), "aic")
  bics      <- vapply(fits, `[[`, numeric(1L), "bic")
  aic_table <- sort(aics)
  bic_table <- sort(bics)

  # Compute selection metric and pick best model
  if (decide_norm == "aic") {
    raw_metrics  <- aics
    best_name    <- names(which.min(raw_metrics))
    metric_table <- sort(raw_metrics)
  } else if (decide_norm == "bic") {
    raw_metrics  <- bics
    best_name    <- names(which.min(raw_metrics))
    metric_table <- sort(raw_metrics)
  } else if (decide_norm == "loglik") {
    raw_metrics  <- vapply(fits, function(f) as.numeric(stats::logLik(f$model)), numeric(1L))
    best_name    <- names(which.max(raw_metrics))
    metric_table <- sort(raw_metrics, decreasing = TRUE)
  } else {
    # McFadden pseudo-R²: 1 - logLik(full) / logLik(null)
    raw_metrics  <- .compute_mcfadden(fits, data, deparse(formula[[2L]]))
    valid        <- !is.na(raw_metrics)
    best_name    <- names(which.max(raw_metrics[valid]))
    metric_table <- sort(raw_metrics, decreasing = TRUE)
  }

  recommendation <- build_recommendation(fits, best_name, aic_table, bic_table,
                                         metric_table, decide_norm)

  structure(
    list(
      call           = match.call(),
      fits           = fits,
      aic_table      = aic_table,
      bic_table      = bic_table,
      metric_table   = metric_table,
      decide         = decide_norm,
      best_model     = best_name,
      recommendation = recommendation,
      vif            = vif
    ),
    class = "countGLM"
  )
}

# ---------------------------------------------------------------------------
# Internal: compute McFadden pseudo-R² for each surviving fit
# ---------------------------------------------------------------------------

.compute_mcfadden <- function(fits, data, y_var) {
  null_str <- paste(y_var, "~ 1")
  vapply(names(fits), function(nm) {
    f       <- fits[[nm]]
    ll_full <- tryCatch(as.numeric(stats::logLik(f$model)), error = function(e) NA_real_)
    if (is.na(ll_full)) return(NA_real_)

    cls      <- class(f)[1L]
    null_fit <- tryCatch(
      switch(cls,
        poissonGLM = stats::glm(
          stats::as.formula(null_str), data = data, family = stats::poisson()
        ),
        negbinGLM = MASS::glm.nb(
          stats::as.formula(null_str), data = data
        ),
        tweedieGLM = glmmTMB::glmmTMB(
          stats::as.formula(null_str), data = data,
          family = glmmTMB::tweedie(link = "log")
        ),
        zeroinflPoissonGLM = pscl::zeroinfl(
          stats::as.formula(paste(y_var, "~ 1 | 1")), data = data, dist = "poisson"
        ),
        zeroinflNegbinGLM = pscl::zeroinfl(
          stats::as.formula(paste(y_var, "~ 1 | 1")), data = data, dist = "negbin"
        ),
        zeroinflTweedieGLM = glmmTMB::glmmTMB(
          stats::as.formula(null_str), data = data,
          family    = glmmTMB::tweedie(link = "log"),
          ziformula = ~ 1
        )
      ),
      error = function(e) NULL
    )
    if (is.null(null_fit)) return(NA_real_)

    ll_null <- tryCatch(as.numeric(stats::logLik(null_fit)), error = function(e) NA_real_)
    if (is.na(ll_null) || ll_null == 0) return(NA_real_)
    1 - ll_full / ll_null
  }, numeric(1L))
}

# ---------------------------------------------------------------------------
# Internal: build plain-language recommendation string
# ---------------------------------------------------------------------------

.model_label <- function(name) {
  switch(name,
    poisson          = "Poisson",
    negbin           = "Negative Binomial",
    tweedie          = "Tweedie",
    zeroinfl_poisson = "Zero-Inflated Poisson",
    zeroinfl_negbin  = "Zero-Inflated Negative Binomial",
    zeroinfl_tweedie = "Zero-Inflated Tweedie",
    name
  )
}

build_recommendation <- function(fits, best_name, aic_table, bic_table,
                                  metric_table, decide) {
  disp_msg <- zi_msg <- ""

  # --- Overdispersion: use Poisson dispersion ratio ---
  pois_fit <- fits[["poisson"]]
  if (!is.null(pois_fit)) {
    disp_ratio <- pois_fit$diagnostics$dispersion_ratio
    if (!is.na(disp_ratio)) {
      disp_msg <- if (disp_ratio > 1.5) {
        sprintf(
          "The Poisson dispersion ratio is %.2f (> 1.5), indicating overdispersion.",
          disp_ratio
        )
      } else {
        sprintf(
          "The Poisson dispersion ratio is %.2f, consistent with equidispersion.",
          disp_ratio
        )
      }
    }
  }

  # --- Zero-inflation: summarise DHARMa results across base models ---
  zi_detected_labels <- character(0L)
  zi_p_parts         <- character(0L)

  for (nm in c("poisson", "negbin")) {
    fit_nm <- fits[[nm]]
    if (!is.null(fit_nm)) {
      zi_res <- fit_nm$diagnostics$zi_test
      if (!is.null(zi_res) && !is.na(zi_res$p_value)) {
        zi_p_parts <- c(
          zi_p_parts,
          sprintf("%s p = %.3f", .model_label(nm), zi_res$p_value)
        )
        if (isTRUE(zi_res$detected)) {
          zi_detected_labels <- c(zi_detected_labels, .model_label(nm))
        }
      }
    }
  }

  # Tweedie ZI is always fitted; report its DHARMa result only informally.
  tw_zi_note <- ""
  if (!is.null(fits[["tweedie"]])) {
    zi_res <- fits[["tweedie"]]$diagnostics$zi_test
    if (!is.null(zi_res) && !is.na(zi_res$p_value)) {
      tw_zi_note <- sprintf(
        "Zero-Inflated Tweedie was fitted alongside Tweedie (DHARMa p = %.3f; ZI test unreliable for this family).",
        zi_res$p_value
      )
    } else {
      tw_zi_note <- "Zero-Inflated Tweedie was fitted alongside Tweedie."
    }
  }

  if (length(zi_detected_labels) > 0L) {
    zi_msg <- sprintf(
      "Zero-inflation detected for %s; corresponding ZI model(s) were fitted.",
      paste(zi_detected_labels, collapse = " and ")
    )
  } else if (length(zi_p_parts) > 0L) {
    zi_msg <- "No significant zero-inflation detected for Poisson or Negative Binomial."
  }

  # --- Selection message ---
  metric_label <- switch(decide,
    aic      = "AIC",
    bic      = "BIC",
    loglik   = "log-likelihood",
    mcfadden = "McFadden R\u00b2"
  )
  val     <- metric_table[best_name]
  fmt     <- if (decide == "mcfadden") "%.4f" else "%.2f"
  val_str <- if (is.na(val)) "NA" else sprintf(fmt, val)
  selection_msg <- sprintf(
    "%s was selected by %s (%s = %s).",
    .model_label(best_name), metric_label, metric_label, val_str
  )

  parts <- c(selection_msg, disp_msg, zi_msg, tw_zi_note)
  parts <- parts[nchar(parts) > 0L]
  paste(parts, collapse = " ")
}
