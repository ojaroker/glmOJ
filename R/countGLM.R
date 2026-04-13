#' Fit and compare count regression models
#'
#' Fits all four count regression models supported by glmOJ (Poisson, negative
#' binomial, zero-inflated Poisson, zero-inflated negative binomial), selects
#' the best by the metric given in `decide`, and provides a plain-language
#' recommendation informed by dispersion and zero-inflation diagnostics.
#'
#' @param formula A model formula for the count component (e.g. `y ~ x1 + x2`).
#'   The response must be a non-negative integer count variable.
#' @param data A data frame containing the variables in `formula` (and
#'   `ziformula` if provided).
#' @param ziformula A one-sided formula for the zero-inflation component passed
#'   to [zeroinflPoissonGLM()] and [zeroinflNegbinGLM()]. When `NULL`
#'   (default), the same right-hand side as `formula` is used.
#' @param decide Character string specifying the model-selection criterion.
#'   One of `"BIC"` (default), `"AIC"`, `"LogLik"` (log-likelihood, higher
#'   is better), or `"McFadden"` (McFadden pseudo-R², higher is better).
#'   Matching is case-insensitive.
#' @param ... Additional arguments passed to each individual model fitter.
#'
#' @return An object of class `"countGLM"`, a list with:
#'   \describe{
#'     \item{`call`}{The matched call.}
#'     \item{`fits`}{A named list of successfully fitted model objects
#'       (`poisson`, `negbin`, `zeroinfl_poisson`, `zeroinfl_negbin`). Any
#'       model that failed to converge is omitted. Poisson and negative
#'       binomial fits include `diagnostics$zi_test` populated from a DHARMa
#'       zero-inflation test run internally.}
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
#'   }
#'
#' @details
#' **Model selection:** The model with the best value of `decide` is chosen.
#' For `"AIC"` and `"BIC"` the model with the *lowest* value wins; for
#' `"LogLik"` and `"McFadden"` the model with the *highest* value wins.
#' AIC and BIC are always computed and displayed regardless of `decide`.
#' When `decide = "McFadden"`, intercept-only null models are fitted for each
#' family to compute the pseudo-R².
#'
#' **Zero-inflation diagnostics:** DHARMa simulation tests are run on both
#' the Poisson and negative binomial fits. Results appear in
#' `result$fits$poisson$diagnostics$zi_test` and
#' `result$fits$negbin$diagnostics$zi_test`, and inform the recommendation
#' text. Individual model warnings are suppressed; the recommendation
#' summarises the findings instead.
#'
#' Individual models support `print()`, `summary()`, and `plot()`.
#'
#' @examples
#' df <- data.frame(
#'   y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
#'   x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
#' )
#' result <- countGLM(y ~ x1, data = df)           # default: BIC
#' result <- countGLM(y ~ x1, data = df, decide = "AIC")
#' result <- countGLM(y ~ x1, data = df, decide = "McFadden")
#' print(result)
#' summary(result)
#'
#' @seealso [poissonGLM()], [negbinGLM()], [zeroinflPoissonGLM()],
#'   [zeroinflNegbinGLM()]
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

  if (is.null(ziformula)) {
    message(
      "Note: zero-inflation component uses the same predictors as the count ",
      "component (ziformula = NULL). Use `ziformula` to specify a different formula."
    )
  }

  # Fit all four models; suppress individual ZI warnings — countGLM handles them
  fits <- list(
    poisson = tryCatch(
      poissonGLM(formula, data, assessZeroInflation = FALSE, ...),
      error = function(e) e
    ),
    negbin = tryCatch(
      negbinGLM(formula, data, assessZeroInflation = FALSE, ...),
      error = function(e) e
    ),
    zeroinfl_poisson = tryCatch(
      zeroinflPoissonGLM(formula, data, ziformula = ziformula, ...),
      error = function(e) e
    ),
    zeroinfl_negbin = tryCatch(
      zeroinflNegbinGLM(formula, data, ziformula = ziformula, ...),
      error = function(e) e
    )
  )

  # Keep only successful fits
  ok   <- !vapply(fits, inherits, logical(1), "error")
  fits <- fits[ok]

  if (length(fits) == 0L) {
    stop("All four model fits failed. Check your formula and data.")
  }

  # Run DHARMa ZI test on Poisson and NB fits and inject into diagnostics
  for (nm in c("poisson", "negbin")) {
    if (!is.null(fits[[nm]])) {
      zi <- tryCatch(
        run_dharma_zi_test(fits[[nm]]$model, model_type = if (nm == "poisson") "Poisson" else "Negative Binomial"),
        error = function(e) NULL
      )
      fits[[nm]]$diagnostics$zi_test <- zi
    }
  }

  aics      <- vapply(fits, `[[`, numeric(1), "aic")
  bics      <- vapply(fits, `[[`, numeric(1), "bic")
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
    raw_metrics  <- vapply(fits, function(f) as.numeric(stats::logLik(f$model)), numeric(1))
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
      recommendation = recommendation
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
        zeroinflPoissonGLM = pscl::zeroinfl(
          stats::as.formula(paste(y_var, "~ 1 | 1")), data = data, dist = "poisson"
        ),
        zeroinflNegbinGLM = pscl::zeroinfl(
          stats::as.formula(paste(y_var, "~ 1 | 1")), data = data, dist = "negbin"
        )
      ),
      error = function(e) NULL
    )
    if (is.null(null_fit)) return(NA_real_)

    ll_null <- tryCatch(as.numeric(stats::logLik(null_fit)), error = function(e) NA_real_)
    if (is.na(ll_null) || ll_null == 0) return(NA_real_)
    1 - ll_full / ll_null
  }, numeric(1))
}

# ---------------------------------------------------------------------------
# Internal: build plain-language recommendation string
# ---------------------------------------------------------------------------

.model_label <- function(name) {
  switch(name,
    poisson          = "Poisson",
    negbin           = "Negative Binomial",
    zeroinfl_poisson = "Zero-Inflated Poisson",
    zeroinfl_negbin  = "Zero-Inflated Negative Binomial",
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

  # --- Zero-inflation: use DHARMa results from both Poisson and NB ---
  pois_zi <- pois_fit$diagnostics$zi_test
  nb_fit   <- fits[["negbin"]]
  nb_zi    <- nb_fit$diagnostics$zi_test

  pois_detected <- isTRUE(pois_zi$detected)
  nb_detected   <- isTRUE(nb_zi$detected)

  if (pois_detected && nb_detected) {
    zi_msg <- sprintf(
      "DHARMa zero-inflation test is significant for both Poisson (p = %.3f) and Negative Binomial (p = %.3f) fits, suggesting structural excess zeros.",
      pois_zi$p_value, nb_zi$p_value
    )
  } else if (pois_detected && !nb_detected) {
    zi_msg <- sprintf(
      "DHARMa zero-inflation test is significant for the Poisson fit (p = %.3f) but not for the Negative Binomial fit (p = %.3f); excess zeros may be explained by overdispersion alone.",
      pois_zi$p_value, nb_zi$p_value
    )
  } else if (!pois_detected && !is.null(pois_zi)) {
    zi_msg <- sprintf(
      "No significant zero-inflation detected (Poisson p = %.3f%s).",
      pois_zi$p_value,
      if (!is.null(nb_zi)) sprintf(", Negative Binomial p = %.3f", nb_zi$p_value) else ""
    )
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

  parts <- c(selection_msg, disp_msg, zi_msg)
  parts <- parts[nchar(parts) > 0L]
  paste(parts, collapse = " ")
}
