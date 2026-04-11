#' Fit and compare count regression models
#'
#' Fits all four count regression models supported by glmOJ (Poisson, negative
#' binomial, zero-inflated Poisson, zero-inflated negative binomial), selects
#' the best by AIC and BIC, and provides a plain-language recommendation
#' informed by dispersion and zero-inflation diagnostics.
#'
#' @param formula A model formula for the count component (e.g. `y ~ x1 + x2`).
#'   The response must be a non-negative integer count variable.
#' @param data A data frame containing the variables in `formula` (and
#'   `ziformula` if provided).
#' @param ziformula A one-sided formula for the zero-inflation component passed
#'   to [zeroinflPoissonGLM()] and [zeroinflNegbinGLM()]. When `NULL`
#'   (default), the same right-hand side as `formula` is used.
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
#'     \item{`best_model`}{Character name of the selected model. When AIC and
#'       BIC agree this is the jointly best model; when they disagree the
#'       simpler of the two candidates is chosen.}
#'     \item{`recommendation`}{A plain-language character string explaining
#'       the selection, including dispersion, zero-inflation context, and a
#'       note when AIC and BIC point to different models.}
#'   }
#'
#' @details
#' **Model selection:** Both AIC and BIC are computed. When they agree, the
#' jointly best model is chosen. When they disagree, the simpler model
#' (Poisson < NB < ZIP < ZINB) is selected with a note.
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
#' result <- countGLM(y ~ x1, data = df)
#' print(result)
#' summary(result)
#'
#' @seealso [poissonGLM()], [negbinGLM()], [zeroinflPoissonGLM()],
#'   [zeroinflNegbinGLM()]
#' @export
countGLM <- function(formula, data, ziformula = NULL, ...) {
  stopifnot(
    "formula must be a formula object"    = inherits(formula, "formula"),
    "data must be a data frame"           = is.data.frame(data),
    "ziformula must be a formula or NULL" =
      is.null(ziformula) || inherits(ziformula, "formula")
  )

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

  best_aic <- names(which.min(aics))
  best_bic <- names(which.min(bics))

  complexity <- c(poisson = 1L, negbin = 2L,
                  zeroinfl_poisson = 3L, zeroinfl_negbin = 4L)

  if (best_aic == best_bic) {
    best_name    <- best_aic
    ic_agreement <- TRUE
  } else {
    aic_rank  <- complexity[best_aic]
    bic_rank  <- complexity[best_bic]
    best_name <- if (aic_rank <= bic_rank) best_aic else best_bic
    ic_agreement <- FALSE
  }

  recommendation <- build_recommendation(fits, best_name, aic_table, bic_table,
                                         best_aic, best_bic, ic_agreement)

  structure(
    list(
      call           = match.call(),
      fits           = fits,
      aic_table      = aic_table,
      bic_table      = bic_table,
      best_model     = best_name,
      recommendation = recommendation
    ),
    class = "countGLM"
  )
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
                                  best_aic_name, best_bic_name, ic_agreement) {
  disp_msg <- zi_msg <- ic_msg <- ""

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

  # --- IC agreement / disagreement ---
  if (ic_agreement) {
    selection_msg <- sprintf(
      "%s was selected \u2014 both AIC (%.2f) and BIC (%.2f) agree.",
      .model_label(best_name),
      aic_table[best_name],
      bic_table[best_name]
    )
  } else {
    ic_msg <- sprintf(
      "Note: AIC favors %s (AIC = %.2f) while BIC favors %s (BIC = %.2f). The simpler model (%s) was selected, but results should be interpreted with caution.",
      .model_label(best_aic_name), aic_table[best_aic_name],
      .model_label(best_bic_name), bic_table[best_bic_name],
      .model_label(best_name)
    )
    selection_msg <- sprintf(
      "%s was selected as the simpler of the two candidates.",
      .model_label(best_name)
    )
  }

  parts <- c(selection_msg, ic_msg, disp_msg, zi_msg)
  parts <- parts[nchar(parts) > 0L]
  paste(parts, collapse = " ")
}
