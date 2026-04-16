#' Fit a zero-inflated Tweedie regression model
#'
#' Fits a zero-inflated Tweedie model (via [glmmTMB::glmmTMB()]) with separate
#' count and zero-inflation components. Returns coefficients on the response
#' scale, randomized quantile residuals, dispersion (`phi`) and power (`p`)
#' parameters, and diagnostic plots.
#'
#' @param formula A model formula for the **count** component (e.g.
#'   `y ~ x1 + x2`). The response must be non-negative.
#' @param data A data frame containing the variables in `formula` (and
#'   `ziformula` if provided).
#' @param ziformula A one-sided formula for the **zero-inflation** component
#'   (e.g. `~ x1`). When `NULL` (default), the same right-hand side as
#'   `formula` is used for both components. Use `~ 1` for an intercept-only
#'   zero-inflation model.
#' @param ... Additional arguments passed to [glmmTMB::glmmTMB()].
#'
#' @return An object of class `c("zeroinflTweedieGLM", "zeroinflGLMfit",
#'   "countGLMfit")`, a list with:
#'   \describe{
#'     \item{`call`}{The matched call.}
#'     \item{`model`}{The underlying [glmmTMB::glmmTMB] fit object.}
#'     \item{`summary`}{The result of `summary()` on the fitted model.}
#'     \item{`phi`}{The estimated Tweedie dispersion parameter (phi).}
#'     \item{`p`}{The estimated Tweedie power parameter (p), in (1, 2).
#'       `NA` if the parameter cannot be extracted from the fit.}
#'     \item{`coefficients`}{A list with two data frames, each with columns
#'       `term`, `exp.coef`, `lower.95`, `upper.95`, `p.value`, and `stars`:
#'       \describe{
#'         \item{`count`}{Exponentiated coefficients for the count component.}
#'         \item{`zero`}{Exponentiated coefficients for the zero-inflation
#'           component (odds ratios for structural-zero membership).}
#'       }
#'     }
#'     \item{`diagnostics`}{A list with `rqr`, `dispersion_ratio`, `plot`, and
#'       `r2_plot` (see [tweedieGLM()] for details). No `zi_test` is included
#'       since this model already accounts for zero-inflation.}
#'     \item{`aic`}{AIC of the fitted model.}
#'     \item{`bic`}{BIC of the fitted model.}
#'   }
#'
#' @details
#' **Coefficient interpretation:**
#' - *Count component*: exponentiating a coefficient gives the multiplicative
#'   change in the expected Tweedie mean among observations that are not
#'   structural zeros, for a one-unit increase in the predictor.
#' - *Zero component*: exponentiating a coefficient gives the multiplicative
#'   change in the odds of being a structural zero (vs. entering the count
#'   process) for a one-unit increase in the predictor. A value greater than 1
#'   means higher odds of a structural zero.
#'
#' **When to use:** Zero-inflated Tweedie is the most flexible model in glmOJ.
#' It handles excess zeros, non-negative semi-continuous responses, and complex
#' variance structures simultaneously. Prefer this over [zeroinflPoissonGLM()]
#' or [zeroinflNegbinGLM()] when the response is semi-continuous rather than
#' strictly integer-valued.
#'
#' @examples
#' df <- data.frame(
#'   y  = c(0, 0, 0, 1.5, 3.2, 0, 0.9, 0, 0, 2.7,
#'          0, 0, 4.1, 0, 1.2, 0, 0, 5.8, 0, 0),
#'   x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7,
#'          -0.5, 0.9, 1.3, -0.3, 0.6, -1.2, 0.1, 2.1, -0.7, 0.4)
#' )
#' fit <- suppressWarnings(zeroinflTweedieGLM(y ~ x1, data = df))
#' print(fit)
#' plot(fit)
#'
#' # Intercept-only zero component:
#' fit2 <- suppressWarnings(
#'   zeroinflTweedieGLM(y ~ x1, data = df, ziformula = ~ 1)
#' )
#'
#' @seealso [tweedieGLM()], [zeroinflNegbinGLM()], [countGLM()],
#'   [glmmTMB::glmmTMB()]
#' @export
zeroinflTweedieGLM <- function(formula, data, ziformula = NULL, ...) {
  stopifnot(
    "formula must be a formula object"    = inherits(formula, "formula"),
    "data must be a data frame"           = is.data.frame(data),
    "ziformula must be a formula or NULL" =
      is.null(ziformula) || inherits(ziformula, "formula")
  )

  effective_zi <- if (is.null(ziformula)) {
    stats::as.formula(paste("~", paste(deparse(formula[[3L]]), collapse = "")))
  } else {
    ziformula
  }
  check_sample_size(formula, data, effective_zi)

  fit <- glmmTMB::glmmTMB(
    formula, data = data,
    family   = glmmTMB::tweedie(link = "log"),
    ziformula = effective_zi, ...
  )

  # Tweedie-specific parameters
  phi <- tryCatch(glmmTMB::sigma(fit), error = function(e) NA_real_)
  p   <- .get_tweedie_p(fit)

  # ---- count component coefficients ----
  cond_coefs <- glmmTMB::fixef(fit)$cond
  vcov_cond  <- stats::vcov(fit)$cond
  se_cond    <- sqrt(diag(vcov_cond))
  ci_cond_lo <- cond_coefs - stats::qnorm(0.975) * se_cond
  ci_cond_hi <- cond_coefs + stats::qnorm(0.975) * se_cond
  pvals_cond <- summary(fit)$coefficients$cond[names(cond_coefs), "Pr(>|z|)"]

  count_df <- data.frame(
    term      = names(cond_coefs),
    exp.coef  = exp(cond_coefs),
    lower.95  = exp(ci_cond_lo),
    upper.95  = exp(ci_cond_hi),
    p.value   = pvals_cond,
    stars     = as.character(sig_stars(pvals_cond)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  # ---- zero-inflation component coefficients ----
  zi_coefs   <- glmmTMB::fixef(fit)$zi
  vcov_zi    <- stats::vcov(fit)$zi
  se_zi      <- sqrt(diag(vcov_zi))
  ci_zi_lo   <- zi_coefs - stats::qnorm(0.975) * se_zi
  ci_zi_hi   <- zi_coefs + stats::qnorm(0.975) * se_zi
  pvals_zi   <- summary(fit)$coefficients$zi[names(zi_coefs), "Pr(>|z|)"]

  zero_df <- data.frame(
    term      = names(zi_coefs),
    exp.coef  = exp(zi_coefs),
    lower.95  = exp(ci_zi_lo),
    upper.95  = exp(ci_zi_hi),
    p.value   = pvals_zi,
    stars     = as.character(sig_stars(pvals_zi)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  rqr           <- compute_rqr(fit, "zeroinfl_tweedie")
  pearson_resid <- tryCatch(
    residuals(fit, type = "pearson"),
    error = function(e) rep(NA_real_, nrow(data))
  )
  disp        <- check_dispersion(fit)
  fitted_vals <- as.numeric(fitted(fit))
  diag_plots  <- plot_diagnostics(rqr, pearson_resid, fitted_vals, disp)

  structure(
    list(
      call         = match.call(),
      model        = fit,
      summary      = summary(fit),
      phi          = phi,
      p            = p,
      coefficients = list(count = count_df, zero = zero_df),
      diagnostics  = list(
        rqr              = rqr,
        dispersion_ratio = disp,
        plot             = diag_plots$rqr_plot,
        r2_plot          = diag_plots$r2_plot
      ),
      aic = stats::AIC(fit),
      bic = stats::BIC(fit)
    ),
    class = c("zeroinflTweedieGLM", "zeroinflGLMfit", "countGLMfit")
  )
}
