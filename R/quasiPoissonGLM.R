#' Fit a quasi-Poisson regression model
#'
#' Fits a quasi-Poisson GLM via `stats::glm(family = quasipoisson())` and
#' returns exponentiated coefficients with quasi-likelihood Wald 95% CIs, the
#' estimated dispersion parameter (`phi`), Pearson dispersion ratio, and
#' diagnostic plots. Quasi-Poisson shares the Poisson mean structure but
#' estimates a free scale parameter so that `Var(Y) = phi * mu`.
#'
#' @param formula A model formula (e.g. `y ~ x1 + x2`). The response must be
#'   a non-negative integer count variable.
#' @param data A data frame containing the variables in `formula`.
#' @param maxit Optional integer; maximum IWLS iterations passed through as
#'   `control = stats::glm.control(maxit = maxit)`. Ignored when the user
#'   supplies their own `control` via `...`.
#' @param ... Additional arguments passed to [stats::glm()].
#'
#' @return An object of class `c("quasiPoissonGLM", "countGLMfit")`, a list
#'   with:
#'   \describe{
#'     \item{`call`}{The matched call.}
#'     \item{`model`}{The underlying [stats::glm] fit object.}
#'     \item{`summary`}{The result of `summary()` on the fitted model.}
#'     \item{`phi`}{The estimated quasi-likelihood dispersion parameter
#'       (`summary(fit)$dispersion`).}
#'     \item{`coefficients`}{A data frame with columns `term`, `exp.coef`,
#'       `lower.95`, `upper.95`, `p.value`, and `stars`. Standard errors (and
#'       hence CIs and p-values) are inflated by `sqrt(phi)` relative to a
#'       Poisson fit.}
#'     \item{`diagnostics`}{A list with:
#'       \describe{
#'         \item{`rqr`}{Numeric vector of randomized quantile residuals,
#'           computed from the Poisson CDF evaluated at the fitted means
#'           (quasi-Poisson shares the Poisson mean structure).}
#'         \item{`dispersion_ratio`}{Pearson chi-squared / df.residual, equal
#'           to `phi`.}
#'         \item{`plot`}{Patchwork ggplot: fitted vs RQR and histo-QQ. The
#'           dispersion-ratio label in the title equals `phi`.}
#'         \item{`r2_plot`}{Squared Pearson residuals vs fitted values.}
#'       }
#'     }
#'     \item{`aic`}{`NA_real_`. Quasi-likelihood fits do not have a proper
#'       likelihood, so AIC is undefined.}
#'     \item{`bic`}{`NA_real_`. BIC is also undefined for quasi-likelihood
#'       fits.}
#'   }
#'
#' @details
#' **Coefficient interpretation:** Identical to Poisson regression;
#' exponentiating a coefficient gives the multiplicative change in the expected
#' count for a one-unit increase in the predictor. The point estimates match
#' [poissonGLM()] exactly — only the standard errors differ.
#'
#' **When to use:** Quasi-Poisson is appropriate when a Poisson fit shows
#' mild-to-moderate overdispersion that appears *constant* across the range
#' of fitted values — i.e. the squared Pearson residuals form a roughly flat
#' cloud with mean `phi > 1`, rather than fanning out with the fitted mean
#' (which would motivate [negbinGLM()]). [countGLM()] automatically fits
#' quasi-Poisson when the Poisson dispersion ratio exceeds 1.2 and the r²
#' vs fitted plot is approximately flat above 1.
#'
#' **No AIC/BIC:** Because quasi-Poisson is a quasi-likelihood method, AIC
#' and BIC are not defined and are returned as `NA`. Quasi-Poisson therefore
#' does not participate in the likelihood-based comparison performed by
#' [countGLM()]; it is reported alongside the main comparison when flagged.
#'
#' @examples
#' df <- data.frame(
#'   y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
#'   x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
#' )
#' fit <- quasiPoissonGLM(y ~ x1, data = df)
#' print(fit)
#' plot(fit)
#'
#' @seealso [poissonGLM()], [negbinGLM()], [tweedieGLM()], [countGLM()],
#'   [stats::glm()]
#' @export
quasiPoissonGLM <- function(formula, data, maxit = NULL, ...) {
  stopifnot(
    "formula must be a formula object" = inherits(formula, "formula"),
    "data must be a data frame"        = is.data.frame(data),
    "maxit must be a positive integer or NULL" =
      is.null(maxit) || (is.numeric(maxit) && length(maxit) == 1L && maxit >= 1)
  )

  check_sample_size(formula, data)
  dots <- list(...)
  if (!is.null(maxit) && !"control" %in% names(dots)) {
    dots$control <- stats::glm.control(maxit = as.integer(maxit))
  }
  fit <- do.call(
    stats::glm,
    c(list(formula = formula, data = data, family = stats::quasipoisson()), dots)
  )
  if (!isTRUE(fit$converged)) {
    stop(
      "Quasi-Poisson GLM did not converge. Check for extreme predictor values, perfect separation, or collinearity.",
      call. = FALSE
    )
  }

  smry <- summary(fit)
  phi  <- as.numeric(smry$dispersion)

  # Exponentiated coefficients with quasi-likelihood Wald 95% CIs and p-values.
  # summary(fit)$coefficients already applies the sqrt(phi) inflation to SEs.
  sm_coef <- smry$coefficients
  est     <- sm_coef[, "Estimate"]
  se      <- sm_coef[, "Std. Error"]
  pvals   <- sm_coef[, "Pr(>|t|)"]
  t_crit  <- stats::qt(0.975, df = fit$df.residual)
  coef_table <- data.frame(
    term      = rownames(sm_coef),
    exp.coef  = exp(est),
    lower.95  = exp(est - t_crit * se),
    upper.95  = exp(est + t_crit * se),
    p.value   = pvals,
    stars     = as.character(sig_stars(pvals)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  pearson_resid <- residuals(fit, type = "pearson")
  disp          <- check_dispersion(fit)
  # RQR via the Poisson CDF with fitted means. Quasi-Poisson and Poisson share
  # the same fitted means, so this is the same RQR as the Poisson fit — the
  # appropriate diagnostic for checking the mean structure.
  rqr           <- compute_rqr_quasipoisson(fit)
  diag_plots    <- plot_diagnostics(rqr, pearson_resid, fit$fitted.values, disp)

  structure(
    list(
      call         = match.call(),
      model        = fit,
      summary      = smry,
      phi          = phi,
      coefficients = coef_table,
      diagnostics  = list(
        rqr              = rqr,
        dispersion_ratio = disp,
        plot             = diag_plots$rqr_plot,
        r2_plot          = diag_plots$r2_plot
      ),
      aic = NA_real_,
      bic = NA_real_
    ),
    class = c("quasiPoissonGLM", "countGLMfit")
  )
}
