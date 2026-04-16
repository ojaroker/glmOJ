#' Fit a Tweedie regression model
#'
#' Fits a Tweedie GLM (via [glmmTMB::glmmTMB()]) and returns model coefficients
#' on the response scale (exponentiated), randomized quantile residuals (RQR),
#' estimated dispersion (`phi`) and power (`p`) parameters, and diagnostic
#' plots. The Tweedie family generalises Poisson and Gamma distributions and is
#' well-suited to non-negative semi-continuous data with a point mass at zero.
#'
#' @param formula A model formula (e.g. `y ~ x1 + x2`). The response must be
#'   non-negative.
#' @param data A data frame containing the variables in `formula`.
#' @param assessZeroInflation Logical; when `TRUE` (default), runs a DHARMa
#'   simulation-based zero-inflation test after fitting. Issues a warning if
#'   significant zero-inflation is detected and adds `zi_test` to the returned
#'   diagnostics. Set to `FALSE` when calling from [countGLM()], which performs
#'   its own zero-inflation assessment.
#' @param ... Additional arguments passed to [glmmTMB::glmmTMB()].
#'
#' @return An object of class `c("tweedieGLM", "countGLMfit")`, a list with:
#'   \describe{
#'     \item{`call`}{The matched call.}
#'     \item{`model`}{The underlying [glmmTMB::glmmTMB] fit object.}
#'     \item{`summary`}{The result of `summary()` on the fitted model.}
#'     \item{`phi`}{The estimated Tweedie dispersion parameter (phi).}
#'     \item{`p`}{The estimated Tweedie power parameter (p), restricted to
#'       (1, 2). Values near 1 resemble Poisson; values near 2 resemble Gamma.
#'       `NA` if the parameter cannot be extracted from the fit.}
#'     \item{`coefficients`}{A data frame with columns `term`, `exp.coef`,
#'       `lower.95`, `upper.95`, `p.value`, and `stars` (all on the
#'       response/exponentiated scale).}
#'     \item{`diagnostics`}{A list with:
#'       \describe{
#'         \item{`rqr`}{Numeric vector of randomized quantile residuals
#'           (DHARMa simulation-based, converted to normal scale).}
#'         \item{`dispersion_ratio`}{Pearson chi-squared / df.residual.}
#'         \item{`plot`}{Patchwork ggplot: fitted vs RQR and histo-QQ.}
#'         \item{`r2_plot`}{Squared Pearson residuals vs fitted values.}
#'         \item{`zi_test`}{When `assessZeroInflation = TRUE`, a list with
#'           `detected` (logical), `p_value` (numeric), and `plot` (ggplot
#'           histogram of DHARMa simulated zero proportions vs observed).
#'           `NULL` when `assessZeroInflation = FALSE`.}
#'       }
#'     }
#'     \item{`aic`}{AIC of the fitted model.}
#'     \item{`bic`}{BIC of the fitted model.}
#'   }
#'
#' @details
#' **Three parameters:** The Tweedie family is characterised by three estimated
#' quantities: the regression coefficients (mean structure, via a log link),
#' the dispersion `phi` (Var(Y) = phi * mu^p), and the power `p` (1 < p < 2).
#' When `p` is close to 1, the variance structure resembles Poisson; when
#' close to 2, it resembles Gamma.
#'
#' **Coefficient interpretation:** Exponentiating a coefficient gives the
#' multiplicative change in the expected response for a one-unit increase in
#' the predictor, holding all other predictors constant.
#'
#' **When to use:** Tweedie regression is appropriate for non-negative
#' semi-continuous data with exact zeros and continuous positive values, or
#' when count data have complex variance structures not well captured by Poisson
#' or negative binomial. If zero-inflation is also detected, consider
#' [zeroinflTweedieGLM()].
#'
#' @examples
#' df <- data.frame(
#'   y  = c(0, 0, 1.5, 3.2, 5.8, 0, 0.9, 4.1, 0, 2.7),
#'   x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
#' )
#' fit <- suppressWarnings(tweedieGLM(y ~ x1, data = df))
#' print(fit)
#' plot(fit)
#'
#' @seealso [zeroinflTweedieGLM()], [negbinGLM()], [poissonGLM()],
#'   [countGLM()], [glmmTMB::glmmTMB()]
#' @export
tweedieGLM <- function(formula, data, assessZeroInflation = TRUE, ...) {
  stopifnot(
    "formula must be a formula object" = inherits(formula, "formula"),
    "data must be a data frame"        = is.data.frame(data)
  )

  check_sample_size(formula, data)
  fit <- glmmTMB::glmmTMB(
    formula, data = data,
    family = glmmTMB::tweedie(link = "log"), ...
  )
  conv_code <- tryCatch(fit$fit$convergence, error = function(e) 1L)
  if (!isTRUE(conv_code == 0L)) {
    stop(
      "Tweedie GLM did not converge. Check for extreme predictor values or numerical issues.",
      call. = FALSE
    )
  }

  # DHARMa zero-inflation test
  zi_test <- NULL
  if (isTRUE(assessZeroInflation)) {
    zi_test <- run_dharma_zi_test(fit, model_type = "Tweedie")
    if (isTRUE(zi_test$detected)) {
      warning(sprintf(
        "Possible zero-inflation detected by DHARMa test (p = %.3f). Consider zeroinflTweedieGLM().",
        zi_test$p_value
      ), call. = FALSE)
    }
  }

  # Tweedie-specific parameters
  phi <- tryCatch(glmmTMB::sigma(fit), error = function(e) NA_real_)
  p   <- .get_tweedie_p(fit)

  if (!is.na(p) && (p <= 1.01 || p >= 1.99)) {
    warning(
      paste0(
        sprintf("Tweedie power parameter p = %.4f", p),
        " is at or near the boundary of (1, 2). ",
        "Values near 1 indicate near-Poisson behaviour; values near 2 indicate ",
        "near-Gamma behaviour. The fit may be degenerate on this data."
      ),
      call. = FALSE
    )
  }

  # Exponentiated coefficients with Wald 95% CIs and p-values
  cond_coefs <- glmmTMB::fixef(fit)$cond
  vcov_cond  <- stats::vcov(fit)$cond
  se_cond    <- sqrt(diag(vcov_cond))
  ci_lo      <- cond_coefs - stats::qnorm(0.975) * se_cond
  ci_hi      <- cond_coefs + stats::qnorm(0.975) * se_cond
  pvals      <- summary(fit)$coefficients$cond[names(cond_coefs), "Pr(>|z|)"]

  coef_table <- data.frame(
    term      = names(cond_coefs),
    exp.coef  = exp(cond_coefs),
    lower.95  = exp(ci_lo),
    upper.95  = exp(ci_hi),
    p.value   = pvals,
    stars     = as.character(sig_stars(pvals)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  rqr           <- compute_rqr(fit, "tweedie")
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
      coefficients = coef_table,
      diagnostics  = list(
        rqr              = rqr,
        dispersion_ratio = disp,
        plot             = diag_plots$rqr_plot,
        r2_plot          = diag_plots$r2_plot,
        zi_test          = zi_test
      ),
      aic = stats::AIC(fit),
      bic = stats::BIC(fit)
    ),
    class = c("tweedieGLM", "countGLMfit")
  )
}

# ---------------------------------------------------------------------------
# Internal helper: extract Tweedie power parameter p from a glmmTMB fit
# ---------------------------------------------------------------------------

#' @noRd
.get_tweedie_p <- function(fit) {
  # Primary: use the stable public API
  tryCatch({
    fp <- glmmTMB::family_params(fit)
    if (!is.null(fp) && "Tweedie power" %in% names(fp))
      return(as.numeric(fp[["Tweedie power"]]))
  }, error = function(e) NULL)

  # Fallback: decode psi from the raw parameter vector directly
  tryCatch({
    pars <- fit$fit$par
    nm   <- names(pars)
    # glmmTMB encodes p in (1, 2) via a logit transform: p = 1 + plogis(psi)
    idx <- which(nm == "psi")
    if (length(idx) > 0L) return(1 + stats::plogis(pars[idx[1L]]))
    idx2 <- grep("^thetaf", nm)
    if (length(idx2) > 0L) return(1 + stats::plogis(pars[idx2[1L]]))
    NA_real_
  }, error = function(e) NA_real_)
}
