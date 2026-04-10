#' Fit a negative binomial regression model
#'
#' Fits a negative binomial GLM (via [MASS::glm.nb()]) and returns model
#' coefficients on the response scale (exponentiated), randomized quantile
#' residuals (RQR), a Pearson dispersion ratio, and a two-panel diagnostic
#' plot.
#'
#' @param formula A model formula (e.g. `y ~ x1 + x2`). The response must be
#'   a non-negative integer count variable.
#' @param data A data frame containing the variables in `formula`.
#' @param ... Additional arguments passed to [MASS::glm.nb()].
#'
#' @return An object of class `c("negbinGLM", "countGLMfit")`, a list with:
#'   \describe{
#'     \item{`call`}{The matched call.}
#'     \item{`model`}{The underlying [MASS::glm.nb] fit object.}
#'     \item{`summary`}{The result of `summary()` on the fitted model.}
#'     \item{`theta`}{The estimated negative binomial dispersion parameter
#'       (smaller values indicate more overdispersion).}
#'     \item{`coefficients`}{A data frame with columns `term`, `exp.coef`,
#'       `lower.95`, `upper.95` (all on the response/exponentiated scale).}
#'     \item{`diagnostics`}{A list with:
#'       \describe{
#'         \item{`rqr`}{Numeric vector of randomized quantile residuals.}
#'         \item{`dispersion_ratio`}{Pearson chi-squared / df.residual.
#'           For a well-fitted negative binomial model this should be near 1.}
#'         \item{`plot`}{A patchwork ggplot: fitted values vs RQR (left) and
#'           histo-QQ of RQR (right). The dispersion ratio is shown in red with
#'           an overdispersion warning if it exceeds 1.2.}
#'       }
#'     }
#'     \item{`aic`}{AIC of the fitted model.}
#'     \item{`bic`}{BIC of the fitted model.}
#'   }
#'
#' @details
#' **Coefficient interpretation:** Like Poisson regression, negative binomial
#' regression models the log of the expected count. Exponentiating a
#' coefficient gives the multiplicative change in the expected count for a
#' one-unit increase in the predictor, adjusting for simultaneous linear
#' changes in other predictors. For example, 1.5 means a 50% higher expected
#' count.
#'
#' **When to use:** Negative binomial is appropriate when count data show
#' overdispersion (variance > mean). A Pearson dispersion ratio from
#' [poissonGLM()] substantially above 1 (rule of thumb: > 1.5) is a common
#' signal. The negative binomial adds a free parameter `theta` to model this
#' extra variance.
#'
#' @examples
#' df <- data.frame(
#'   y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
#'   x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
#' )
#' fit <- negbinGLM(y ~ x1, data = df)
#' print(fit)
#' plot(fit)
#'
#' @seealso [poissonGLM()], [zeroinflNegbinGLM()], [countGLM()], [MASS::glm.nb()]
#' @export
negbinGLM <- function(formula, data, ...) {
  stopifnot(
    "formula must be a formula object" = inherits(formula, "formula"),
    "data must be a data frame"        = is.data.frame(data)
  )

  check_sample_size(formula, data)
  fit <- MASS::glm.nb(formula, data = data, ...)

  # check zero inflation
  zi_check <- check_zero_inflation(fit, "negbin")
  if (isTRUE(zi_check$zero_inflation)) {
    warning(sprintf(
      "Observed zeros (%d) exceed expected zeros (%.1f); possible zero inflation (ratio = %.2f). Consider a zero-inflated model.",
      zi_check$observed_zeros, zi_check$expected_zeros, zi_check$ratio
    ), call. = FALSE)
  }

  # Exponentiated coefficients with Wald 95% CIs
  est <- stats::coef(fit)
  ci  <- stats::confint.default(fit)
  coef_table <- data.frame(
    term      = names(est),
    exp.coef  = exp(est),
    lower.95  = exp(ci[, 1L]),
    upper.95  = exp(ci[, 2L]),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  rqr           <- compute_rqr(fit, "negbin")
  pearson_resid <- residuals(fit, type = "pearson")
  disp          <- check_dispersion(fit)
  diag_plots    <- plot_diagnostics(rqr, pearson_resid, fit$fitted.values, disp)

  structure(
    list(
      call         = match.call(),
      model        = fit,
      summary      = summary(fit),
      theta        = fit$theta,
      coefficients = coef_table,
      diagnostics  = list(
        rqr              = rqr,
        dispersion_ratio = disp,
        plot             = diag_plots$rqr_plot,
        r2_plot          = diag_plots$r2_plot
      ),
      aic = stats::AIC(fit),
      bic = stats::BIC(fit)
    ),
    class = c("negbinGLM", "countGLMfit")
  )
}
