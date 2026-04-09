#' Fit a Poisson regression model
#'
#' Fits a Poisson GLM and returns model coefficients on the response scale
#' (exponentiated), randomized quantile residuals (RQR), a Pearson
#' dispersion ratio, and a two-panel diagnostic plot.
#'
#' @param formula A model formula (e.g. `y ~ x1 + x2`). The response must be
#'   a non-negative integer count variable.
#' @param data A data frame containing the variables in `formula`.
#' @param ... Additional arguments passed to [stats::glm()].
#'
#' @return An object of class `c("poissonGLM", "countGLMfit")`, a list with:
#'   \describe{
#'     \item{`call`}{The matched call.}
#'     \item{`model`}{The underlying [stats::glm] fit object.}
#'     \item{`summary`}{The result of `summary()` on the fitted model.}
#'     \item{`coefficients`}{A data frame with columns `term`, `exp.coef`,
#'       `lower.95`, `upper.95` (all on the response/exponentiated scale).}
#'     \item{`diagnostics`}{A list with:
#'       \describe{
#'         \item{`rqr`}{Numeric vector of randomized quantile residuals.}
#'         \item{`dispersion_ratio`}{Pearson chi-squared / df.residual.
#'           Values substantially above 1 (rule of thumb: > 1.5) suggest
#'           overdispersion; consider [negbinGLM()].}
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
#' **Coefficient interpretation:** Poisson regression models the log of the
#' expected count. Exponentiating a coefficient gives the multiplicative change in the expected count for a
#' one-unit increase in the predictor, adjusting for simultaneous linear changes in other predictors.
#' For example, 1.5 means a 50% higher expected count.
#'
#' **Condition checking:** Inspect `diagnostics$dispersion_ratio`. A value
#' near 1 is consistent with the Poisson assumption (mean = variance). The
#' RQR diagnostic plot should show points scattered randomly around zero with
#' approximately normal QQ behaviour.
#'
#' @examples
#' df <- data.frame(
#'   y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
#'   x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
#' )
#' fit <- poissonGLM(y ~ x1, data = df)
#' print(fit)
#' plot(fit)
#'
#' @seealso [negbinGLM()], [zeroinflPoissonGLM()], [countGLM()], [stats::glm()]
#' @export
poissonGLM <- function(formula, data, ...) {
  stopifnot(
    "formula must be a formula object" = inherits(formula, "formula"),
    "data must be a data frame"        = is.data.frame(data)
  )

  fit <- stats::glm(formula, data = data, family = stats::poisson(), ...)

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

  rqr  <- compute_rqr(fit, "poisson")
  disp <- check_dispersion(fit)
  diag_plot <- plot_diagnostics(rqr, fit$fitted.values, disp)

  structure(
    list(
      call         = match.call(),
      model        = fit,
      summary      = summary(fit),
      coefficients = coef_table,
      diagnostics  = list(
        rqr              = rqr,
        dispersion_ratio = disp,
        plot             = diag_plot
      ),
      aic = stats::AIC(fit),
      bic = stats::BIC(fit)
    ),
    class = c("poissonGLM", "countGLMfit")
  )
}
