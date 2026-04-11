#' Fit a Poisson regression model
#'
#' Fits a Poisson GLM and returns model coefficients on the response scale
#' (exponentiated), randomized quantile residuals (RQR), a Pearson
#' dispersion ratio, and diagnostic plots.
#'
#' @param formula A model formula (e.g. `y ~ x1 + x2`). The response must be
#'   a non-negative integer count variable.
#' @param data A data frame containing the variables in `formula`.
#' @param assessZeroInflation Logical; when `TRUE` (default), runs a DHARMa
#'   simulation-based zero-inflation test after fitting. Issues a warning if
#'   significant zero-inflation is detected and adds `zi_test` to the returned
#'   diagnostics. Set to `FALSE` when calling from [countGLM()], which performs
#'   its own zero-inflation assessment.
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
#' **Coefficient interpretation:** Poisson regression models the log of the
#' expected count. Exponentiating a coefficient gives the multiplicative change
#' in the expected count for a one-unit increase in the predictor.
#'
#' **Condition checking:** Inspect `diagnostics$dispersion_ratio` (near 1 is
#' good) and `diagnostics$zi_test$p_value` for zero-inflation.
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
poissonGLM <- function(formula, data, assessZeroInflation = TRUE, ...) {
  stopifnot(
    "formula must be a formula object" = inherits(formula, "formula"),
    "data must be a data frame"        = is.data.frame(data)
  )

  check_sample_size(formula, data)
  fit <- stats::glm(formula, data = data, family = stats::poisson(), ...)

  # DHARMa zero-inflation test
  zi_test <- NULL
  if (isTRUE(assessZeroInflation)) {
    zi_test <- run_dharma_zi_test(fit, model_type = "Poisson")
    if (isTRUE(zi_test$detected)) {
      warning(sprintf(
        "Possible zero-inflation detected by DHARMa test (p = %.3f). Consider zeroinflPoissonGLM() or zeroinflNegbinGLM().",
        zi_test$p_value
      ), call. = FALSE)
    }
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

  rqr           <- compute_rqr(fit, "poisson")
  pearson_resid <- residuals(fit, type = "pearson")
  disp          <- check_dispersion(fit)
  diag_plots    <- plot_diagnostics(rqr, pearson_resid, fit$fitted.values, disp)

  structure(
    list(
      call         = match.call(),
      model        = fit,
      summary      = summary(fit),
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
    class = c("poissonGLM", "countGLMfit")
  )
}
