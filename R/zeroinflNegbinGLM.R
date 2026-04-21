#' Fit a zero-inflated negative binomial regression model
#'
#' Fits a zero-inflated negative binomial model (via [pscl::zeroinfl()]) with
#' separate count and zero-inflation components. Returns coefficients on the
#' response scale, randomized quantile residuals, a dispersion ratio, and a
#' diagnostic plot.
#'
#' @param formula A model formula for the **count** component (e.g.
#'   `y ~ x1 + x2`). The response must be a non-negative integer count.
#' @param data A data frame containing the variables in `formula` (and
#'   `ziformula` if provided).
#' @param ziformula A one-sided formula for the **zero-inflation** component
#'   (e.g. `~ x1`). When `NULL` (default), the same right-hand side as
#'   `formula` is used for both components. Use `~ 1` for an intercept-only
#'   zero-inflation model.
#' @param maxit Optional integer; maximum optimizer iterations passed through
#'   as `control = pscl::zeroinfl.control(maxit = maxit)`. Ignored when the
#'   user supplies their own `control` via `...`.
#' @param ... Additional arguments passed to [pscl::zeroinfl()].
#'
#' @return An object of class `c("zeroinflNegbinGLM", "zeroinflGLMfit",
#'   "countGLMfit")`, a list with:
#'   \describe{
#'     \item{`call`}{The matched call.}
#'     \item{`model`}{The underlying [pscl::zeroinfl] fit object.}
#'     \item{`theta`}{The estimated negative binomial dispersion parameter.}
#'     \item{`coefficients`}{A list with two data frames, each with columns
#'       `term`, `exp.coef`, `lower.95`, `upper.95`:
#'       \describe{
#'         \item{`count`}{Exponentiated coefficients for the count component.}
#'         \item{`zero`}{Exponentiated coefficients for the zero-inflation
#'           component.}
#'       }
#'     }
#'     \item{`summary`}{The result of `summary()` on the fitted model.}
#'     \item{`diagnostics`}{A list with `rqr`, `dispersion_ratio`, and
#'       `plot` (see [poissonGLM()] for details).}
#'     \item{`aic`}{AIC of the fitted model.}
#'     \item{`bic`}{BIC of the fitted model.}
#'   }
#'
#' @details
#' **Coefficient interpretation:**
#' - *Count component*: exponentiating a coefficient gives the multiplicative
#'   change in the expected count among non-structural-zero observations, for a
#'   one-unit increase in the predictor, adjusting for simultaneous linear
#'   changes in other predictors. For example, 1.5 means a 50% higher expected
#'   count.
#' - *Zero component*: exponentiating a coefficient gives the multiplicative
#'   change in the odds of being a structural zero (vs. entering the count
#'   process) for a one-unit increase in the predictor.
#'
#' **When to use:** Zero-inflated negative binomial handles both excess zeros
#' *and* overdispersion in the non-zero counts. Prefer this over
#' [zeroinflPoissonGLM()] when the non-zero counts remain overdispersed. For
#' count data with complex variance structures and excess zeros, consider
#' [zeroinflTweedieGLM()].
#'
#' @examples
#' df <- data.frame(
#'   y  = c(0L, 0L, 0L, 1L, 2L, 0L, 3L, 0L, 1L, 0L),
#'   x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
#' )
#' fit <- zeroinflNegbinGLM(y ~ x1, data = df)
#' print(fit)
#' plot(fit)
#'
#' # Intercept-only zero component:
#' fit2 <- zeroinflNegbinGLM(y ~ x1, data = df, ziformula = ~ 1)
#'
#' @seealso [zeroinflPoissonGLM()], [zeroinflTweedieGLM()], [negbinGLM()],
#'   [countGLM()], [pscl::zeroinfl()]
#' @export
zeroinflNegbinGLM <- function(formula, data, ziformula = NULL, maxit = NULL, ...) {
  stopifnot(
    "formula must be a formula object"    = inherits(formula, "formula"),
    "data must be a data frame"           = is.data.frame(data),
    "ziformula must be a formula or NULL" =
      is.null(ziformula) || inherits(ziformula, "formula"),
    "maxit must be a positive integer or NULL" =
      is.null(maxit) || (is.numeric(maxit) && length(maxit) == 1L && maxit >= 1)
  )

  effective_zi <- if (is.null(ziformula)) {
    stats::as.formula(paste("~", paste(deparse(formula[[3L]]), collapse = "")))
  } else {
    ziformula
  }
  check_sample_size(formula, data, effective_zi)

  full_formula <- build_zi_formula(formula, ziformula)
  dots <- list(...)
  if (!is.null(maxit) && !"control" %in% names(dots)) {
    dots$control <- pscl::zeroinfl.control(maxit = as.integer(maxit))
  }
  fit <- do.call(
    pscl::zeroinfl,
    c(list(formula = full_formula, data = data, dist = "negbin"), dots)
  )

  coef_tables <- zi_coef_tables(fit, count_label = "exp.coef", zero_label = "exp.coef")

  rqr           <- compute_rqr(fit, "zeroinfl_negbin")
  pearson_resid <- residuals(fit, type = "pearson")
  disp          <- check_dispersion(fit)
  diag_plots    <- plot_diagnostics(rqr, pearson_resid, fit$fitted.values, disp)

  structure(
    list(
      call         = match.call(),
      model        = fit,
      summary      = summary(fit),
      theta        = fit$theta,
      coefficients = coef_tables,
      diagnostics  = list(
        rqr              = rqr,
        dispersion_ratio = disp,
        plot             = diag_plots$rqr_plot,
        r2_plot          = diag_plots$r2_plot
      ),
      aic = stats::AIC(fit),
      bic = stats::BIC(fit)
    ),
    class = c("zeroinflNegbinGLM", "zeroinflGLMfit", "countGLMfit")
  )
}
