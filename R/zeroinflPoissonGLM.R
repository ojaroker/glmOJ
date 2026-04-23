#' Fit a zero-inflated Poisson regression model
#'
#' Fits a zero-inflated Poisson model (via [pscl::zeroinfl()]) with separate
#' count and zero-inflation components. Returns coefficients on the response
#' scale, randomized quantile residuals, a dispersion ratio, and a diagnostic
#' plot.
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
#' @param dispersion_threshold Numeric; dispersion ratios above this value
#'   are flagged as overdispersed in the diagnostic plot. Default 1.2.
#' @param ... Additional arguments passed to [pscl::zeroinfl()].
#'
#' @return An object of class `c("zeroinflPoissonGLM", "zeroinflGLMfit",
#'   "countGLMfit")`, a list with:
#'   \describe{
#'     \item{`call`}{The matched call.}
#'     \item{`model`}{The underlying [pscl::zeroinfl] fit object.}
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
#'   process) for a one-unit increase in the predictor. A value greater than 1
#'   means higher odds of being a structural zero.
#'
#' **When to use:** Zero-inflated Poisson is appropriate when there are more
#' zeros than a standard Poisson model would predict (excess zeros), but the
#' non-zero counts themselves follow a Poisson distribution. If the non-zero
#' counts are also overdispersed, prefer [zeroinflNegbinGLM()].
#'
#' @examples
#' df <- data.frame(
#'   y  = c(0L, 0L, 0L, 1L, 2L, 0L, 3L, 0L, 1L, 0L),
#'   x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
#' )
#' fit <- zeroinflPoissonGLM(y ~ x1, data = df)
#' print(fit)
#' plot(fit)
#'
#' # Intercept-only zero component:
#' fit2 <- zeroinflPoissonGLM(y ~ x1, data = df, ziformula = ~ 1)
#'
#' @seealso [zeroinflNegbinGLM()], [zeroinflTweedieGLM()], [poissonGLM()],
#'   [countGLM()], [pscl::zeroinfl()]
#' @export
zeroinflPoissonGLM <- function(
  formula,
  data,
  ziformula = NULL,
  maxit = NULL,
  dispersion_threshold = 1.2,
  ...
) {
  stopifnot(
    "formula must be a formula object" = inherits(formula, "formula"),
    "data must be a data frame" = is.data.frame(data),
    "ziformula must be a formula or NULL" = is.null(ziformula) ||
      inherits(ziformula, "formula"),
    "maxit must be a positive integer or NULL" = is.null(maxit) ||
      (is.numeric(maxit) && length(maxit) == 1L && maxit >= 1)
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
    c(list(formula = full_formula, data = data, dist = "poisson"), dots)
  )

  coef_tables <- zi_coef_tables(
    fit,
    count_label = "exp.coef",
    zero_label = "exp.coef"
  )

  rqr <- compute_rqr(fit, "zeroinfl_poisson")
  pearson_resid <- residuals(fit, type = "pearson")
  disp <- check_dispersion(fit)
  diag_plots <- plot_diagnostics(
    rqr,
    pearson_resid,
    fitted_vals,
    disp,
    dispersion_threshold = dispersion_threshold
  )
  structure(
    list(
      call = match.call(),
      model = fit,
      summary = summary(fit),
      coefficients = coef_tables,
      diagnostics = list(
        rqr = rqr,
        dispersion_ratio = disp,
        plot = diag_plots$rqr_plot,
        r2_plot = diag_plots$r2_plot
      ),
      aic = stats::AIC(fit),
      bic = stats::BIC(fit)
    ),
    class = c("zeroinflPoissonGLM", "zeroinflGLMfit", "countGLMfit")
  )
}

# ---------------------------------------------------------------------------
# Internal helpers shared by zeroinflPoissonGLM and zeroinflNegbinGLM
# ---------------------------------------------------------------------------

#' Build the pscl-style combined ZI formula
#' @noRd
build_zi_formula <- function(formula, ziformula) {
  count_rhs <- paste(deparse(formula[[3L]]), collapse = "")
  lhs <- paste(deparse(formula[[2L]]), collapse = "")

  if (is.null(ziformula)) {
    zero_rhs <- count_rhs
  } else {
    # ziformula may or may not have a LHS — use the RHS only
    zero_rhs <- deparse(ziformula[[length(ziformula)]])
  }

  stats::as.formula(
    paste0(lhs, " ~ ", count_rhs, " | ", zero_rhs)
  )
}

#' Build count and zero coefficient tables for a zeroinfl fit
#' @noRd
zi_coef_tables <- function(
  fit,
  count_label = "exp.coef",
  zero_label = "exp.coef"
) {
  count_coefs <- stats::coef(fit, model = "count")
  zero_coefs <- stats::coef(fit, model = "zero")
  p_count <- length(count_coefs)

  ci_all <- stats::confint.default(fit)
  ci_count <- ci_all[seq_len(p_count), , drop = FALSE]
  ci_zero <- ci_all[seq_len(nrow(ci_all)) > p_count, , drop = FALSE]

  # p-values from the summary coefficient matrix.
  # For ZINB, sm$count includes a "Log(theta)" row not in coef() — match by name.
  sm <- summary(fit)$coefficients
  pvals_count <- sm$count[names(count_coefs), "Pr(>|z|)"]
  pvals_zero <- sm$zero[names(zero_coefs), "Pr(>|z|)"]

  count_df <- data.frame(
    term = names(count_coefs),
    setNames(
      data.frame(exp(count_coefs), exp(ci_count[, 1L]), exp(ci_count[, 2L])),
      c(count_label, "lower.95", "upper.95")
    ),
    p.value = pvals_count,
    stars = as.character(sig_stars(pvals_count)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  zero_df <- data.frame(
    term = names(zero_coefs),
    setNames(
      data.frame(exp(zero_coefs), exp(ci_zero[, 1L]), exp(ci_zero[, 2L])),
      c(zero_label, "lower.95", "upper.95")
    ),
    p.value = pvals_zero,
    stars = as.character(sig_stars(pvals_zero)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  list(count = count_df, zero = zero_df)
}
