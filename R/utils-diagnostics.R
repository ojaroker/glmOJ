# Internal diagnostic helpers for count regression models
# Not exported — used by poissonGLM, negbinGLM, zeroinflPoissonGLM, zeroinflNegbinGLM

#' Compute the Pearson dispersion ratio
#'
#' @param model A fitted model object with `residuals(..., type = "pearson")`
#'   and a `df.residual` slot (e.g., `glm`, `negbin`, `zeroinfl`).
#' @return A single numeric scalar (Pearson chi-squared / df.residual), or
#'   `NA_real_` if df.residual is 0.
#' @noRd
check_dispersion <- function(model) {
  df_resid <- model$df.residual
  if (is.null(df_resid) || df_resid == 0) return(NA_real_)
  pearson_resid <- residuals(model, type = "pearson")
  sum(pearson_resid^2) / df_resid
}

#' Compute randomized quantile residuals
#'
#' For Poisson and negative binomial GLMs, delegates to `statmod`. For
#' zero-inflated models, uses the manual CDF approach via `pscl::predprob`.
#'
#' @param model A fitted model object.
#' @param family One of `"poisson"`, `"negbin"`, `"zeroinfl_poisson"`, or
#'   `"zeroinfl_negbin"`.
#' @return A numeric vector of randomized quantile residuals, length `n`.
#' @noRd
compute_rqr <- function(model,
                         family = c("poisson", "negbin",
                                    "zeroinfl_poisson", "zeroinfl_negbin")) {
  family <- match.arg(family)

  if (family == "poisson") {
    return(statmod::qres.pois(model))
  }

  if (family == "negbin") {
    return(statmod::qres.nbinom(model))
  }

  # Zero-inflated models: manual CDF-based RQR via pscl::predprob
  # predprob returns an n x K matrix of P(Y = k) for k = 0, 1, ..., max(y)
  pp  <- pscl::predprob(model)
  y   <- model$y
  cts <- as.integer(colnames(pp))
  n   <- length(y)

  cdf_y <- vapply(seq_len(n), function(i) {
    sum(pp[i, cts <= y[i]])
  }, numeric(1))

  cdf_ym1 <- vapply(seq_len(n), function(i) {
    if (y[i] == 0L) 0 else sum(pp[i, cts <= (y[i] - 1L)])
  }, numeric(1))

  u <- stats::runif(n, min = cdf_ym1, max = cdf_y)
  # Clamp away from exact 0/1 to avoid Inf from qnorm
  u <- pmin(pmax(u, 1e-10), 1 - 1e-10)
  stats::qnorm(u)
}

#' Build the two-panel RQR diagnostic plot
#'
#' Returns a patchwork of (1) fitted values vs RQR scatter and (2) normal QQ
#' plot of the RQR, with the dispersion ratio annotated in the scatter title.
#'
#' @param rqr Numeric vector of randomized quantile residuals.
#' @param fitted_vals Numeric vector of fitted (predicted) values.
#' @param dispersion_ratio Scalar; the Pearson dispersion ratio.
#' @return A `patchwork` / `gg` object.
#' @noRd
plot_diagnostics <- function(rqr, fitted_vals, dispersion_ratio) {
  df_diag <- data.frame(fitted = fitted_vals, rqr = rqr)

  disp_label <- if (is.na(dispersion_ratio)) {
    "Dispersion ratio: NA"
  } else {
    sprintf("Dispersion ratio: %.3f", dispersion_ratio)
  }

  p_scatter <- ggplot2::ggplot(df_diag, ggplot2::aes(x = .data$fitted,
                                                       y = .data$rqr)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::labs(
      x      = "Fitted values",
      y      = "Randomized quantile residuals",
      title  = disp_label
    ) +
    ggplot2::theme_bw()

  p_qq <- ggplot2::ggplot(df_diag, ggplot2::aes(sample = .data$rqr)) +
    ggplot2::stat_qq(alpha = 0.5) +
    ggplot2::stat_qq_line(color = "firebrick") +
    ggplot2::labs(
      x     = "Theoretical quantiles",
      y     = "Sample quantiles",
      title = "Normal Q-Q of RQR"
    ) +
    ggplot2::theme_bw()

  patchwork::wrap_plots(p_scatter, p_qq, ncol = 2)
}
