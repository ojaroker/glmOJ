# Internal diagnostic helpers for count regression models
# Not exported — used by poissonGLM, negbinGLM, zeroinflPoissonGLM, zeroinflNegbinGLM

#' Check minimum events-per-predictor for count (and ZI) components
#'
#' Issues a warning when the number of non-zero observations falls below
#' 10 per predictor in the count component, or the number of zeros falls below
#' 10 per predictor in the zero-inflation component.
#'
#' @param formula The count model formula (response on the left).
#' @param data The data frame.
#' @param ziformula One-sided formula (`~ x1 + x2`) for the ZI component, or
#'   `NULL` to skip the ZI check.
#' @return `NULL` invisibly; called for its side-effects (warnings).
#' @noRd
check_sample_size <- function(formula, data, ziformula = NULL) {
  mf <- model.frame(formula, data)
  y  <- model.response(mf)
  X  <- model.matrix(terms(mf), mf)

  n_pred   <- max(ncol(X) - 1L, 0L)
  n_events <- sum(y > 0)

  if (n_pred > 0L && n_events < 10L * n_pred) {
    warning(sprintf(
      "Count component: %d events (y > 0) for %d predictor(s) (%.1f per predictor). At least 10 events per predictor is recommended.",
      n_events, n_pred, n_events / n_pred
    ), call. = FALSE)
  }

  if (!is.null(ziformula)) {
    zi_mf     <- model.frame(ziformula, data)
    zi_X      <- model.matrix(terms(zi_mf), zi_mf)
    n_zi_pred <- max(ncol(zi_X) - 1L, 0L)
    n_zeros   <- sum(y == 0)

    if (n_zi_pred > 0L && n_zeros < 10L * n_zi_pred) {
      warning(sprintf(
        "Zero-inflation component: %d zeros for %d predictor(s) (%.1f per predictor). At least 10 zeros per ZI predictor is recommended.",
        n_zeros, n_zi_pred, n_zeros / n_zi_pred
      ), call. = FALSE)
    }
  }

  invisible(NULL)
}

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

#' Build the two-panel RQR diagnostic plot and the squared Pearson residual plot
#'
#' Returns a named list of two plots:
#' * `rqr_plot`: a patchwork of (1) fitted values vs RQR scatter and (2) a
#'   histo-QQ of the RQR, with the dispersion ratio annotated in the title.
#'   The title turns red and a subtitle warns of overdispersion when the ratio
#'   exceeds 1.2.
#' * `r2_plot`: squared Pearson residuals vs fitted values, with a horizontal
#'   reference line at 1 and a loess/GAM smooth. Useful for diagnosing
#'   mean-variance misspecification.
#'
#' @param rqr Numeric vector of randomized quantile residuals.
#' @param pearson_resid Numeric vector of Pearson residuals (same length as
#'   `rqr`).
#' @param fitted_vals Numeric vector of fitted (predicted) values.
#' @param dispersion_ratio Scalar; the Pearson dispersion ratio.
#' @return A named list with elements `rqr_plot` and `r2_plot` (both `gg`
#'   objects).
#' @noRd
plot_diagnostics <- function(rqr, pearson_resid, fitted_vals, dispersion_ratio) {
  df_diag <- data.frame(fitted = fitted_vals, rqr = rqr)

  overdispersed <- !is.na(dispersion_ratio) && dispersion_ratio > 1.2

  disp_label  <- if (is.na(dispersion_ratio)) {
    "Dispersion ratio: NA"
  } else {
    sprintf("Dispersion ratio: %.3f", dispersion_ratio)
  }

  disp_subtitle <- if (overdispersed) "Possible overdispersion (ratio > 1.2)" else NULL

  title_color <- if (overdispersed) "firebrick" else "black"

  p_scatter <- ggplot2::ggplot(df_diag, ggplot2::aes(x = .data$fitted,
                                                       y = .data$rqr)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::labs(
      x        = "Fitted values",
      y        = "Randomized quantile residuals",
      title    = disp_label,
      subtitle = disp_subtitle
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(color = title_color),
      plot.subtitle = ggplot2::element_text(color = title_color)
    )

  p_qq <- histoqq(rqr) +
    ggplot2::labs(title = "Histo-QQ of RQR")

  df_r2 <- data.frame(fitted = fitted_vals, r2 = pearson_resid^2)
  p_r2 <- ggplot2::ggplot(df_r2, ggplot2::aes(x = .data$fitted, y = .data$r2)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dotted", color = "firebrick") +
    ggplot2::geom_smooth(ggplot2::aes(x = .data$fitted, y = .data$r2),
                         color = "black", se = TRUE) +
    ggplot2::labs(
      x     = "Fitted values",
      y     = expression(r^2),
      title = "Squared Pearson residuals vs fitted"
    ) +
    ggplot2::theme_bw()

  list(
    rqr_plot = patchwork::wrap_plots(p_scatter, p_qq, ncol = 2),
    r2_plot  = p_r2
  )
}
