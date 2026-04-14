# Internal diagnostic helpers for count regression models
# Not exported — used by poissonGLM, negbinGLM, zeroinflPoissonGLM, zeroinflNegbinGLM

#' Warn when NA rows will be silently dropped during model fitting
#' @noRd
check_na_rows <- function(formula, data, ziformula = NULL) {
  vars <- all.vars(formula)
  if (!is.null(ziformula)) vars <- union(vars, all.vars(ziformula))
  vars <- intersect(vars, names(data))

  n_dropped <- sum(!stats::complete.cases(data[, vars, drop = FALSE]))
  if (n_dropped > 0L) {
    warning(
      sprintf(
        "%d row(s) contain missing values and will be dropped before fitting.",
        n_dropped
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Check minimum events-per-predictor for count (and ZI) components
#' @noRd
check_sample_size <- function(formula, data, ziformula = NULL) {
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  X <- model.matrix(terms(mf), mf)

  n_pred <- max(ncol(X) - 1L, 0L)
  n_events <- sum(y > 0)

  if (n_pred > 0L && n_events < 10L * n_pred) {
    warning(
      sprintf(
        "Count component: %d events (y > 0) for %d predictor(s) (%.1f per predictor). At least 10 events per predictor is recommended.",
        n_events,
        n_pred,
        n_events / n_pred
      ),
      call. = FALSE
    )
  }

  if (!is.null(ziformula)) {
    zi_mf <- model.frame(ziformula, data)
    zi_X <- model.matrix(terms(zi_mf), zi_mf)
    n_zi_pred <- max(ncol(zi_X) - 1L, 0L)
    n_zeros <- sum(y == 0)

    if (n_zi_pred > 0L && n_zeros < 10L * n_zi_pred) {
      warning(
        sprintf(
          "Zero-inflation component: %d zeros for %d predictor(s) (%.1f per predictor). At least 10 zeros per ZI predictor is recommended.",
          n_zeros,
          n_zi_pred,
          n_zeros / n_zi_pred
        ),
        call. = FALSE
      )
    }
  }

  invisible(NULL)
}

#' Run DHARMa zero-inflation test and return result with a ggplot
#'
#' Simulates residuals via DHARMa::simulateResiduals(), applies
#' DHARMa::testZeroInflation(), and returns the p-value, a detection flag,
#' and a frequency histogram of simulated zero counts vs the observed count.
#'
#' @param fit A fitted model (Poisson or negative-binomial GLM).
#' @param model_type Character label shown in the plot title, e.g. "Poisson".
#' @param n_sim Integer; number of simulations. Default 1000.
#' @return Named list: detected (logical), p_value (numeric), plot (ggplot).
#' @noRd
run_dharma_zi_test <- function(fit, model_type = "Model", n_sim = 1000L) {
  simres <- suppressMessages(
    DHARMa::simulateResiduals(fit, n = n_sim, plot = FALSE, refit = FALSE)
  )
  zi_result <- DHARMa::testZeroInflation(simres, plot = FALSE)

  p_val <- zi_result$p.value
  detected <- !is.na(p_val) && p_val < 0.05

  # Simulated zero counts (one per replicate); observed zero count
  sim_counts <- colSums(simres$simulatedResponse == 0)
  obs_count <- sum(fit$y == 0)

  # Place label left of line when observed is in the upper half of the range
  # to avoid clipping at the right plot edge
  label_hjust <- if (obs_count > stats::median(sim_counts)) 1.15 else -0.15

  subtitle_text <- if (detected) {
    "Significant zero-inflation detected (p < 0.05) \u2014 consider a ZI model"
  } else {
    "No significant zero-inflation (p \u2265 0.05)"
  }

  p <- ggplot2::ggplot(
    data.frame(sim_counts = sim_counts),
    ggplot2::aes(x = .data$sim_counts)
  ) +
    ggplot2::geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    ggplot2::geom_vline(
      xintercept = obs_count,
      color = "firebrick",
      linetype = "dashed",
      linewidth = 1
    ) +
    ggplot2::geom_hline(
      yintercept = 0
    ) +
    ggplot2::annotate(
      "text",
      x = obs_count,
      y = Inf,
      label = sprintf("observed\n%d", obs_count),
      hjust = label_hjust,
      vjust = 1.4,
      color = "firebrick",
      size = 3
    ) +
    ggplot2::labs(
      title = sprintf(
        "%s \u2014 DHARMa zero-inflation test (p = %.3f)",
        model_type,
        p_val
      ),
      subtitle = subtitle_text,
      x = "Simulated zero count",
      y = "Frequency"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(
        color = if (detected) "firebrick" else "black"
      )
    )

  list(detected = detected, p_value = p_val, plot = p)
}

#' Compute the Pearson dispersion ratio
#' @noRd
check_dispersion <- function(model) {
  df_resid <- model$df.residual
  if (is.null(df_resid) || df_resid == 0) {
    return(NA_real_)
  }
  pearson_resid <- residuals(model, type = "pearson")
  sum(pearson_resid^2) / df_resid
}

#' @noRd
.warn_nonfinite_rqr <- function(rqr) {
  n_bad <- sum(!is.finite(rqr))
  if (n_bad > 0L) {
    warning(
      sprintf(
        "%d observation(s) produced non-finite randomized quantile residuals and will be excluded from diagnostic plots.",
        n_bad
      ),
      call. = FALSE
    )
  }
}

#' Compute randomized quantile residuals
#' @noRd
compute_rqr <- function(
  model,
  family = c("poisson", "negbin", "zeroinfl_poisson", "zeroinfl_negbin")
) {
  family <- match.arg(family)

  if (family == "poisson") {
    rqr <- suppressWarnings(statmod::qres.pois(model))
    .warn_nonfinite_rqr(rqr)
    return(rqr)
  }
  if (family == "negbin") {
    rqr <- suppressWarnings(statmod::qres.nbinom(model))
    .warn_nonfinite_rqr(rqr)
    return(rqr)
  }

  # Zero-inflated: manual CDF-based RQR via pscl::predprob
  pp <- pscl::predprob(model)
  y <- model$y
  cts <- as.integer(colnames(pp))
  n <- length(y)

  cdf_y <- vapply(
    seq_len(n),
    function(i) {
      sum(pp[i, cts <= y[i]])
    },
    numeric(1)
  )

  cdf_ym1 <- vapply(
    seq_len(n),
    function(i) {
      if (y[i] == 0L) 0 else sum(pp[i, cts <= (y[i] - 1L)])
    },
    numeric(1)
  )

  # Guard against floating-point cases where cdf_ym1 slightly exceeds cdf_y
  cdf_ym1 <- pmin(cdf_ym1, cdf_y)
  u <- stats::runif(n, min = cdf_ym1, max = cdf_y)
  u <- pmin(pmax(u, 1e-10), 1 - 1e-10)
  stats::qnorm(u)
}

#' Build the two-panel RQR diagnostic plot and the squared Pearson residual plot
#' @noRd
plot_diagnostics <- function(
  rqr,
  pearson_resid,
  fitted_vals,
  dispersion_ratio
) {
  df_diag <- data.frame(fitted = fitted_vals, rqr = rqr)

  overdispersed <- !is.na(dispersion_ratio) && dispersion_ratio > 1.2

  disp_label <- if (is.na(dispersion_ratio)) {
    "Dispersion ratio: NA"
  } else {
    sprintf("Dispersion ratio: %.3f", dispersion_ratio)
  }

  disp_subtitle <- if (overdispersed) {
    "Possible overdispersion (ratio > 1.2)"
  } else {
    NULL
  }
  title_color <- if (overdispersed) "firebrick" else "black"

  p_scatter <- ggplot2::ggplot(
    df_diag,
    ggplot2::aes(x = .data$fitted, y = .data$rqr)
  ) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::labs(
      x = "Fitted values",
      y = "Randomized quantile residuals",
      title = disp_label,
      subtitle = disp_subtitle
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = title_color),
      plot.subtitle = ggplot2::element_text(color = title_color)
    )

  p_qq <- histoqq(rqr) +
    ggplot2::labs(title = "Histo-QQ of RQR")

  df_r2 <- data.frame(fitted = fitted_vals, r2 = pearson_resid^2)
  p_r2 <- ggplot2::ggplot(df_r2, ggplot2::aes(x = .data$fitted, y = .data$r2)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_hline(
      yintercept = 1,
      linetype = "dotted",
      color = "firebrick"
    ) +
    ggplot2::geom_smooth(
      ggplot2::aes(x = .data$fitted, y = .data$r2),
      color = "black",
      se = TRUE
    ) +
    ggplot2::labs(
      x = "Fitted values",
      y = expression(r^2),
      title = "Squared Pearson residuals vs fitted"
    ) +
    ggplot2::theme_bw()

  list(
    rqr_plot = patchwork::wrap_plots(p_scatter, p_qq, ncol = 2),
    r2_plot = p_r2
  )
}

#' Compute VIF on main-effect terms only, avoiding false positives from
#' structural collinearity (interaction terms, polynomial terms, etc.)
#'
#' Extracts terms of order 1 from `formula`, fits an OLS model on those
#' predictors, and returns VIF values computed from the correlation matrix
#' of the design matrix. Warns if any VIF exceeds 5.
#'
#' @param formula The count-component formula.
#' @param data The data frame.
#' @return Named numeric vector of VIF values, or `NULL` if fewer than two
#'   main-effect predictors are present.
#' @noRd
check_vif <- function(formula, data) {
  tt           <- stats::terms(formula, data = data)
  term_labels  <- attr(tt, "term.labels")
  term_orders  <- attr(tt, "order")

  # Keep only order-1 terms (main effects); this excludes x1:x2, I(x^2), etc.
  main_labels <- term_labels[term_orders == 1L]

  if (length(main_labels) < 2L) {
    return(NULL)
  }

  response     <- deparse(formula[[2L]])
  main_formula <- stats::as.formula(
    paste(response, "~", paste(main_labels, collapse = " + "))
  )

  fit_ols <- tryCatch(
    stats::lm(main_formula, data = data),
    error = function(e) NULL
  )
  if (is.null(fit_ols)) return(NULL)

  X <- stats::model.matrix(fit_ols)[, -1L, drop = FALSE]
  if (ncol(X) < 2L) return(NULL)

  R    <- stats::cor(X)
  vifs <- tryCatch(diag(solve(R)), error = function(e) NULL)
  if (is.null(vifs)) return(NULL)

  high <- names(vifs)[vifs > 5]
  if (length(high) > 0L) {
    warning(
      sprintf(
        "High VIF detected (> 5) for predictor(s): %s. Multicollinearity may affect estimates.",
        paste(high, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  vifs
}

#' Convert p-values to significance stars
#' @noRd
sig_stars <- function(p) {
  cut(
    p,
    breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
    labels = c("***", "**", "*", ".", ""),
    right = TRUE
  )
}
