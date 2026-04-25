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

#' Return the complete-case subset used by a count / ZI fit
#' @noRd
subset_complete_cases <- function(formula, data, ziformula = NULL) {
  vars <- all.vars(formula)
  if (!is.null(ziformula)) vars <- union(vars, all.vars(ziformula))
  vars <- intersect(vars, names(data))

  if (length(vars) == 0L) {
    return(data)
  }

  keep <- stats::complete.cases(data[, vars, drop = FALSE])
  data[keep, , drop = FALSE]
}

#' Stop early on degenerate responses (all zero, all constant, all missing)
#'
#' Count GLMs cannot identify regression coefficients when the response has no
#' variation. We check up-front and stop with a clear message rather than
#' returning a fit whose coefficients are noise.
#' @noRd
check_degenerate_response <- function(formula, data, ziformula = NULL) {
  data_cc <- subset_complete_cases(formula, data, ziformula)
  if (nrow(data_cc) == 0L) {
    stop("No complete-case rows remain after dropping missing values.",
         call. = FALSE)
  }
  mf <- stats::model.frame(formula, data_cc)
  y  <- stats::model.response(mf)

  if (length(y) == 0L) {
    stop("Response is empty.", call. = FALSE)
  }
  if (any(!is.finite(y))) {
    stop("Response contains non-finite values.", call. = FALSE)
  }
  if (all(y == 0)) {
    stop(
      "Response is identically zero; a count regression model is not identifiable.",
      call. = FALSE
    )
  }
  if (length(unique(y)) == 1L) {
    stop(
      sprintf("Response is constant (all values equal %s); cannot fit a regression model.",
              format(y[1L])),
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Check minimum events-per-predictor for count (and ZI) components
#' @noRd
check_sample_size <- function(formula, data, ziformula = NULL) {
  data_cc <- subset_complete_cases(formula, data, ziformula)
  mf <- stats::model.frame(formula, data_cc)
  y <- stats::model.response(mf)
  X <- stats::model.matrix(stats::terms(mf), mf)

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
    zi_mf <- stats::model.frame(ziformula, data_cc)
    zi_X <- stats::model.matrix(stats::terms(zi_mf), zi_mf)
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
  # Guard: DHARMa simulates nsim * n draws from the fitted distribution.
  # With extreme fitted values (e.g. lambda > 1e8 for Poisson) this can
  # hang or exhaust memory, so bail out early with a missing result.
  max_fitted <- tryCatch(max(fitted(fit), na.rm = TRUE), error = function(e) NA_real_)
  if (!is.finite(max_fitted) || max_fitted > 1e8) {
    warning(sprintf(
      "%s: fitted values are extreme (max = %.2e); skipping DHARMa zero-inflation test.",
      model_type, max_fitted
    ), call. = FALSE)
    return(list(detected = NA, p_value = NA_real_, plot = NULL))
  }

  simres <- suppressMessages(
    DHARMa::simulateResiduals(fit, n = n_sim, plot = FALSE, refit = FALSE)
  )
  zi_result <- DHARMa::testZeroInflation(simres, plot = FALSE)

  p_val <- zi_result$p.value
  detected <- !is.na(p_val) && p_val < 0.05

  # Simulated zero counts (one per replicate); observed zero count
  sim_counts <- colSums(simres$simulatedResponse == 0)
  # fit$y works for glm/glm.nb/zeroinfl; for glmmTMB use fit$frame[[1]]
  obs_count <- tryCatch({
    y_raw <- fit$y
    if (!is.null(y_raw) && length(y_raw) > 0L) {
      sum(y_raw == 0)
    } else {
      y_frame <- fit$frame[[1L]]
      if (is.null(y_frame)) NA_integer_ else sum(y_frame == 0)
    }
  }, error = function(e) {
    y <- tryCatch(fit$frame[[1L]], error = function(e2) NULL)
    if (is.null(y)) NA_integer_ else sum(y == 0)
  })

  # Place label left of line when observed is in the upper half of the range
  # to avoid clipping at the right plot edge. Guard against NA obs_count and
  # simulated vectors containing NA.
  sim_median <- tryCatch(stats::median(sim_counts, na.rm = TRUE),
                         error = function(e) NA_real_)
  label_hjust <- if (isTRUE(!is.na(obs_count) && !is.na(sim_median) &&
                            obs_count > sim_median)) 1.15 else -0.15

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
      label = if (is.na(obs_count)) "observed\nNA" else sprintf("observed\n%d", obs_count),
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
  # model$df.residual is NULL for glmmTMB objects; fall back to the generic
  df_resid <- tryCatch(
    {
      dr <- model$df.residual
      if (is.null(dr) || length(dr) == 0L) stats::df.residual(model) else dr
    },
    error = function(e) NULL
  )
  if (is.null(df_resid) || length(df_resid) == 0L || df_resid == 0) {
    return(NA_real_)
  }
  pearson_resid <- tryCatch(
    residuals(model, type = "pearson"),
    error = function(e) NULL
  )
  if (is.null(pearson_resid)) return(NA_real_)
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
  family = c("poisson", "negbin", "tweedie",
             "zeroinfl_poisson", "zeroinfl_negbin", "zeroinfl_tweedie")
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

  # Tweedie (glmmTMB): DHARMa simulation-based quantile residuals → normal scale
  if (family %in% c("tweedie", "zeroinfl_tweedie")) {
    max_fitted <- tryCatch(max(fitted(model), na.rm = TRUE), error = function(e) NA_real_)
    if (!is.finite(max_fitted) || max_fitted > 1e8) {
      n_obs <- tryCatch(length(fitted(model)), error = function(e) 0L)
      rqr <- rep(NA_real_, n_obs)
      .warn_nonfinite_rqr(rqr)
      return(rqr)
    }
    simres <- suppressMessages(
      DHARMa::simulateResiduals(model, n = 500L, plot = FALSE, refit = FALSE)
    )
    rqr <- stats::qnorm(pmin(pmax(simres$scaledResiduals, 1e-10), 1 - 1e-10))
    .warn_nonfinite_rqr(rqr)
    return(rqr)
  }

  # Zero-inflated (pscl): manual CDF-based RQR via pscl::predprob
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
#' @param dispersion_threshold Numeric; dispersion ratios above this value are
#'   flagged as overdispersed (title/subtitle rendered in red). Default 1.2.
#' @noRd
plot_diagnostics <- function(
  rqr,
  pearson_resid,
  fitted_vals,
  dispersion_ratio,
  dispersion_threshold = 1.2
) {
  df_diag <- data.frame(fitted = fitted_vals, rqr = rqr)

  overdispersed <- !is.na(dispersion_ratio) &&
    dispersion_ratio > dispersion_threshold

  disp_label <- if (is.na(dispersion_ratio)) {
    "Dispersion ratio: NA"
  } else {
    sprintf("Dispersion ratio: %.3f", dispersion_ratio)
  }

  disp_subtitle <- if (overdispersed) {
    sprintf("Possible overdispersion (ratio > %g)", dispersion_threshold)
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

#' Randomized quantile residuals for a quasi-Poisson fit.
#'
#' Uses the Poisson CDF with the fitted means. Quasi-Poisson shares the Poisson
#' mean structure, so these RQRs are the appropriate diagnostic for checking
#' the conditional mean — they match the RQRs of a Poisson fit with the same
#' coefficients. `statmod::qres.pois()` refuses to run on a quasi family, so
#' we inline the same computation here.
#' @noRd
compute_rqr_quasipoisson <- function(fit) {
  mu <- fit$fitted.values
  y  <- fit$y
  if (is.null(y)) y <- stats::model.response(stats::model.frame(fit))
  n  <- length(y)
  a  <- stats::ppois(pmax(y - 1L, 0L), mu)
  a[y == 0L] <- 0
  b  <- stats::ppois(y, mu)
  a  <- pmin(a, b)
  u  <- stats::runif(n, min = a, max = b)
  u  <- pmin(pmax(u, 1e-10), 1 - 1e-10)
  rqr <- stats::qnorm(u)
  .warn_nonfinite_rqr(rqr)
  rqr
}

#' Detect whether a quasi-Poisson model is appropriate given a Poisson fit.
#'
#' Returns `TRUE` when (a) the Pearson dispersion ratio exceeds 1.2 and (b)
#' the squared Pearson residuals form a roughly flat cloud across the range
#' of fitted values whose central level is not 1 (i.e. approximately
#' constant `Var(Y)/mu = phi > 1`, the quasi-Poisson signature, rather than
#' a fan pattern that would motivate negative binomial).
#'
#' Flatness is assessed by regressing squared Pearson residuals on fitted
#' values and requiring the absolute slope to be small relative to the mean
#' r^2 (|slope * range(fitted)| < 0.5 * mean(r^2)).
#' @noRd
is_quasipoisson_appropriate <- function(pois_fit) {
  disp <- tryCatch(pois_fit$diagnostics$dispersion_ratio, error = function(e) NA_real_)
  if (is.null(disp) || is.na(disp) || disp <= 1.2) return(FALSE)

  fitted_vals <- tryCatch(pois_fit$model$fitted.values, error = function(e) NULL)
  pearson     <- tryCatch(residuals(pois_fit$model, type = "pearson"),
                          error = function(e) NULL)
  if (is.null(fitted_vals) || is.null(pearson) || length(fitted_vals) < 3L) {
    return(FALSE)
  }

  r2 <- pearson^2
  ok <- is.finite(fitted_vals) & is.finite(r2)
  if (sum(ok) < 3L) return(FALSE)
  fv <- fitted_vals[ok]
  r2 <- r2[ok]

  fv_range <- diff(range(fv))
  mean_r2  <- mean(r2)
  if (fv_range <= 0 || mean_r2 <= 0) return(FALSE)

  slope <- tryCatch(
    stats::coef(stats::lm(r2 ~ fv))[2L],
    error = function(e) NA_real_
  )
  if (is.na(slope)) return(FALSE)

  # Flat: total change across fitted range < half the mean r^2
  flat <- abs(slope * fv_range) < 0.5 * mean_r2
  # Not at 1: mean level substantially above 1 (we already know disp > 1.2)
  not_at_one <- mean_r2 > 1.2

  isTRUE(flat && not_at_one)
}

#' Compute (generalized) VIF on main-effect terms only, avoiding false
#' positives from structural collinearity (interaction terms, polynomial
#' terms, etc.)
#'
#' Extracts terms of order 1 from `formula`, fits an OLS model on those
#' predictors, and returns generalized VIF values (Fox & Monette 1992) for
#' each term. For single-column (continuous, or two-level factor) terms this
#' reduces to the usual VIF. For multi-column terms (factors with >2 levels)
#' it reports GVIF along with its degrees of freedom and the comparable
#' scalar GVIF^{1/(2*df)}. Warns when any term's GVIF^{1/(2*df)} exceeds
#' sqrt(5) (the GVIF analogue of the VIF > 5 rule of thumb).
#'
#' @param formula The count-component formula.
#' @param data The data frame.
#' @return A data frame with one row per main-effect term, columns
#'   `GVIF`, `Df`, and `GVIF^(1/(2*Df))`; or `NULL` if fewer than two
#'   main-effect terms are present. For backwards compatibility the returned
#'   object also carries an attribute `"vif"` giving the single-df GVIF values
#'   as a named numeric vector (equal to ordinary VIFs when each term has one
#'   column).
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

  # Map each column of X back to its originating main-effect term via the
  # assign attribute (0 = intercept, 1..k = term index in term.labels).
  assign_idx <- attr(stats::model.matrix(fit_ols), "assign")
  assign_idx <- assign_idx[assign_idx > 0L]
  all_terms  <- attr(stats::terms(fit_ols), "term.labels")
  col_term   <- all_terms[assign_idx]

  R <- tryCatch(stats::cor(X), error = function(e) NULL)
  if (is.null(R)) return(NULL)

  detR <- tryCatch(det(R), error = function(e) NA_real_)
  if (!is.finite(detR) || detR <= 0) return(NULL)

  unique_terms <- unique(col_term)
  gvif_rows <- lapply(unique_terms, function(tm) {
    in_term <- col_term == tm
    df_tm   <- sum(in_term)
    if (df_tm == ncol(X)) {
      # Degenerate: only this term in the design; GVIF is 1 by definition.
      return(data.frame(term = tm, GVIF = 1, Df = df_tm,
                        `GVIF^(1/(2*Df))` = 1,
                        check.names = FALSE, stringsAsFactors = FALSE))
    }
    R11 <- R[in_term, in_term, drop = FALSE]
    R22 <- R[!in_term, !in_term, drop = FALSE]
    d11 <- tryCatch(det(R11), error = function(e) NA_real_)
    d22 <- tryCatch(det(R22), error = function(e) NA_real_)
    if (!is.finite(d11) || !is.finite(d22) || d11 <= 0 || d22 <= 0) {
      return(data.frame(term = tm, GVIF = NA_real_, Df = df_tm,
                        `GVIF^(1/(2*Df))` = NA_real_,
                        check.names = FALSE, stringsAsFactors = FALSE))
    }
    gvif <- (d11 * d22) / detR
    data.frame(
      term              = tm,
      GVIF              = gvif,
      Df                = df_tm,
      `GVIF^(1/(2*Df))` = gvif^(1 / (2 * df_tm)),
      check.names       = FALSE,
      stringsAsFactors  = FALSE
    )
  })
  out <- do.call(rbind, gvif_rows)
  rownames(out) <- out$term

  # VIF analogue threshold: GVIF^(1/(2*Df)) > sqrt(5).
  scaled <- out[["GVIF^(1/(2*Df))"]]
  high   <- out$term[is.finite(scaled) & scaled > sqrt(5)]
  if (length(high) > 0L) {
    warning(
      sprintf(
        "High (G)VIF detected (GVIF^(1/(2*Df)) > sqrt(5)) for term(s): %s. Multicollinearity may affect estimates.",
        paste(high, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Backwards-compatible scalar vector: names match term labels.
  vif_compat <- stats::setNames(out$GVIF, out$term)
  attr(out, "vif") <- vif_compat
  out
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
