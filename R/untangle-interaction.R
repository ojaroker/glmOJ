#' Untangle a two-way interaction in a fitted model
#'
#' Produces post-hoc marginal means and/or slopes that describe how two
#' interacting predictors jointly shape the fitted model. The behaviour
#' adapts to the types of the two variables:
#' \itemize{
#'   \item **categorical x categorical:** estimated marginal means
#'     ([emmeans::emmeans()]) for every combination of the two factors, plus
#'     a faceted ggplot of those means with 95% error bars. Faceting is by
#'     any categorical predictor not listed in `average.over`. The plot is
#'     suppressed (with a warning) when the number of facets exceeds 8,
#'     unless `overridePlot = TRUE`.
#'   \item **categorical x continuous:** slopes of the continuous predictor at
#'     each level of the factor ([emmeans::emtrends()]), plus a faceted
#'     ggplot of predicted responses across `n` equally-spaced values of the
#'     continuous predictor. Faceting is by any categorical predictor not
#'     listed in `average.over`. The plot is suppressed (with a warning) when
#'     the number of facets exceeds 8, unless `overridePlot = TRUE`.
#'   \item **continuous x continuous:** slopes of each predictor at low,
#'     medium and high values of the other ([emmeans::emtrends()]), plus
#'     Johnson-Neyman intervals in both directions
#'     ([interactions::johnson_neyman()]).
#' }
#'
#' By default, the function never averages over *categorical* predictors
#' outside the interaction of interest: results are reported separately for
#' each level. Predictors listed in `average.over` are averaged over instead.
#' Continuous predictors outside the interaction are always held at their
#' mean (the emmeans default) and therefore implicitly averaged in a linear
#' sense.
#'
#' @param model A fitted model. Either an object returned by [countGLM()] or
#'   one of the package's individual fitters ([poissonGLM()], [negbinGLM()],
#'   [tweedieGLM()], [zeroinflPoissonGLM()], [zeroinflNegbinGLM()],
#'   [zeroinflTweedieGLM()]), or a raw `glm` / `glmmTMB` fit. For `countGLM`
#'   objects the best-fitting component model is used and a message is
#'   printed.
#' @param interaction The pair of interacting variables. Either a length-2
#'   character vector (e.g. `c("x7", "x8")`) or a one-sided formula
#'   (e.g. `~ x7:x8` or `~ x7*x8`).
#' @param average.over Character vector of *categorical* predictor names that
#'   may be averaged over by emmeans. Default `NULL` means do not average
#'   over any categorical predictor; each level is reported separately.
#' @param standardized Logical; set `TRUE` when the continuous predictors
#'   have been standardized to mean 0 and SD 1. In that case the low /
#'   medium / high values used for continuous x continuous slopes are
#'   `c(-1, 0, 1)` (standard deviations), and the interpretation text refers
#'   to a "one-standard-deviation increase" rather than a "one-unit
#'   increase". Default `FALSE`.
#' @param n Integer; number of equally-spaced points used for the continuous
#'   predictor in the categorical x continuous emmeans grid backing the
#'   plot. Default 100.
#' @param overridePlot Logical; when `TRUE`, the faceted plot in the
#'   categorical x categorical and categorical x continuous cases is built
#'   and returned even if the number of facets exceeds 8. Default `FALSE`.
#'
#' @return A list with class `"untangle_interaction"` whose contents depend
#'   on the interaction type:
#'   \describe{
#'     \item{`type`}{One of `"categorical-categorical"`,
#'       `"categorical-continuous"`, `"continuous-continuous"`.}
#'     \item{`variables`}{Character vector of the two predictor names.}
#'     \item{`emmeans`}{(cat x cat) data frame with columns for each factor,
#'       `Mean`, `SE`, `df`, `Lower`, `Upper`.}
#'     \item{`plot`}{(cat x cat) faceted ggplot of estimated means with 95%
#'       error bars. `NULL` when the number of facets exceeds 8 and
#'       `overridePlot = FALSE`.}
#'     \item{`emtrends`}{(cat x cont) data frame with `Slope`, `SE`, `df`,
#'       `Lower`, `Upper` at each level of the factor.}
#'     \item{`plot`}{(cat x cont) faceted ggplot of predicted responses vs
#'       the continuous predictor, coloured by the factor, with a 95%
#'       confidence ribbon. `NULL` when the number of facets exceeds 8 and
#'       `overridePlot = FALSE`.}
#'     \item{`emtrends_<v1>`, `emtrends_<v2>`}{(cont x cont) data frames of
#'       slopes of each predictor at low/medium/high values of the other.}
#'     \item{`johnson_neyman_<v1>_by_<v2>`, `johnson_neyman_<v2>_by_<v1>`}{(cont
#'       x cont) Johnson-Neyman objects from [interactions::johnson_neyman()].
#'       `NULL` if the call failed (e.g. unsupported model class).}
#'     \item{`interpretation`}{A plain-language description, also printed.}
#'   }
#'
#' @details
#' `emmeans` and `interactions` are declared in `Suggests`; both must be
#' installed before calling this function.
#'
#' All emmeans summaries are returned on the response scale when a known
#' link is present. Slopes from `emtrends` are returned on the **linear
#' predictor (link) scale** — on a log link, a positive slope means the
#' expected response grows multiplicatively with the predictor.
#'
#' @seealso [interpret_coef()], [emmeans::emmeans()], [emmeans::emtrends()],
#'   [interactions::johnson_neyman()]
#'
#' @examples
#' \dontrun{
#' fit <- poissonGLM(y ~ x1 * x2 + x3, data = df)
#' untangle_interaction(fit, c("x1", "x2"))
#' untangle_interaction(fit, ~ x1:x2, average.over = "x3")
#' untangle_interaction(fit, c("x7", "x8"), standardized = TRUE)
#' }
#' @export
untangle_interaction <- function(model, interaction,
                                  average.over = NULL,
                                  standardized = FALSE,
                                  n = 100,
                                  overridePlot = FALSE) {
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("Package 'emmeans' is required. Install it with install.packages('emmeans').",
         call. = FALSE)
  }
  stopifnot(
    "`standardized` must be TRUE or FALSE"   = isTRUE(standardized) || isFALSE(standardized),
    "`overridePlot` must be TRUE or FALSE"   = isTRUE(overridePlot) || isFALSE(overridePlot),
    "`n` must be a positive integer"         = is.numeric(n) && length(n) == 1L && n >= 2
  )

  vars <- .parse_interaction(interaction)
  raw  <- .extract_raw_model(model)
  mf   <- tryCatch(stats::model.frame(raw),
                   error = function(e)
                     stop("Could not extract model frame from the fitted model: ",
                          conditionMessage(e), call. = FALSE))

  missing_vars <- setdiff(vars, names(mf))
  if (length(missing_vars) > 0L) {
    stop(sprintf(
      "Variable(s) not found in the model: %s.\nAvailable: %s",
      paste(missing_vars, collapse = ", "),
      paste(names(mf), collapse = ", ")
    ), call. = FALSE)
  }

  types <- vapply(vars, function(v) .predictor_type(mf[[v]]), character(1L))
  if (any(types == "other")) {
    stop("Interaction variables must be categorical (factor/character/logical) ",
         "or continuous (numeric).", call. = FALSE)
  }

  # Categorical predictors outside the interaction: by default none are averaged over
  resp_name   <- .response_name_from_formula(raw)
  other_preds <- setdiff(names(mf), c(vars, resp_name))
  other_cats  <- other_preds[vapply(other_preds,
                                    function(v) .predictor_type(mf[[v]]) == "categorical",
                                    logical(1L))]
  unknown_avg <- setdiff(average.over, other_cats)
  if (length(unknown_avg) > 0L) {
    warning(sprintf(
      "`average.over` contains names that are not categorical predictors in the model: %s.",
      paste(unknown_avg, collapse = ", ")
    ), call. = FALSE)
  }
  keep_cats <- setdiff(other_cats, average.over)

  kind <- paste(sort(types), collapse = "-")
  result <- switch(kind,
    "categorical-categorical" =
      .untangle_cat_cat(raw, vars, keep_cats, mf, overridePlot),
    "categorical-continuous"  =
      .untangle_cat_cont(raw, vars, types, keep_cats, mf, standardized, n,
                         overridePlot),
    "continuous-continuous"   =
      .untangle_cont_cont(raw, vars, keep_cats, mf, standardized)
  )

  class(result) <- c("untangle_interaction", "list")
  cat(result$interpretation, "\n", sep = "")
  invisible(result)
}

# ---------------------------------------------------------------------------
# Input helpers
# ---------------------------------------------------------------------------

.parse_interaction <- function(interaction) {
  if (inherits(interaction, "formula")) {
    vars <- all.vars(interaction)
  } else if (is.character(interaction)) {
    if (length(interaction) == 1L) {
      vars <- trimws(unlist(strsplit(interaction, "[:*]")))
      vars <- vars[nzchar(vars)]
    } else {
      vars <- interaction
    }
  } else {
    stop("`interaction` must be a length-2 character vector or a formula.",
         call. = FALSE)
  }
  if (length(vars) != 2L) {
    stop("`interaction` must refer to exactly two variables.", call. = FALSE)
  }
  vars
}

.extract_raw_model <- function(obj) {
  if (inherits(obj, "countGLM")) {
    fit <- obj$fits[[obj$best_model]]
    message("Using best model: ", obj$best_model)
    return(fit$model)
  }
  if (inherits(obj, "countGLMfit") || inherits(obj, "zeroinflGLMfit")) {
    return(obj$model)
  }
  obj
}

.predictor_type <- function(col) {
  if (is.factor(col) || is.character(col) || is.logical(col)) "categorical"
  else if (is.numeric(col))                                   "continuous"
  else                                                         "other"
}

.response_name_from_formula <- function(fit) {
  tryCatch(all.vars(stats::formula(fit))[1L], error = function(e) NA_character_)
}

# ---------------------------------------------------------------------------
# emmeans / emtrends column renaming
# ---------------------------------------------------------------------------

.rename_emmeans_df <- function(df) {
  nm <- names(df)
  for (cand in c("emmean", "response", "rate", "prob")) {
    nm[nm == cand] <- "Mean"
  }
  nm[nm == "lower.CL"]  <- "Lower"
  nm[nm == "upper.CL"]  <- "Upper"
  nm[nm == "asymp.LCL"] <- "Lower"
  nm[nm == "asymp.UCL"] <- "Upper"
  names(df) <- nm
  df
}

.rename_emtrends_df <- function(df, trend_var) {
  nm <- names(df)
  tcol <- paste0(trend_var, ".trend")
  nm[nm == tcol]        <- "Slope"
  nm[nm == "lower.CL"]  <- "Lower"
  nm[nm == "upper.CL"]  <- "Upper"
  nm[nm == "asymp.LCL"] <- "Lower"
  nm[nm == "asymp.UCL"] <- "Upper"
  names(df) <- nm
  df
}

.emm_to_response_df <- function(emm) {
  df <- tryCatch(
    as.data.frame(summary(emm, type = "response")),
    error = function(e) as.data.frame(summary(emm))
  )
  .rename_emmeans_df(df)
}

.emtrends_to_df <- function(emt, trend_var) {
  df <- as.data.frame(summary(emt, infer = c(TRUE, TRUE)))
  .rename_emtrends_df(df, trend_var)
}

# ---------------------------------------------------------------------------
# Categorical x categorical
# ---------------------------------------------------------------------------

.untangle_cat_cat <- function(raw, vars, keep_cats, mf, overridePlot) {
  specs_vars <- c(vars, keep_cats)
  specs <- stats::as.formula(paste("~", paste(specs_vars, collapse = " * ")))

  emm <- emmeans::emmeans(raw, specs = specs)
  emm_df <- .emm_to_response_df(emm)

  n_facets <- if (length(keep_cats) == 0L) 1L
              else prod(vapply(keep_cats,
                               function(v) length(unique(mf[[v]])),
                               integer(1L)))

  plot_obj <- NULL
  if (n_facets > 8L && !overridePlot) {
    warning(sprintf(
      "Faceted plot would contain %d panels (> 8); plot not returned. Set overridePlot = TRUE to build it anyway.",
      n_facets
    ), call. = FALSE)
  } else {
    plot_df <- emm_df
    plot_df$Observation <- paste0(
      "(", vars[1L], "=", plot_df[[vars[1L]]], ", ",
      vars[2L], "=", plot_df[[vars[2L]]], ")"
    )

    p <- ggplot2::ggplot(plot_df) +
      ggplot2::geom_point(ggplot2::aes(x = .data$Mean, y = .data$Observation)) +
      ggplot2::geom_errorbar(ggplot2::aes(y = .data$Observation,
                                          xmin = .data$Lower,
                                          xmax = .data$Upper),
                             width = 0.25) +
      ggplot2::labs(x = "Estimated mean", y = NULL) +
      ggplot2::theme_bw()

    if (length(keep_cats) > 0L) {
      p <- p + ggplot2::facet_wrap(
        stats::as.formula(paste("~", paste(keep_cats, collapse = " + ")))
      )
    }
    plot_obj <- p
  }

  plot_note <- if (is.null(plot_obj))
    sprintf(
      " Plot suppressed: %d facet panels exceed the limit of 8. Re-run with overridePlot = TRUE to force it.",
      n_facets
    )
  else
    " 'plot' contains a faceted ggplot of the means with 95% error bars."

  interp <- sprintf(
    paste0(
      "Interaction %s x %s (categorical x categorical).\n",
      "'emmeans' holds estimated marginal means for every combination of the ",
      "two factors%s. Columns: Mean = EMM on the response scale, Lower/Upper ",
      "= 95%% confidence bounds.%s"
    ),
    vars[1L], vars[2L],
    if (length(keep_cats) > 0L)
      sprintf(" and of %s (not averaged over)", paste(keep_cats, collapse = ", "))
    else "",
    plot_note
  )

  list(type          = "categorical-categorical",
       variables     = vars,
       emmeans       = emm_df,
       plot          = plot_obj,
       interpretation = interp)
}

# ---------------------------------------------------------------------------
# Categorical x continuous
# ---------------------------------------------------------------------------

.untangle_cat_cont <- function(raw, vars, types, keep_cats, mf, standardized,
                                n, overridePlot) {
  cat_var  <- vars[types == "categorical"][1L]
  cont_var <- vars[types == "continuous"][1L]

  # Slopes of the continuous predictor at each level of the factor
  trend_specs <- stats::as.formula(paste("~", paste(c(cat_var, keep_cats),
                                                     collapse = " * ")))
  emt    <- emmeans::emtrends(raw, specs = trend_specs, var = cont_var)
  emt_df <- .emtrends_to_df(emt, cont_var)

  # Number of facets = product of level counts for keep_cats (1 when none)
  n_facets <- if (length(keep_cats) == 0L) 1L
              else prod(vapply(keep_cats,
                               function(v) length(unique(mf[[v]])),
                               integer(1L)))

  plot_obj <- NULL
  if (n_facets > 8L && !overridePlot) {
    warning(sprintf(
      "Faceted plot would contain %d panels (> 8); plot not returned. Set overridePlot = TRUE to build it anyway.",
      n_facets
    ), call. = FALSE)
  } else {
    cont_vals  <- seq(min(mf[[cont_var]], na.rm = TRUE),
                      max(mf[[cont_var]], na.rm = TRUE),
                      length.out = as.integer(n))
    grid_specs <- stats::as.formula(paste("~", paste(c(vars, keep_cats),
                                                      collapse = " * ")))
    emm    <- emmeans::emmeans(raw, specs = grid_specs,
                               at = stats::setNames(list(cont_vals), cont_var))
    emm_df <- .emm_to_response_df(emm)

    p <- ggplot2::ggplot(emm_df) +
      ggplot2::geom_line(ggplot2::aes(x = .data[[cont_var]], y = .data$Mean,
                                      colour = .data[[cat_var]])) +
      ggplot2::geom_ribbon(ggplot2::aes(x = .data[[cont_var]],
                                        ymin = .data$Lower, ymax = .data$Upper,
                                        fill = .data[[cat_var]]),
                           alpha = 0.25) +
      ggplot2::labs(x = cont_var, y = "Predicted response",
                    colour = cat_var, fill = cat_var) +
      ggplot2::theme_bw()

    if (length(keep_cats) > 0L) {
      p <- p + ggplot2::facet_wrap(
        stats::as.formula(paste("~", paste(keep_cats, collapse = " + ")))
      )
    }
    plot_obj <- p
  }

  unit_word <- if (standardized) "one-standard-deviation" else "one-unit"
  scale_note <- if (standardized)
    " Continuous predictors are assumed to be standardized (mean 0, SD 1)."
  else ""

  facet_note <- if (length(keep_cats) > 0L)
    sprintf(" Facets are combinations of %s.",
            paste(keep_cats, collapse = " x "))
  else ""

  plot_note <- if (is.null(plot_obj))
    sprintf(
      " Plot suppressed: %d facet panels exceed the limit of 8. Re-run with overridePlot = TRUE to force it.",
      n_facets
    )
  else
    " 'plot' contains the faceted ggplot of predicted responses vs the continuous predictor, coloured by the factor."

  interp <- sprintf(
    paste0(
      "Interaction %s (categorical) x %s (continuous).%s\n",
      "'emtrends' gives the slope of %s at each level of %s on the linear ",
      "predictor scale: a %s increase in %s shifts the linear predictor by ",
      "'Slope' units. Slopes whose CI excludes zero are discernible from zero.",
      "%s%s"
    ),
    cat_var, cont_var, scale_note,
    cont_var, cat_var,
    unit_word, cont_var,
    plot_note, facet_note
  )

  list(type           = "categorical-continuous",
       variables      = vars,
       emtrends       = emt_df,
       plot           = plot_obj,
       interpretation = interp)
}

# ---------------------------------------------------------------------------
# Continuous x continuous
# ---------------------------------------------------------------------------

.untangle_cont_cont <- function(raw, vars, keep_cats, mf, standardized) {
  v1 <- vars[1L]
  v2 <- vars[2L]

  lmh <- function(x) {
    if (standardized) c(-1, 0, 1)
    else mean(x, na.rm = TRUE) + c(-1, 0, 1) * stats::sd(x, na.rm = TRUE)
  }
  vals1 <- lmh(mf[[v1]])
  vals2 <- lmh(mf[[v2]])

  by_part <- if (length(keep_cats))
    paste0(" * ", paste(keep_cats, collapse = " * "))
  else ""

  # Slope of v1 at low/med/high values of v2
  specs1 <- stats::as.formula(paste0("~ ", v1, " | ", v2, by_part))
  emt1   <- emmeans::emtrends(raw, specs = specs1, var = v1,
                              at = stats::setNames(list(vals2), v2))
  emt1_df <- .emtrends_to_df(emt1, v1)

  # Slope of v2 at low/med/high values of v1
  specs2 <- stats::as.formula(paste0("~ ", v2, " | ", v1, by_part))
  emt2   <- emmeans::emtrends(raw, specs = specs2, var = v2,
                              at = stats::setNames(list(vals1), v1))
  emt2_df <- .emtrends_to_df(emt2, v2)

  # Johnson-Neyman in both directions
  jn1 <- .safe_johnson_neyman(raw, pred = v1, modx = v2)
  jn2 <- .safe_johnson_neyman(raw, pred = v2, modx = v1)

  unit_word <- if (standardized) "one-standard-deviation" else "one-unit"
  level_desc <- if (standardized)
    "-1, 0, and +1 (standardized units)"
  else
    "(mean - SD), mean, and (mean + SD)"

  scale_note <- if (standardized)
    " Continuous predictors are assumed to be standardized (mean 0, SD 1); a 'unit increase' is a one-SD increase."
  else
    ""

  interp <- sprintf(
    paste0(
      "Interaction %s x %s (continuous x continuous).%s\n",
      "'emtrends_%s' gives the slope of %s at %s values of %s: Slope is the ",
      "change in the linear predictor for a %s increase in %s.\n",
      "'emtrends_%s' gives the slope of %s at %s values of %s (analogous).\n",
      "'johnson_neyman_%s_by_%s' reports the range of %s values over which the ",
      "slope of %s on the linear predictor is statistically discernible ",
      "(control.fdr = TRUE). 'johnson_neyman_%s_by_%s' is the swapped version."
    ),
    v1, v2, scale_note,
    v1, v1, level_desc, v2, unit_word, v1,
    v2, v2, level_desc, v1,
    v1, v2, v2, v1,
    v2, v1
  )

  out <- list(
    type       = "continuous-continuous",
    variables  = vars,
    interpretation = interp
  )
  out[[paste0("emtrends_", v1)]]              <- emt1_df
  out[[paste0("emtrends_", v2)]]              <- emt2_df
  out[[paste0("johnson_neyman_", v1, "_by_", v2)]] <- jn1
  out[[paste0("johnson_neyman_", v2, "_by_", v1)]] <- jn2
  out
}

.safe_johnson_neyman <- function(model, pred, modx) {
  if (!requireNamespace("interactions", quietly = TRUE)) {
    warning("Package 'interactions' is required for Johnson-Neyman intervals. ",
            "Install via install.packages('interactions'); skipping.",
            call. = FALSE)
    return(NULL)
  }
  tryCatch(
    rlang::inject(interactions::johnson_neyman(
      model,
      pred = !!rlang::sym(pred),
      modx = !!rlang::sym(modx),
      control.fdr = TRUE
    )),
    error = function(e) {
      warning(sprintf(
        "johnson_neyman(pred = %s, modx = %s) failed: %s",
        pred, modx, conditionMessage(e)
      ), call. = FALSE)
      NULL
    }
  )
}

# ---------------------------------------------------------------------------
# print method
# ---------------------------------------------------------------------------

#' @export
print.untangle_interaction <- function(x, ...) {
  cat(x$interpretation, "\n\n", sep = "")
  switch(x$type,
    "categorical-categorical" = {
      cat("$emmeans\n")
      print(x$emmeans, row.names = FALSE)
      if (!is.null(x$plot))
        cat("\nAccess $plot to render the means-and-error-bar plot.\n")
    },
    "categorical-continuous" = {
      cat("$emtrends\n")
      print(x$emtrends, row.names = FALSE)
      if (!is.null(x$plot))
        cat("\nAccess $plot to render the predicted-response plot.\n")
    },
    "continuous-continuous" = {
      v <- x$variables
      cat(sprintf("$emtrends_%s\n", v[1L]))
      print(x[[paste0("emtrends_", v[1L])]], row.names = FALSE)
      cat(sprintf("\n$emtrends_%s\n", v[2L]))
      print(x[[paste0("emtrends_", v[2L])]], row.names = FALSE)
      jn1 <- x[[paste0("johnson_neyman_", v[1L], "_by_", v[2L])]]
      jn2 <- x[[paste0("johnson_neyman_", v[2L], "_by_", v[1L])]]
      if (!is.null(jn1)) {
        cat(sprintf("\n$johnson_neyman_%s_by_%s (text summary)\n", v[1L], v[2L]))
        cat(.jn_text(jn1), "\n")
      }
      if (!is.null(jn2)) {
        cat(sprintf("\n$johnson_neyman_%s_by_%s (text summary)\n", v[2L], v[1L]))
        cat(.jn_text(jn2), "\n")
      }
      cat("\nAccess $johnson_neyman_*$plot to render the J-N plots.\n")
    }
  )
  invisible(x)
}

.jn_text <- function(jn) {
  tryCatch({
    bounds  <- attr(jn, "bounds")
    inside  <- attr(jn, "inside")
    all_sig <- attr(jn, "all_sig")
    pred    <- attr(jn, "pred")
    modx    <- attr(jn, "modx")
    rng     <- attr(jn, "modrange")
    if (isTRUE(all_sig)) {
      sprintf(
        "The slope of %s is significant (p < .05) across the entire observed range of %s [%.2f, %.2f].",
        pred, modx, rng[1L], rng[2L]
      )
    } else if (length(bounds) == 0L || all(is.na(bounds))) {
      sprintf(
        "The Johnson-Neyman interval could not be found for pred = %s, modx = %s.",
        pred, modx
      )
    } else {
      word <- if (isTRUE(inside)) "INSIDE" else "OUTSIDE"
      sprintf(
        "When %s is %s [%.2f, %.2f], the slope of %s is p < .05.\nObserved range of %s: [%.2f, %.2f].",
        modx, word, bounds[1L], bounds[2L], pred, modx, rng[1L], rng[2L]
      )
    }
  }, error = function(e) "(J-N summary unavailable)")
}
