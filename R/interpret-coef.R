#' Interpret a model coefficient in natural language
#'
#' Generates a plain-language interpretation of an exponentiated regression
#' coefficient: the percent change in the expected count (or rate, if an offset
#' is present), with a 95% Wald confidence interval. For zero-inflated models,
#' the `component` argument selects the count or zero-inflation part.
#' A note is appended whenever the coefficient is not discernibly different
#' from zero (p > 0.05).
#'
#' @param model A fitted model returned by [poissonGLM()], [negbinGLM()],
#'   [tweedieGLM()], [zeroinflPoissonGLM()], [zeroinflNegbinGLM()],
#'   [zeroinflTweedieGLM()], or [countGLM()].
#'   For `countGLM`, the best-fitting model is used automatically.
#' @param predictor Character; the exact term name as it appears in the
#'   coefficient table (e.g. `"pctnonwhite10"`, `"EPAregion2"`). Optional
#'   when the model has exactly one non-intercept term, in which case that
#'   term is used.
#' @param component For zero-inflated models: `"count"` (default) for the
#'   count component, or `"zero"` for the zero-inflation component. Ignored
#'   for Poisson and negative binomial models.
#' @return The interpretation string, printed to the console and returned
#'   invisibly.
#' @export
interpret_coef <- function(model, predictor, component = "count") {
  UseMethod("interpret_coef")
}

#' @export
interpret_coef.poissonGLM <- function(model, predictor, component = "count") {
  if (missing(predictor)) predictor <- .resolve_predictor(model$coefficients)
  .interp_standard(model, predictor)
}

#' @export
interpret_coef.negbinGLM <- function(model, predictor, component = "count") {
  if (missing(predictor)) predictor <- .resolve_predictor(model$coefficients)
  .interp_standard(model, predictor)
}

#' @export
interpret_coef.quasiPoissonGLM <- function(model, predictor,
                                            component = "count") {
  if (missing(predictor)) predictor <- .resolve_predictor(model$coefficients)
  .interp_standard(model, predictor)
}

#' @export
interpret_coef.tweedieGLM <- function(model, predictor, component = "count") {
  if (missing(predictor)) predictor <- .resolve_predictor(model$coefficients)
  .interp_glmmtmb_standard(model, predictor)
}

#' @export
interpret_coef.zeroinflTweedieGLM <- function(model, predictor,
                                               component = "count") {
  component <- match.arg(component, c("count", "zero"))
  if (missing(predictor)) {
    predictor <- .resolve_predictor(model$coefficients[[component]],
                                     component = component)
  }
  .interp_glmmtmb_zeroinfl(model, predictor, component)
}

#' @export
interpret_coef.zeroinflGLMfit <- function(model, predictor,
                                           component = "count") {
  component <- match.arg(component, c("count", "zero"))
  if (missing(predictor)) {
    predictor <- .resolve_predictor(model$coefficients[[component]],
                                     component = component)
  }
  .interp_zeroinfl(model, predictor, component)
}

#' @export
interpret_coef.countGLM <- function(model, predictor, component = "count") {
  best <- model$fits[[model$best_model]]
  message("Using best model: ", model$best_model)
  if (missing(predictor)) {
    interpret_coef(best, component = component)
  } else {
    interpret_coef(best, predictor = predictor, component = component)
  }
}

# Auto-detect the predictor when there is exactly one non-intercept term.
# Otherwise, error with a helpful message listing the available terms.
.resolve_predictor <- function(coef_df, component = NULL) {
  candidates <- coef_df$term[coef_df$term != "(Intercept)"]
  comp_label <- if (is.null(component)) "" else sprintf(" in the %s component", component)
  if (length(candidates) == 0L) {
    stop(sprintf("No non-intercept terms available%s.", comp_label),
         call. = FALSE)
  }
  if (length(candidates) > 1L) {
    stop(sprintf(
      "`predictor` is required when the model has more than one non-intercept term%s.\n  Available terms: %s",
      comp_label, paste(candidates, collapse = ", ")
    ), call. = FALSE)
  }
  candidates
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Detect an offset in a fitted model by inspecting the formula for offset()
# terms. `fit$offset` is populated by stats::glm and MASS::glm.nb but is NULL
# for glmmTMB objects, so we also check the formula.
.has_offset <- function(fit) {
  # 1. Numeric offset vector stored on the fit (glm, glm.nb, zeroinfl)
  off <- tryCatch(fit$offset, error = function(e) NULL)
  if (is.list(off)) off <- unlist(off, use.names = FALSE)
  if (!is.null(off) && length(off) > 0L) {
    has_num <- tryCatch(any(off != 0, na.rm = TRUE), error = function(e) FALSE)
    if (isTRUE(has_num)) return(TRUE)
  }

  # 2. offset() term inside the formula (covers glmmTMB where $offset is NULL)
  fm <- tryCatch(stats::formula(fit), error = function(e) NULL)
  if (!is.null(fm)) {
    if (any(grepl("offset\\(", deparse(fm)))) return(TRUE)
  }

  # 3. zeroinfl stores count/zero terms as a named list
  tms <- tryCatch(fit$terms, error = function(e) NULL)
  if (is.list(tms) && !inherits(tms, "terms")) {
    for (tm in tms) {
      if (!is.null(tm) && length(attr(tm, "offset")) > 0L) return(TRUE)
    }
  } else if (!is.null(tms) && length(attr(tms, "offset")) > 0L) {
    return(TRUE)
  }

  FALSE
}

# Extract the response name. `fit$terms` is a named list (count/zero/full) for
# pscl::zeroinfl objects, so [[2L]] would return a terms object instead of the
# response symbol. Prefer formula()-based extraction.
.response_name <- function(fit) {
  fm <- tryCatch(stats::formula(fit), error = function(e) NULL)
  if (!is.null(fm) && length(fm) >= 2L) {
    nm <- tryCatch(deparse(fm[[2L]]), error = function(e) NULL)
    if (!is.null(nm) && length(nm) > 0L) return(paste(nm, collapse = ""))
  }
  tryCatch(
    deparse(fit$formula[[2L]]),
    error = function(e) "the response"
  )
}

.pval_standard <- function(model, predictor) {
  tryCatch(
    model$summary$coefficients[predictor, "Pr(>|z|)"],
    error = function(e) NA_real_
  )
}

.pval_zeroinfl <- function(model, predictor, component) {
  tryCatch(
    model$summary$coefficients[[component]][predictor, "Pr(>|z|)"],
    error = function(e) NA_real_
  )
}

.format_msg <- function(predictor, exp_b, lo, hi, outcome_phrase, pval) {
  pct       <- abs(exp_b - 1) * 100
  direction <- if (exp_b >= 1) "increase" else "decrease"

  msg <- sprintf(
    paste0(
      "Holding all other predictors constant, a one-unit increase in %s is ",
      "associated with a %.1f%% %s in %s ",
      "(exp(\u03b2) = %.3f, 95%% CI: [%.3f, %.3f])."
    ),
    predictor, pct, direction, outcome_phrase, exp_b, lo, hi
  )

  if (!is.na(pval) && pval > 0.05) {
    msg <- paste0(
      msg,
      sprintf(
        "\nNote: this coefficient is not discernibly different from zero (p = %.3f).",
        pval
      )
    )
  }
  msg
}

.is_interaction_term <- function(term) {
  grepl(":", term, fixed = TRUE)
}

.interaction_note <- function() {
  paste0(
    "Note: this is an interaction term. The above interpretation is the ",
    "multiplicative effect conditional on the other interacting predictor(s) ",
    "being at 0 (or at their reference level). See untangle_interaction() ",
    "for a marginal-effects-based interpretation across the interacting ",
    "predictor's range."
  )
}

.interp_standard <- function(model, predictor) {
  coef_df <- model$coefficients
  row <- coef_df[coef_df$term == predictor, ]
  if (nrow(row) == 0L) {
    stop(sprintf(
      "Predictor '%s' not found. Available terms:\n  %s",
      predictor, paste(coef_df$term, collapse = ", ")
    ))
  }

  pval    <- .pval_standard(model, predictor)
  resp    <- .response_name(model$model)
  has_off <- .has_offset(model$model)
  is_int  <- .is_interaction_term(predictor)

  outcome_phrase <- if (has_off) {
    sprintf("the expected rate of %s per unit of exposure", resp)
  } else {
    sprintf("the expected count of %s", resp)
  }

  msg <- .format_msg(predictor, row$exp.coef, row$lower.95, row$upper.95,
                     outcome_phrase, pval)
  if (is_int) msg <- paste0(msg, "\n", .interaction_note())
  cat(msg, "\n")
  invisible(msg)
}

# glmmTMB stores summary$coefficients as a list ($cond, $zi, …), not a plain
# matrix, so we pull p-values from the already-built coefficient table instead.

.interp_glmmtmb_standard <- function(model, predictor) {
  coef_df <- model$coefficients
  row <- coef_df[coef_df$term == predictor, ]
  if (nrow(row) == 0L) {
    stop(sprintf(
      "Predictor '%s' not found. Available terms:\n  %s",
      predictor, paste(coef_df$term, collapse = ", ")
    ))
  }

  pval    <- row$p.value
  resp    <- .response_name(model$model)
  has_off <- .has_offset(model$model)
  is_int  <- .is_interaction_term(predictor)

  outcome_phrase <- if (has_off) {
    sprintf("the expected rate of %s per unit of exposure", resp)
  } else {
    sprintf("the expected value of %s", resp)
  }

  msg <- .format_msg(predictor, row$exp.coef, row$lower.95, row$upper.95,
                     outcome_phrase, pval)
  if (is_int) msg <- paste0(msg, "\n", .interaction_note())
  cat(msg, "\n")
  invisible(msg)
}

.interp_glmmtmb_zeroinfl <- function(model, predictor, component) {
  coef_df <- model$coefficients[[component]]
  row <- coef_df[coef_df$term == predictor, ]
  if (nrow(row) == 0L) {
    stop(sprintf(
      "Predictor '%s' not found in the %s component. Available terms:\n  %s",
      predictor, component, paste(coef_df$term, collapse = ", ")
    ))
  }

  pval    <- row$p.value
  resp    <- .response_name(model$model)
  has_off <- .has_offset(model$model)
  is_int  <- .is_interaction_term(predictor)

  if (component == "count") {
    outcome_phrase <- if (has_off) {
      sprintf("the expected rate of %s per unit of exposure", resp)
    } else {
      sprintf("the expected value of %s (among non-structural zeros)", resp)
    }
  } else {
    outcome_phrase <- "the odds of being a structural zero"
  }

  msg <- .format_msg(predictor, row$exp.coef, row$lower.95, row$upper.95,
                     outcome_phrase, pval)
  if (is_int) msg <- paste0(msg, "\n", .interaction_note())
  cat(msg, "\n")
  invisible(msg)
}

.interp_zeroinfl <- function(model, predictor, component) {
  coef_df <- model$coefficients[[component]]
  row <- coef_df[coef_df$term == predictor, ]
  if (nrow(row) == 0L) {
    stop(sprintf(
      "Predictor '%s' not found in the %s component. Available terms:\n  %s",
      predictor, component, paste(coef_df$term, collapse = ", ")
    ))
  }

  pval    <- .pval_zeroinfl(model, predictor, component)
  resp    <- .response_name(model$model)
  has_off <- .has_offset(model$model)
  is_int  <- .is_interaction_term(predictor)

  if (component == "count") {
    outcome_phrase <- if (has_off) {
      sprintf("the expected rate of %s per unit of exposure", resp)
    } else {
      sprintf("the expected count of %s (among non-structural zeros)", resp)
    }
  } else {
    outcome_phrase <- "the odds of being a structural zero"
  }

  msg <- .format_msg(predictor, row$exp.coef, row$lower.95, row$upper.95,
                     outcome_phrase, pval)
  if (is_int) msg <- paste0(msg, "\n", .interaction_note())
  cat(msg, "\n")
  invisible(msg)
}
