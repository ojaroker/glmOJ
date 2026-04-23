# S3 methods for countGLMfit, zeroinflGLMfit, and countGLM objects
# -----------------------------------------------------------------------

# ---- Helpers -----------------------------------------------------------

.fmt_coef_table <- function(df, digits = 4) {
  num_cols <- vapply(df, is.numeric, logical(1))
  df[num_cols] <- lapply(df[num_cols], round, digits = digits)
  df
}

# ---- print methods -----------------------------------------------------

#' @export
print.tweedieGLM <- function(x, digits = 4, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nModel family: tweedieGLM\n")
  cat("\nCoefficients (on response scale):\n")
  print(.fmt_coef_table(x$coefficients, digits), row.names = FALSE)
  if (!is.na(x$phi)) cat(sprintf("\nDispersion (phi): %.4f\n", x$phi))
  if (!is.na(x$p))   cat(sprintf("Power (p): %.4f\n", x$p))
  cat(sprintf("Dispersion ratio: %.4f\n", x$diagnostics$dispersion_ratio))
  cat(sprintf("AIC: %.2f\n", x$aic))
  invisible(x)
}

#' @export
print.zeroinflTweedieGLM <- function(x, digits = 4, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nModel family: zeroinflTweedieGLM\n")
  cat("\nCount component (exponentiated coefficients):\n")
  print(.fmt_coef_table(x$coefficients$count, digits), row.names = FALSE)
  cat("\nZero-inflation component (exponentiated coefficients):\n")
  print(.fmt_coef_table(x$coefficients$zero, digits), row.names = FALSE)
  if (!is.na(x$phi)) cat(sprintf("\nDispersion (phi): %.4f\n", x$phi))
  if (!is.na(x$p))   cat(sprintf("Power (p): %.4f\n", x$p))
  cat(sprintf("Dispersion ratio: %.4f\n", x$diagnostics$dispersion_ratio))
  cat(sprintf("AIC: %.2f\n", x$aic))
  invisible(x)
}

#' @export
print.quasiPoissonGLM <- function(x, digits = 4, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nModel family: quasiPoissonGLM\n")
  cat("\nCoefficients (on response scale):\n")
  print(.fmt_coef_table(x$coefficients, digits), row.names = FALSE)
  if (!is.na(x$phi)) cat(sprintf("\nDispersion (phi): %.4f\n", x$phi))
  cat(sprintf("Dispersion ratio: %.4f\n", x$diagnostics$dispersion_ratio))
  cat("AIC: NA (quasi-likelihood)\n")
  invisible(x)
}

#' @export
print.countGLMfit <- function(x, digits = 4, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nModel family:", class(x)[1L], "\n")
  cat("\nCoefficients (on response scale):\n")
  print(.fmt_coef_table(x$coefficients, digits), row.names = FALSE)
  cat(sprintf("\nDispersion ratio: %.4f\n", x$diagnostics$dispersion_ratio))
  cat(sprintf("AIC: %.2f\n", x$aic))
  invisible(x)
}

#' @export
print.zeroinflGLMfit <- function(x, digits = 4, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nModel family:", class(x)[1L], "\n")
  cat("\nCount component (exponentiated coefficients):\n")
  print(.fmt_coef_table(x$coefficients$count, digits), row.names = FALSE)
  cat("\nZero-inflation component (exponentiated coefficients):\n")
  print(.fmt_coef_table(x$coefficients$zero, digits), row.names = FALSE)
  cat(sprintf("\nDispersion ratio: %.4f\n", x$diagnostics$dispersion_ratio))
  cat(sprintf("AIC: %.2f\n", x$aic))
  invisible(x)
}

# ---- summary methods ---------------------------------------------------

#' @export
summary.countGLMfit <- function(object, ...) {
  out <- list(
    call             = object$call,
    family           = class(object)[1L],
    coefficients     = object$coefficients,
    dispersion_ratio = object$diagnostics$dispersion_ratio,
    aic              = object$aic,
    rqr              = object$diagnostics$rqr,
    plot             = object$diagnostics$plot
  )
  class(out) <- "summary.countGLMfit"
  out
}

#' @export
summary.zeroinflGLMfit <- function(object, ...) {
  out <- list(
    call             = object$call,
    family           = class(object)[1L],
    coefficients     = object$coefficients,
    dispersion_ratio = object$diagnostics$dispersion_ratio,
    aic              = object$aic,
    rqr              = object$diagnostics$rqr,
    plot             = object$diagnostics$plot
  )
  class(out) <- c("summary.zeroinflGLMfit", "summary.countGLMfit")
  out
}

#' @export
print.summary.countGLMfit <- function(x, digits = 4, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nModel family:", x$family, "\n")
  cat("\nCoefficients (on response scale):\n")
  print(.fmt_coef_table(x$coefficients, digits), row.names = FALSE)
  cat(sprintf("\nDispersion ratio: %.4f", x$dispersion_ratio))
  disp <- x$dispersion_ratio
  if (!is.na(disp)) {
    if (disp > 1.5) cat("  [suggests overdispersion]")
    else if (disp < 0.5) cat("  [suggests underdispersion]")
  }
  cat(sprintf("\nAIC: %.2f\n", x$aic))
  invisible(x)
}

#' @export
summary.tweedieGLM <- function(object, ...) {
  out <- list(
    call             = object$call,
    family           = class(object)[1L],
    coefficients     = object$coefficients,
    phi              = object$phi,
    p                = object$p,
    dispersion_ratio = object$diagnostics$dispersion_ratio,
    aic              = object$aic,
    rqr              = object$diagnostics$rqr,
    plot             = object$diagnostics$plot
  )
  class(out) <- c("summary.tweedieGLM", "summary.countGLMfit")
  out
}

#' @export
summary.quasiPoissonGLM <- function(object, ...) {
  out <- list(
    call             = object$call,
    family           = class(object)[1L],
    coefficients     = object$coefficients,
    phi              = object$phi,
    dispersion_ratio = object$diagnostics$dispersion_ratio,
    aic              = object$aic,
    rqr              = object$diagnostics$rqr,
    plot             = object$diagnostics$plot
  )
  class(out) <- c("summary.quasiPoissonGLM", "summary.countGLMfit")
  out
}

#' @export
summary.zeroinflTweedieGLM <- function(object, ...) {
  out <- list(
    call             = object$call,
    family           = class(object)[1L],
    coefficients     = object$coefficients,
    phi              = object$phi,
    p                = object$p,
    dispersion_ratio = object$diagnostics$dispersion_ratio,
    aic              = object$aic,
    rqr              = object$diagnostics$rqr,
    plot             = object$diagnostics$plot
  )
  class(out) <- c("summary.zeroinflTweedieGLM", "summary.zeroinflGLMfit",
                  "summary.countGLMfit")
  out
}

#' @export
print.summary.tweedieGLM <- function(x, digits = 4, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nModel family:", x$family, "\n")
  cat("\nCoefficients (on response scale):\n")
  print(.fmt_coef_table(x$coefficients, digits), row.names = FALSE)
  if (!is.null(x$phi) && !is.na(x$phi)) cat(sprintf("\nDispersion (phi): %.4f\n", x$phi))
  if (!is.null(x$p)   && !is.na(x$p))   cat(sprintf("Power (p): %.4f\n", x$p))
  cat(sprintf("Dispersion ratio: %.4f", x$dispersion_ratio))
  disp <- x$dispersion_ratio
  if (!is.na(disp)) {
    if (disp > 1.5) cat("  [suggests overdispersion]")
    else if (disp < 0.5) cat("  [suggests underdispersion]")
  }
  cat(sprintf("\nAIC: %.2f\n", x$aic))
  invisible(x)
}

#' @export
print.summary.quasiPoissonGLM <- function(x, digits = 4, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nModel family:", x$family, "\n")
  cat("\nCoefficients (on response scale):\n")
  print(.fmt_coef_table(x$coefficients, digits), row.names = FALSE)
  if (!is.null(x$phi) && !is.na(x$phi)) cat(sprintf("\nDispersion (phi): %.4f\n", x$phi))
  cat(sprintf("Dispersion ratio: %.4f\n", x$dispersion_ratio))
  cat("AIC: NA (quasi-likelihood)\n")
  invisible(x)
}

#' @export
print.summary.zeroinflTweedieGLM <- function(x, digits = 4, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nModel family:", x$family, "\n")
  cat("\nCount component (exponentiated coefficients):\n")
  print(.fmt_coef_table(x$coefficients$count, digits), row.names = FALSE)
  cat("\nZero-inflation component (exponentiated coefficients):\n")
  print(.fmt_coef_table(x$coefficients$zero, digits), row.names = FALSE)
  if (!is.null(x$phi) && !is.na(x$phi)) cat(sprintf("\nDispersion (phi): %.4f\n", x$phi))
  if (!is.null(x$p)   && !is.na(x$p))   cat(sprintf("Power (p): %.4f\n", x$p))
  cat(sprintf("Dispersion ratio: %.4f", x$dispersion_ratio))
  disp <- x$dispersion_ratio
  if (!is.na(disp)) {
    if (disp > 1.5) cat("  [suggests overdispersion]")
    else if (disp < 0.5) cat("  [suggests underdispersion]")
  }
  cat(sprintf("\nAIC: %.2f\n", x$aic))
  invisible(x)
}

#' @export
print.summary.zeroinflGLMfit <- function(x, digits = 4, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nModel family:", x$family, "\n")
  cat("\nCount component (exponentiated coefficients):\n")
  print(.fmt_coef_table(x$coefficients$count, digits), row.names = FALSE)
  cat("\nZero-inflation component (exponentiated coefficients):\n")
  print(.fmt_coef_table(x$coefficients$zero, digits), row.names = FALSE)
  cat(sprintf("\nDispersion ratio: %.4f", x$dispersion_ratio))
  disp <- x$dispersion_ratio
  if (!is.na(disp)) {
    if (disp > 1.5) cat("  [suggests overdispersion]")
    else if (disp < 0.5) cat("  [suggests underdispersion]")
  }
  cat(sprintf("\nAIC: %.2f\n", x$aic))
  invisible(x)
}

# ---- plot methods ------------------------------------------------------

#' @export
plot.countGLMfit <- function(x, ...) {
  print(x$diagnostics$plot)
  invisible(x)
}

#' @export
plot.countGLM <- function(x, ...) {
  plot(x$fits[[x$best_model]], ...)
  invisible(x)
}

# ---- coef methods ------------------------------------------------------

#' @export
coef.countGLMfit <- function(object, ...) {
  object$coefficients
}

#' @export
coef.zeroinflGLMfit <- function(object, component = c("count", "zero"), ...) {
  component <- match.arg(component)
  object$coefficients[[component]]
}

# ---- print.countGLM ----------------------------------------------------

#' @export
print.countGLM <- function(x, digits = 4, ...) {
  cat("\nCall:\n")
  print(x$call)

  # Rows ordered best-first by the chosen metric (metric_table is pre-sorted)
  all_models <- names(x$metric_table)
  # Quasi-Poisson (if fitted) is excluded from metric_table; append it at the
  # end so it appears in the comparison table with NA criterion values.
  extra_models <- setdiff(names(x$fits), all_models)
  all_models <- c(all_models, extra_models)

  decide <- x$decide
  metric_label <- switch(decide,
    aic      = "AIC",
    bic      = "BIC",
    loglik   = "LogLik",
    mcfadden = "McFadden R2"
  )
  sort_dir <- if (decide %in% c("loglik", "mcfadden")) "descending" else "ascending"
  sort_note <- sprintf("sorted by %s (%s)", metric_label, sort_dir)

  ic_df <- data.frame(
    model = all_models,
    AIC   = round(unname(vapply(all_models,
                                 function(m) {
                                   if (m %in% names(x$aic_table)) x$aic_table[[m]]
                                   else NA_real_
                                 }, numeric(1L))), 2),
    BIC   = round(unname(vapply(all_models,
                                 function(m) {
                                   if (m %in% names(x$bic_table)) x$bic_table[[m]]
                                   else NA_real_
                                 }, numeric(1L))), 2),
    row.names = NULL
  )

  # Add the selection metric column when it is not already AIC or BIC
  if (decide == "loglik") {
    ic_df$LogLik <- round(unname(vapply(all_models,
                                         function(m) {
                                           if (m %in% names(x$metric_table)) x$metric_table[[m]]
                                           else NA_real_
                                         }, numeric(1L))), 2)
  } else if (decide == "mcfadden") {
    ic_df[["McFadden R2"]] <- round(unname(vapply(all_models,
                                                    function(m) {
                                                      if (m %in% names(x$metric_table)) x$metric_table[[m]]
                                                      else NA_real_
                                                    }, numeric(1L))), 4)
  }

  cat(sprintf("\nModel comparison (%s):\n", sort_note))
  print(ic_df, row.names = FALSE)

  cat(sprintf("\nSelected model: %s\n", x$best_model))
  cat("\nRecommendation:\n")
  cat(strwrap(x$recommendation, width = 72, prefix = "  "), sep = "\n")
  cat("\n")
  invisible(x)
}

#' @export
summary.countGLM <- function(object, ...) {
  cat("Summary of selected model (", object$best_model, "):\n\n", sep = "")
  invisible(summary(object$fits[[object$best_model]], ...))
}
