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

# ---- plot method -------------------------------------------------------

#' @export
plot.countGLMfit <- function(x, ...) {
  print(x$diagnostics$plot)
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
  cat("\nAIC comparison:\n")
  aic_df <- data.frame(
    model = names(x$aic_table),
    AIC   = round(x$aic_table, 2),
    row.names = NULL
  )
  print(aic_df, row.names = FALSE)
  cat(sprintf("\nSelected model: %s\n", x$best_model))
  cat("\nRecommendation:\n")
  cat(strwrap(x$recommendation, width = 72, prefix = "  "), sep = "\n")
  cat("\n")
  invisible(x)
}

#' @export
summary.countGLM <- function(object, ...) {
  cat("Summary of selected model (", object$best_model, "):\n\n", sep = "")
  summary(object$fits[[object$best_model]], ...)
}
