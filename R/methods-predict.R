# fitted() and predict() methods for countGLMfit, zeroinflGLMfit, and countGLM
# objects.  Each method delegates to the underlying backend fit stored at
# `object$model`, so the full vocabulary of `type` values supported by the
# backend (`stats::predict.glm`, `glmmTMB:::predict.glmmTMB`, or
# `pscl:::predict.zeroinfl`) is available.

#' Extract fitted values from a count regression model
#'
#' Returns the vector of fitted values from any model class produced by
#' [poissonGLM()], [negbinGLM()], [quasiPoissonGLM()], [tweedieGLM()],
#' [zeroinflPoissonGLM()], [zeroinflNegbinGLM()], [zeroinflTweedieGLM()], or
#' [countGLM()]. For [countGLM()] objects, fitted values are taken from the
#' selected best model.
#'
#' @param object A fitted model returned by any of the package fitters, or a
#'   `countGLM` comparison object.
#' @param ... Additional arguments passed to the underlying backend's
#'   `fitted()` method.
#'
#' @return A numeric vector of fitted means on the response scale.
#' @name glmOJ-fitted
#' @examples
#' df <- data.frame(
#'   y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
#'   x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
#' )
#' fit <- poissonGLM(y ~ x1, data = df)
#' head(fitted(fit))
NULL

#' @rdname glmOJ-fitted
#' @export
fitted.countGLMfit <- function(object, ...) {
  stats::fitted(object$model, ...)
}

#' @rdname glmOJ-fitted
#' @export
fitted.countGLM <- function(object, ...) {
  fitted(object$fits[[object$best_model]], ...)
}

#' Predict from a count regression model
#'
#' Generic prediction method for the model classes produced by [poissonGLM()],
#' [negbinGLM()], [quasiPoissonGLM()], [tweedieGLM()], [zeroinflPoissonGLM()],
#' [zeroinflNegbinGLM()], [zeroinflTweedieGLM()], and [countGLM()]. The call is
#' forwarded to the backend's own `predict()` method (`stats::predict.glm`,
#' `glmmTMB:::predict.glmmTMB`, or `pscl:::predict.zeroinfl`) so the full set
#' of supported `type` values is available. Defaults to `type = "response"`.
#'
#' For a [countGLM()] object the prediction comes from whichever model was
#' selected by `decide`.
#'
#' @param object A fitted model returned by any of the package fitters, or a
#'   `countGLM` comparison object.
#' @param newdata Optional data frame to predict on. When omitted, predictions
#'   on the training data are returned.
#' @param type Backend-dependent prediction type. Common values:
#'   `"response"` (default; mean on the response scale), `"link"` (linear
#'   predictor), and for zero-inflated models `"count"`, `"zero"`, or
#'   `"prob"`. See `?stats::predict.glm`, `?glmmTMB::predict.glmmTMB`, and
#'   `?pscl::predict.zeroinfl` for the full list per backend.
#' @param ... Additional arguments forwarded to the backend predict method.
#'
#' @return A numeric vector (or matrix, for `type = "prob"`) of predictions.
#' @name glmOJ-predict
#' @examples
#' df <- data.frame(
#'   y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
#'   x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
#' )
#' fit <- poissonGLM(y ~ x1, data = df)
#' predict(fit, newdata = data.frame(x1 = c(0, 1, 2)))
NULL

#' @rdname glmOJ-predict
#' @export
predict.countGLMfit <- function(object, newdata = NULL, type = "response", ...) {
  args <- list(object = object$model, type = type, ...)
  if (!is.null(newdata)) args$newdata <- newdata
  do.call(stats::predict, args)
}

#' @rdname glmOJ-predict
#' @export
predict.zeroinflGLMfit <- function(object, newdata = NULL, type = "response", ...) {
  args <- list(object = object$model, type = type, ...)
  if (!is.null(newdata)) args$newdata <- newdata
  do.call(stats::predict, args)
}

#' @rdname glmOJ-predict
#' @export
predict.countGLM <- function(object, newdata = NULL, type = "response", ...) {
  predict(object$fits[[object$best_model]], newdata = newdata, type = type, ...)
}
