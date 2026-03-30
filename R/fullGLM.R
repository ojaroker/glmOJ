#' Title for fullGLM
#'
#' Short description of what fullGLM does.
#'
#' @param formula A model formula (e.g. \code{y ~ x1 + x2}).
#' @param data A data frame containing the variables in \code{formula}.
#' @param family A description of the error distribution and link function to be used in the model (default: \code{gaussian()}).
#' @param weights Optional numeric vector of observation weights.
#' @param subset Optional expression indicating subset of rows to use.
#' @param na.action Function which indicates what should happen when the data contain \code{NA}s.
#' @param control A list of control parameters (document the elements you use; e.g. from \code{glm.control}).
#' @param offset Optional offset.
#' @param ... Additional arguments passed to lower-level methods.
#'
#' @return An object of class \code{fullGLM} (a list containing at least: \code{coefficients}, \code{fitted.values}, \code{residuals}, \code{vcov}, \code{converged}). Describe components precisely.
#'
#' @details Longer description of algorithm, assumptions, and any important notes (e.g. handling of singularities, convergence criteria, warnings produced).
#'
#' @examples
#' # basic usage
#' # fm <- fullGLM(y ~ x1 + x2, data = mydf, family = binomial())
#' # summary(fm)
#'
#' @seealso \link[stats]{glm}, \link[stats]{model.frame}
#' @keywords regression models
#' @export
fullGLM <- function(
  formula,
  data,
  family = gaussian(),
  weights = NULL,
  subset = NULL,
  na.action = getOption("na.action"),
  control = glm.control(),
  offset = NULL,
  ...
) {
  stopifnot(inherits(formula, "formula"))
  # Implementation here...
  structure(
    list(
      coefficients = NULL,
      fitted.values = NULL,
      residuals = NULL,
      vcov = NULL,
      converged = logical(1)
    ),
    class = "fullGLM"
  )
}
