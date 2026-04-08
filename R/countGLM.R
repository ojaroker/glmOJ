#' Title for countGLM
#'
#' Short description of what countGLM does.
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
#' @return An object of class \code{countGLM} (a list containing at least: \code{coefficients}, \code{fitted.values}, \code{residuals}, \code{vcov}, \code{converged}). Describe components precisely.
#'
#' @details Longer description of algorithm, assumptions, and any important notes (e.g. handling of singularities, convergence criteria, warnings produced).
#'
#' @examples
#' # fm = countGLM(y ~ x1 + x2, data = dat, family = poisson())
#'
#'
#'
#' @seealso \link[stats]{glm}, \link[stats]{model.frame}
#' @keywords regression models
#' @export
countGLM <- function(
  formula,
  data,
  family = poisson(),
  weights = NULL,
  subset = NULL,
  na.action = getOption("na.action"),
  control = glm.control(),
  offset = NULL,
  ...
) {
  stopifnot(inherits(formula, "formula"))
  








  structure(
    list(
      coefficients = NULL,
      fitted.values = NULL,
      residuals = NULL,
      vcov = NULL,
      converged = logical(1)
    ),
    class = "countGLM"
  )
}
