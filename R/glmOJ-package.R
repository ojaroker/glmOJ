#' glmOJ: Toolkit for Count Regression Modeling
#'
#' Provides a workflow for fitting, diagnosing, and interpreting count and
#' semi-continuous regression models. Supports Poisson, quasi-Poisson,
#' negative binomial, Tweedie, zero-inflated Poisson, zero-inflated negative
#' binomial, and zero-inflated Tweedie models. Includes data summarization
#' tools, randomized quantile residual diagnostics, and a general-purpose
#' wrapper ([countGLM()]) that fits all model families and selects the best
#' by AIC, BIC, log-likelihood, or McFadden pseudo-R\ifelse{html}{\out{&sup2;}}{\eqn{^2}}.
#'
#' @docType package
#' @name glmOJ
#' @importFrom stats aggregate coef density fitted model.frame model.matrix
#'   model.offset model.response na.omit ppoints predict quantile residuals
#'   setNames terms var
#' @importFrom grDevices nclass.Sturges
"_PACKAGE"

# NSE column / variable names used inside dplyr / ggplot pipelines.
# Declared here to silence the "no visible binding for global variable" notes
# raised by R CMD check for histoqq() and plot_count_data().
utils::globalVariables(c(
  "Freq", "Var1", "bin.cut", "bin.idx", "bin.mid", "count", "dens",
  "density", "deviation", "dx", "dy", "expected.count", "final.x",
  "final.y", "h", "val", "x", "x.base", "x.c", "y", "y.base", "y.c"
))
