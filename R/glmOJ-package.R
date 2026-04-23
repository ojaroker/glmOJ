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
"_PACKAGE"
