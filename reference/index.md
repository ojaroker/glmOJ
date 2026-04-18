# Package index

## Package

- [`glmOJ-package`](http://oscar.jaroker.com/glmOJ/reference/glmOJ.md)
  [`glmOJ`](http://oscar.jaroker.com/glmOJ/reference/glmOJ.md) : glmOJ:
  Toolkit for Count Regression Modeling

## Data Summarization

Explore count response variables numerically and graphically before
fitting a model.

- [`summarizeCountData()`](http://oscar.jaroker.com/glmOJ/reference/summarizeCountData.md)
  : Summarize count data

## Model Fitting

Individual model fitters. Each returns exponentiated coefficients with
95% Wald confidence intervals, randomized quantile residuals, a Pearson
dispersion ratio, and a two-panel diagnostic plot.

### Poisson

- [`poissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/poissonGLM.md)
  : Fit a Poisson regression model

### Negative Binomial

- [`negbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/negbinGLM.md)
  : Fit a negative binomial regression model

### Zero-Inflated Poisson

- [`zeroinflPoissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflPoissonGLM.md)
  : Fit a zero-inflated Poisson regression model

### Zero-Inflated Negative Binomial

- [`zeroinflNegbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflNegbinGLM.md)
  : Fit a zero-inflated negative binomial regression model

### Tweedie

- [`tweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/tweedieGLM.md)
  : Fit a Tweedie regression model

### Zero-Inflated Tweedie

- [`zeroinflTweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflTweedieGLM.md)
  : Fit a zero-inflated Tweedie regression model

## Model Selection

Fit all four model families and select the best by a user-chosen
criterion (`decide`: `"BIC"` (default), `"AIC"`, `"LogLik"`, or
`"McFadden"`), with plain-language diagnostics to guide interpretation.

- [`countGLM()`](http://oscar.jaroker.com/glmOJ/reference/countGLM.md) :
  Fit and compare count regression models

## Interpretation

Natural-language interpretation of model coefficients and interactions.

- [`interpret_coef()`](http://oscar.jaroker.com/glmOJ/reference/interpret_coef.md)
  : Interpret a model coefficient in natural language
- [`untangle_interaction()`](http://oscar.jaroker.com/glmOJ/reference/untangle_interaction.md)
  : Untangle a two-way interaction in a fitted model
