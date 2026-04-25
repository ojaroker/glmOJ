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

### Quasi-Poisson

- [`quasiPoissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/quasiPoissonGLM.md)
  : Fit a quasi-Poisson regression model

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

## Generic Methods

S3 generics for all model classes.
[`predict()`](https://rdrr.io/r/stats/predict.html) and
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html) delegate to the
underlying backend. [`coef()`](https://rdrr.io/r/stats/coef.html)
returns a named numeric vector of exponentiated coefficients.
[`coef_table()`](http://oscar.jaroker.com/glmOJ/reference/coef_table.md)
returns the full table with 95% Wald CIs, p-values, and significance
stars. For `countGLM` objects all methods automatically use the selected
best model.

- [`predict(`*`<countGLMfit>`*`)`](http://oscar.jaroker.com/glmOJ/reference/glmOJ-predict.md)
  [`predict(`*`<zeroinflGLMfit>`*`)`](http://oscar.jaroker.com/glmOJ/reference/glmOJ-predict.md)
  [`predict(`*`<countGLM>`*`)`](http://oscar.jaroker.com/glmOJ/reference/glmOJ-predict.md)
  : Predict from a count regression model
- [`fitted(`*`<countGLMfit>`*`)`](http://oscar.jaroker.com/glmOJ/reference/glmOJ-fitted.md)
  [`fitted(`*`<countGLM>`*`)`](http://oscar.jaroker.com/glmOJ/reference/glmOJ-fitted.md)
  : Extract fitted values from a count regression model
- [`coef(`*`<countGLMfit>`*`)`](http://oscar.jaroker.com/glmOJ/reference/glmOJ-coef.md)
  [`coef(`*`<zeroinflGLMfit>`*`)`](http://oscar.jaroker.com/glmOJ/reference/glmOJ-coef.md)
  [`coef(`*`<countGLM>`*`)`](http://oscar.jaroker.com/glmOJ/reference/glmOJ-coef.md)
  : Extract model coefficients
- [`coef_table()`](http://oscar.jaroker.com/glmOJ/reference/coef_table.md)
  : Coefficient table with confidence intervals and p-values

## Interpretation

Natural-language interpretation of model coefficients and interactions.

- [`interpret_coef()`](http://oscar.jaroker.com/glmOJ/reference/interpret_coef.md)
  : Interpret a model coefficient in natural language
- [`untangle_interaction()`](http://oscar.jaroker.com/glmOJ/reference/untangle_interaction.md)
  : Untangle a two-way interaction in a fitted model

## Datasets

Real and simulated datasets bundled with the package for use in examples
and the vignette.

- [`Greenberg26.dat`](http://oscar.jaroker.com/glmOJ/reference/Greenberg26.dat.md)
  : County-level environmental enforcement penalties (Greenberg 2026)
- [`Dahir25.dat`](http://oscar.jaroker.com/glmOJ/reference/Dahir25.dat.md)
  : Census-tract street-camera and crime data (Dahir 2025)
- [`ZITweedie.dat`](http://oscar.jaroker.com/glmOJ/reference/ZITweedie.dat.md)
  : Simulated zero-inflated Tweedie count data
