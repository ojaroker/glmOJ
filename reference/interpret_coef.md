# Interpret a model coefficient in natural language

Generates a plain-language interpretation of an exponentiated regression
coefficient: the percent change in the expected count (or rate, if an
offset is present), with a 95% Wald confidence interval. For
zero-inflated models, the `component` argument selects the count or
zero-inflation part. A note is appended whenever the coefficient is not
discernibly different from zero (p \> 0.05).

## Usage

``` r
interpret_coef(model, predictor, component = "count")
```

## Arguments

- model:

  A fitted model returned by
  [`poissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/poissonGLM.md),
  [`negbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/negbinGLM.md),
  [`tweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/tweedieGLM.md),
  [`zeroinflPoissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflPoissonGLM.md),
  [`zeroinflNegbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflNegbinGLM.md),
  [`zeroinflTweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflTweedieGLM.md),
  or
  [`countGLM()`](http://oscar.jaroker.com/glmOJ/reference/countGLM.md).
  For `countGLM`, the best-fitting model is used automatically.

- predictor:

  Character; the exact term name as it appears in the coefficient table
  (e.g. `"pctnonwhite10"`, `"EPAregion2"`).

- component:

  For zero-inflated models: `"count"` (default) for the count component,
  or `"zero"` for the zero-inflation component. Ignored for Poisson and
  negative binomial models.

## Value

The interpretation string, printed to the console and returned
invisibly.
