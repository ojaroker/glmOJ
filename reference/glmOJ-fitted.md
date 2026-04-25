# Extract fitted values from a count regression model

Returns the vector of fitted values from any model class produced by
[`poissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/poissonGLM.md),
[`negbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/negbinGLM.md),
[`quasiPoissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/quasiPoissonGLM.md),
[`tweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/tweedieGLM.md),
[`zeroinflPoissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflPoissonGLM.md),
[`zeroinflNegbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflNegbinGLM.md),
[`zeroinflTweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflTweedieGLM.md),
or [`countGLM()`](http://oscar.jaroker.com/glmOJ/reference/countGLM.md).
For [`countGLM()`](http://oscar.jaroker.com/glmOJ/reference/countGLM.md)
objects, fitted values are taken from the selected best model.

## Usage

``` r
# S3 method for class 'countGLMfit'
fitted(object, ...)

# S3 method for class 'countGLM'
fitted(object, ...)
```

## Arguments

- object:

  A fitted model returned by any of the package fitters, or a `countGLM`
  comparison object.

- ...:

  Additional arguments passed to the underlying backend's
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) method.

## Value

A numeric vector of fitted means on the response scale.

## Examples

``` r
df <- data.frame(
  y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
  x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
)
fit <- poissonGLM(y ~ x1, data = df)
#> Warning: Count component: 8 events (y > 0) for 1 predictor(s) (8.0 per predictor). At least 10 events per predictor is recommended.
head(fitted(fit))
#>        1        2        3        4        5        6 
#> 2.554880 1.600319 2.272896 1.304135 3.228141 1.963771 
```
