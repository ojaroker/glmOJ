# Predict from a count regression model

Generic prediction method for the model classes produced by
[`poissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/poissonGLM.md),
[`negbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/negbinGLM.md),
[`quasiPoissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/quasiPoissonGLM.md),
[`tweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/tweedieGLM.md),
[`zeroinflPoissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflPoissonGLM.md),
[`zeroinflNegbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflNegbinGLM.md),
[`zeroinflTweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflTweedieGLM.md),
and
[`countGLM()`](http://oscar.jaroker.com/glmOJ/reference/countGLM.md).
The call is forwarded to the backend's own
[`predict()`](https://rdrr.io/r/stats/predict.html) method
([`stats::predict.glm`](https://rdrr.io/r/stats/predict.glm.html),
`glmmTMB:::predict.glmmTMB`, or `pscl:::predict.zeroinfl`) so the full
set of supported `type` values is available. Defaults to
`type = "response"`.

## Usage

``` r
# S3 method for class 'countGLMfit'
predict(object, newdata = NULL, type = "response", ...)

# S3 method for class 'zeroinflGLMfit'
predict(object, newdata = NULL, type = "response", ...)

# S3 method for class 'countGLM'
predict(object, newdata = NULL, type = "response", ...)
```

## Arguments

- object:

  A fitted model returned by any of the package fitters, or a `countGLM`
  comparison object.

- newdata:

  Optional data frame to predict on. When omitted, predictions on the
  training data are returned.

- type:

  Backend-dependent prediction type. Common values: `"response"`
  (default; mean on the response scale), `"link"` (linear predictor),
  and for zero-inflated models `"count"`, `"zero"`, or `"prob"`. See
  [`?stats::predict.glm`](https://rdrr.io/r/stats/predict.glm.html),
  [`?glmmTMB::predict.glmmTMB`](https://rdrr.io/pkg/glmmTMB/man/predict.glmmTMB.html),
  and
  [`?pscl::predict.zeroinfl`](https://rdrr.io/pkg/pscl/man/predict.zeroinfl.html)
  for the full list per backend.

- ...:

  Additional arguments forwarded to the backend predict method.

## Value

A numeric vector (or matrix, for `type = "prob"`) of predictions.

## Details

For a
[`countGLM()`](http://oscar.jaroker.com/glmOJ/reference/countGLM.md)
object the prediction comes from whichever model was selected by
`decide`.

## Examples

``` r
df <- data.frame(
  y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
  x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
)
fit <- poissonGLM(y ~ x1, data = df)
#> Warning: Count component: 8 events (y > 0) for 1 predictor(s) (8.0 per predictor). At least 10 events per predictor is recommended.
predict(fit, newdata = data.frame(x1 = c(0, 1, 2)))
#>        1        2        3 
#> 1.798861 2.409767 3.228141 
```
