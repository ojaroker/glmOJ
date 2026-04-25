# Coefficient table with confidence intervals and p-values

Returns the full coefficient table (term, exponentiated coefficient,
lower/upper 95\\ For zero-inflated models, returns a named list with
`count` and `zero` data frames.

## Usage

``` r
coef_table(object, ...)
```

## Arguments

- object:

  A fitted model of class `countGLMfit`, `zeroinflGLMfit`, or
  `countGLM`.

- ...:

  Unused.

## Value

A data frame, or a named list of two data frames for zero-inflated
models.

## Examples

``` r
df <- data.frame(
  y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
  x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
)
fit <- poissonGLM(y ~ x1, data = df)
#> Warning: Count component: 8 events (y > 0) for 1 predictor(s) (8.0 per predictor). At least 10 events per predictor is recommended.
coef_table(fit)
#>          term exp.coef lower.95 upper.95    p.value stars
#> 1 (Intercept) 1.798861 1.068297 3.029027 0.02721241     *
#> 2          x1 1.339607 0.857165 2.093584 0.19934617      
```
