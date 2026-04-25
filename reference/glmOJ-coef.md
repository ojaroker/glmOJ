# Extract model coefficients

Returns coefficients on the response scale (i.e. exponentiated linear-
predictor coefficients) as a named numeric vector, matching the
convention of [`stats::coef()`](https://rdrr.io/r/stats/coef.html). For
zero-inflated models the count and zero-inflation components are
concatenated into a single vector with `count_` and `zero_` name
prefixes, mirroring
[`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html).

## Usage

``` r
# S3 method for class 'countGLMfit'
coef(object, ...)

# S3 method for class 'zeroinflGLMfit'
coef(object, component = c("both", "count", "zero"), ...)

# S3 method for class 'countGLM'
coef(object, ...)
```

## Arguments

- object:

  A fitted model object of class `countGLMfit` or `zeroinflGLMfit`.

- ...:

  Unused.

- component:

  For zero-inflated models, `"both"` (default) returns the concatenated
  vector; `"count"` returns only the count component; `"zero"` returns
  only the zero-inflation component.

## Value

A named numeric vector of exponentiated coefficients.

## Details

Use
[`coef_table()`](http://oscar.jaroker.com/glmOJ/reference/coef_table.md)
to get the full coefficient table including 95\\ CIs, p-values, and
significance stars.

## Examples

``` r
df <- data.frame(
  y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
  x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
)
fit <- poissonGLM(y ~ x1, data = df)
#> Warning: Count component: 8 events (y > 0) for 1 predictor(s) (8.0 per predictor). At least 10 events per predictor is recommended.
coef(fit)
#> (Intercept)          x1 
#>    1.798861    1.339607 
coef_table(fit)
#>          term exp.coef lower.95 upper.95    p.value stars
#> 1 (Intercept) 1.798861 1.068297 3.029027 0.02721241     *
#> 2          x1 1.339607 0.857165 2.093584 0.19934617      
```
