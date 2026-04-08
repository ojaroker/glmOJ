# Fit a zero-inflated Poisson regression model

Fits a zero-inflated Poisson model (via
[`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html)) with
separate count and zero-inflation components. Returns coefficients on
the response scale, randomized quantile residuals, a dispersion ratio,
and a diagnostic plot.

## Usage

``` r
zeroinflPoissonGLM(formula, data, ziformula = NULL, ...)
```

## Arguments

- formula:

  A model formula for the **count** component (e.g. `y ~ x1 + x2`). The
  response must be a non-negative integer count.

- data:

  A data frame containing the variables in `formula` (and `ziformula` if
  provided).

- ziformula:

  A one-sided formula for the **zero-inflation** component (e.g.
  `~ x1`). When `NULL` (default), the same right-hand side as `formula`
  is used for both components. Use `~ 1` for an intercept-only
  zero-inflation model.

- ...:

  Additional arguments passed to
  [`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html).

## Value

An object of class
`c("zeroinflPoissonGLM", "zeroinflGLMfit", "countGLMfit")`, a list with:

- `call`:

  The matched call.

- `model`:

  The underlying
  [pscl::zeroinfl](https://rdrr.io/pkg/pscl/man/zeroinfl.html) fit
  object.

- `coefficients`:

  A list with two data frames, each with columns `term`, `exp.coef`,
  `lower.95`, `upper.95`:

  `count`

  :   Exponentiated coefficients for the count component.

  `zero`

  :   Exponentiated coefficients for the zero-inflation component.

- `diagnostics`:

  A list with `rqr`, `dispersion_ratio`, and `plot` (see
  [`poissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/poissonGLM.md)
  for details).

- `aic`:

  AIC of the fitted model.

## Details

**Coefficient interpretation:**

- *Count component*: exponentiating a coefficient gives the
  multiplicative change in the expected count among non-structural-zero
  observations, for a one-unit increase in the predictor, adjusting for
  simultaneous linear changes in other predictors. For example, 1.5
  means a 50% higher expected count.

- *Zero component*: exponentiating a coefficient gives the
  multiplicative change in the odds of being a structural zero (vs.
  entering the count process) for a one-unit increase in the predictor.
  A value greater than 1 means higher odds of being a structural zero.

**When to use:** Zero-inflated Poisson is appropriate when there are
more zeros than a standard Poisson model would predict (excess zeros),
but the non-zero counts themselves follow a Poisson distribution. If the
non-zero counts are also overdispersed, prefer
[`zeroinflNegbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflNegbinGLM.md).

## See also

[`zeroinflNegbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflNegbinGLM.md),
[`poissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/poissonGLM.md),
[`countGLM()`](http://oscar.jaroker.com/glmOJ/reference/countGLM.md),
[`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html)

## Examples

``` r
df <- data.frame(
  y  = c(0L, 0L, 0L, 1L, 2L, 0L, 3L, 0L, 1L, 0L),
  x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
)
fit <- zeroinflPoissonGLM(y ~ x1, data = df)
print(fit)
#> 
#> Call:
#> zeroinflPoissonGLM(formula = y ~ x1, data = df)
#> 
#> Model family: zeroinflPoissonGLM 
#> 
#> Count component (exponentiated coefficients):
#>         term exp.coef lower.95 upper.95
#>  (Intercept)   1.3408   0.4923   3.6521
#>           x1   0.9433   0.4383   2.0303
#> 
#> Zero-inflation component (exponentiated coefficients):
#>         term exp.coef lower.95 upper.95
#>  (Intercept)   0.6737   0.0821   5.5318
#>           x1   2.1030   0.4077  10.8484
#> 
#> Dispersion ratio: 1.6711
#> AIC: 29.60
plot(fit)


# Intercept-only zero component:
fit2 <- zeroinflPoissonGLM(y ~ x1, data = df, ziformula = ~ 1)
```
