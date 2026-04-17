# Fit a zero-inflated Tweedie regression model

Fits a zero-inflated Tweedie model (via
[`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html))
with separate count and zero-inflation components. Returns coefficients
on the response scale, randomized quantile residuals, dispersion (`phi`)
and power (`p`) parameters, and diagnostic plots.

## Usage

``` r
zeroinflTweedieGLM(formula, data, ziformula = NULL, ...)
```

## Arguments

- formula:

  A model formula for the **count** component (e.g. `y ~ x1 + x2`). The
  response must be non-negative.

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
  [`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html).

## Value

An object of class
`c("zeroinflTweedieGLM", "zeroinflGLMfit", "countGLMfit")`, a list with:

- `call`:

  The matched call.

- `model`:

  The underlying
  [glmmTMB::glmmTMB](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html) fit
  object.

- `summary`:

  The result of [`summary()`](https://rdrr.io/r/base/summary.html) on
  the fitted model.

- `phi`:

  The estimated Tweedie dispersion parameter (phi).

- `p`:

  The estimated Tweedie power parameter (p), in (1, 2). `NA` if the
  parameter cannot be extracted from the fit.

- `coefficients`:

  A list with two data frames, each with columns `term`, `exp.coef`,
  `lower.95`, `upper.95`, `p.value`, and `stars`:

  `count`

  :   Exponentiated coefficients for the count component.

  `zero`

  :   Exponentiated coefficients for the zero-inflation component (odds
      ratios for structural-zero membership).

- `diagnostics`:

  A list with `rqr`, `dispersion_ratio`, `plot`, and `r2_plot` (see
  [`tweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/tweedieGLM.md)
  for details). No `zi_test` is included since this model already
  accounts for zero-inflation.

- `aic`:

  AIC of the fitted model.

- `bic`:

  BIC of the fitted model.

## Details

**Coefficient interpretation:**

- *Count component*: exponentiating a coefficient gives the
  multiplicative change in the expected Tweedie mean among observations
  that are not structural zeros, for a one-unit increase in the
  predictor.

- *Zero component*: exponentiating a coefficient gives the
  multiplicative change in the odds of being a structural zero (vs.
  entering the count process) for a one-unit increase in the predictor.
  A value greater than 1 means higher odds of a structural zero.

**When to use:** Zero-inflated Tweedie is the most flexible model in
glmOJ. It handles excess zeros, non-negative semi-continuous responses,
and complex variance structures simultaneously. Prefer this over
[`zeroinflPoissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflPoissonGLM.md)
or
[`zeroinflNegbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflNegbinGLM.md)
when the response is semi-continuous rather than strictly
integer-valued.

## See also

[`tweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/tweedieGLM.md),
[`zeroinflNegbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflNegbinGLM.md),
[`countGLM()`](http://oscar.jaroker.com/glmOJ/reference/countGLM.md),
[`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html)

## Examples

``` r
df <- data.frame(
  y  = c(0, 0, 0, 1.5, 3.2, 0, 0.9, 0, 0, 2.7,
         0, 0, 4.1, 0, 1.2, 0, 0, 5.8, 0, 0),
  x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7,
         -0.5, 0.9, 1.3, -0.3, 0.6, -1.2, 0.1, 2.1, -0.7, 0.4)
)
fit <- suppressWarnings(zeroinflTweedieGLM(y ~ x1, data = df))
print(fit)
#> 
#> Call:
#> zeroinflTweedieGLM(formula = y ~ x1, data = df)
#> 
#> Model family: zeroinflTweedieGLM
#> 
#> Count component (exponentiated coefficients):
#>         term exp.coef lower.95 upper.95 p.value stars
#>  (Intercept)   0.7391   0.1812   3.0144  0.6733      
#>           x1   2.3596   1.1133   5.0011  0.0251     *
#> 
#> Zero-inflation component (exponentiated coefficients):
#>         term exp.coef lower.95 upper.95 p.value stars
#>  (Intercept)   0.5444   0.0309   9.5888  0.6778      
#>           x1   0.9352   0.2112   4.1411  0.9297      
#> 
#> Dispersion (phi): 1.3833
#> Power (p): 1.0360
#> Dispersion ratio: 1.2846
#> AIC: 53.06
plot(fit)


# Intercept-only zero component:
fit2 <- suppressWarnings(
  zeroinflTweedieGLM(y ~ x1, data = df, ziformula = ~ 1)
)
```
