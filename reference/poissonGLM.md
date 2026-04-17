# Fit a Poisson regression model

Fits a Poisson GLM and returns model coefficients on the response scale
(exponentiated), randomized quantile residuals (RQR), a Pearson
dispersion ratio, and diagnostic plots.

## Usage

``` r
poissonGLM(formula, data, assessZeroInflation = TRUE, ...)
```

## Arguments

- formula:

  A model formula (e.g. `y ~ x1 + x2`). The response must be a
  non-negative integer count variable.

- data:

  A data frame containing the variables in `formula`.

- assessZeroInflation:

  Logical; when `TRUE` (default), runs a DHARMa simulation-based
  zero-inflation test after fitting. Issues a warning if significant
  zero-inflation is detected and adds `zi_test` to the returned
  diagnostics. Set to `FALSE` when calling from
  [`countGLM()`](http://oscar.jaroker.com/glmOJ/reference/countGLM.md),
  which performs its own zero-inflation assessment.

- ...:

  Additional arguments passed to
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html).

## Value

An object of class `c("poissonGLM", "countGLMfit")`, a list with:

- `call`:

  The matched call.

- `model`:

  The underlying [stats::glm](https://rdrr.io/r/stats/glm.html) fit
  object.

- `summary`:

  The result of [`summary()`](https://rdrr.io/r/base/summary.html) on
  the fitted model.

- `coefficients`:

  A data frame with columns `term`, `exp.coef`, `lower.95`, `upper.95`
  (all on the response/exponentiated scale).

- `diagnostics`:

  A list with:

  `rqr`

  :   Numeric vector of randomized quantile residuals.

  `dispersion_ratio`

  :   Pearson chi-squared / df.residual.

  `plot`

  :   Patchwork ggplot: fitted vs RQR and histo-QQ.

  `r2_plot`

  :   Squared Pearson residuals vs fitted values.

  `zi_test`

  :   When `assessZeroInflation = TRUE`, a list with `detected`
      (logical), `p_value` (numeric), and `plot` (ggplot histogram of
      DHARMa simulated zero proportions vs observed). `NULL` when
      `assessZeroInflation = FALSE`.

- `aic`:

  AIC of the fitted model.

- `bic`:

  BIC of the fitted model.

## Details

**Coefficient interpretation:** Poisson regression models the log of the
expected count. Exponentiating a coefficient gives the multiplicative
change in the expected count for a one-unit increase in the predictor.

**Condition checking:** Inspect `diagnostics$dispersion_ratio` (near 1
is good) and `diagnostics$zi_test$p_value` for zero-inflation.

## See also

[`negbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/negbinGLM.md),
[`tweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/tweedieGLM.md),
[`zeroinflPoissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflPoissonGLM.md),
[`countGLM()`](http://oscar.jaroker.com/glmOJ/reference/countGLM.md),
[`stats::glm()`](https://rdrr.io/r/stats/glm.html)

## Examples

``` r
df <- data.frame(
  y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
  x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
)
fit <- poissonGLM(y ~ x1, data = df)
#> Warning: Count component: 8 events (y > 0) for 1 predictor(s) (8.0 per predictor). At least 10 events per predictor is recommended.
print(fit)
#> 
#> Call:
#> poissonGLM(formula = y ~ x1, data = df)
#> 
#> Model family: poissonGLM 
#> 
#> Coefficients (on response scale):
#>         term exp.coef lower.95 upper.95 p.value stars
#>  (Intercept)   1.7989   1.0683   3.0290  0.0272     *
#>           x1   1.3396   0.8572   2.0936  0.1993      
#> 
#> Dispersion ratio: 1.1658
#> AIC: 39.02
plot(fit)

```
