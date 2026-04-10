# Fit a Poisson regression model

Fits a Poisson GLM and returns model coefficients on the response scale
(exponentiated), randomized quantile residuals (RQR), a Pearson
dispersion ratio, and a two-panel diagnostic plot.

## Usage

``` r
poissonGLM(formula, data, ...)
```

## Arguments

- formula:

  A model formula (e.g. `y ~ x1 + x2`). The response must be a
  non-negative integer count variable.

- data:

  A data frame containing the variables in `formula`.

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

  :   Pearson chi-squared / df.residual. Values substantially above 1
      (rule of thumb: \> 1.5) suggest overdispersion; consider
      [`negbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/negbinGLM.md).

  `plot`

  :   A patchwork ggplot: fitted values vs RQR (left) and histo-QQ of
      RQR (right). The dispersion ratio is shown in red with an
      overdispersion warning if it exceeds 1.2.

  `r2_plot`

  :   Squared Pearson residuals vs fitted values, with a red dotted
      reference line at 1 and a smooth. Useful for diagnosing
      mean-variance misspecification.

- `aic`:

  AIC of the fitted model.

- `bic`:

  BIC of the fitted model.

## Details

**Coefficient interpretation:** Poisson regression models the log of the
expected count. Exponentiating a coefficient gives the multiplicative
change in the expected count for a one-unit increase in the predictor,
adjusting for simultaneous linear changes in other predictors. For
example, 1.5 means a 50% higher expected count.

**Condition checking:** Inspect `diagnostics$dispersion_ratio`. A value
near 1 is consistent with the Poisson assumption (mean = variance). The
RQR diagnostic plot should show points scattered randomly around zero
with approximately normal QQ behaviour.

## See also

[`negbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/negbinGLM.md),
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
#>         term exp.coef lower.95 upper.95
#>  (Intercept)   1.7989   1.0683   3.0290
#>           x1   1.3396   0.8572   2.0936
#> 
#> Dispersion ratio: 1.1658
#> AIC: 39.02
plot(fit)

```
