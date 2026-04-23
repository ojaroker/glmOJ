# Fit a Tweedie regression model

Fits a Tweedie GLM (via
[`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html))
and returns model coefficients on the response scale (exponentiated),
randomized quantile residuals (RQR), estimated dispersion (`phi`) and
power (`p`) parameters, and diagnostic plots. The Tweedie family
generalises Poisson and Gamma distributions and is well-suited to
non-negative count data with complex variance structures.

## Usage

``` r
tweedieGLM(
  formula,
  data,
  assessZeroInflation = TRUE,
  maxit = NULL,
  dispersion_threshold = 1.2,
  ...
)
```

## Arguments

- formula:

  A model formula (e.g. `y ~ x1 + x2`). The response must be
  non-negative.

- data:

  A data frame containing the variables in `formula`.

- assessZeroInflation:

  Logical; when `TRUE` (default), runs a DHARMa simulation-based
  zero-inflation test after fitting. Issues a warning if significant
  zero-inflation is detected and adds `zi_test` to the returned
  diagnostics. Set to `FALSE` when calling from
  [`countGLM()`](http://oscar.jaroker.com/glmOJ/reference/countGLM.md),
  which performs its own zero-inflation assessment.

- maxit:

  Optional integer; maximum optimizer iterations passed through as
  `control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = maxit, eval.max = maxit))`.
  Ignored when the user supplies their own `control` via `...`.

- dispersion_threshold:

  Numeric; dispersion ratios above this value are flagged as
  overdispersed in the diagnostic plot. Default 1.2.

- ...:

  Additional arguments passed to
  [`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html).

## Value

An object of class `c("tweedieGLM", "countGLMfit")`, a list with:

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

  The estimated Tweedie power parameter (p), restricted to (1, 2).
  Values near 1 resemble Poisson; values near 2 resemble Gamma. `NA` if
  the parameter cannot be extracted from the fit.

- `coefficients`:

  A data frame with columns `term`, `exp.coef`, `lower.95`, `upper.95`,
  `p.value`, and `stars` (all on the response/exponentiated scale).

- `diagnostics`:

  A list with:

  `rqr`

  :   Numeric vector of randomized quantile residuals (DHARMa
      simulation-based, converted to normal scale).

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

**Three parameters:** The Tweedie family is characterised by three
estimated quantities: the regression coefficients (mean structure, via a
log link), the dispersion `phi` (Var(Y) = phi \* mu^p), and the power
`p` (1 \< p \< 2). When `p` is close to 1, the variance structure
resembles Poisson; when close to 2, it resembles Gamma.

**Coefficient interpretation:** Exponentiating a coefficient gives the
multiplicative change in the expected response for a one-unit increase
in the predictor, holding all other predictors constant.

**When to use:** Tweedie regression is appropriate for non-negative
count data with complex variance structures not well captured by Poisson
or negative binomial. If zero-inflation is also present, consider
[`zeroinflTweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflTweedieGLM.md).

## See also

[`zeroinflTweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflTweedieGLM.md),
[`negbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/negbinGLM.md),
[`poissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/poissonGLM.md),
[`countGLM()`](http://oscar.jaroker.com/glmOJ/reference/countGLM.md),
[`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html)

## Examples

``` r
df <- data.frame(
  y  = c(0, 0, 1.5, 3.2, 5.8, 0, 0.9, 4.1, 0, 2.7),
  x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
)
fit <- suppressWarnings(tweedieGLM(y ~ x1, data = df))
print(fit)
#> 
#> Call:
#> tweedieGLM(formula = y ~ x1, data = df)
#> 
#> Model family: tweedieGLM
#> 
#> Coefficients (on response scale):
#>         term exp.coef lower.95 upper.95 p.value stars
#>  (Intercept)   1.2700   0.5923   2.7229  0.5391      
#>           x1   1.7403   0.9555   3.1698  0.0701     .
#> 
#> Dispersion (phi): 1.4059
#> Power (p): 1.0420
#> Dispersion ratio: 2.0335
#> AIC: 40.64
plot(fit)

```
