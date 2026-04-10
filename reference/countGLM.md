# Fit and compare count regression models

Fits all four count regression models supported by glmOJ (Poisson,
negative binomial, zero-inflated Poisson, zero-inflated negative
binomial), selects the best by AIC, and provides a plain-language
recommendation informed by dispersion and zero-inflation diagnostics.

## Usage

``` r
countGLM(formula, data, ziformula = NULL, ...)
```

## Arguments

- formula:

  A model formula for the count component (e.g. `y ~ x1 + x2`). The
  response must be a non-negative integer count variable.

- data:

  A data frame containing the variables in `formula` (and `ziformula` if
  provided).

- ziformula:

  A one-sided formula for the zero-inflation component passed to
  [`zeroinflPoissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflPoissonGLM.md)
  and
  [`zeroinflNegbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflNegbinGLM.md).
  When `NULL` (default), the same right-hand side as `formula` is used.

- ...:

  Additional arguments passed to each individual model fitter.

## Value

An object of class `"countGLM"`, a list with:

- `call`:

  The matched call.

- `fits`:

  A named list of successfully fitted model objects (`poisson`,
  `negbin`, `zeroinfl_poisson`, `zeroinfl_negbin`). Any model that
  failed to converge is omitted.

- `aic_table`:

  A named numeric vector of AICs, sorted ascending.

- `bic_table`:

  A named numeric vector of BICs, sorted ascending.

- `best_model`:

  Character name of the selected model. When AIC and BIC agree this is
  the jointly best model; when they disagree the simpler of the two
  candidates is chosen.

- `recommendation`:

  A plain-language character string explaining the selection, including
  dispersion, zero-inflation context, and a note when AIC and BIC point
  to different models.

## Details

**Model selection:** AIC is the primary criterion. Lower AIC indicates a
better balance of fit and parsimony.

**Heuristic diagnostics** (computed from the Poisson fit and used to
annotate the recommendation, not to override AIC):

- *Overdispersion*: Pearson dispersion ratio \> 1.5 from the Poisson fit
  suggests the negative binomial family may be more appropriate.

- *Zero-inflation*: If the observed number of zeros exceeds 1.3x the
  number of zeros expected under a Poisson model, excess zeros are
  flagged.

Individual models can be accessed via `result$fits$negbin`, etc., and
support [`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html), and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## See also

[`poissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/poissonGLM.md),
[`negbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/negbinGLM.md),
[`zeroinflPoissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflPoissonGLM.md),
[`zeroinflNegbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflNegbinGLM.md)

## Examples

``` r
df <- data.frame(
  y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
  x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
)
result <- countGLM(y ~ x1, data = df)
#> Note: zero-inflation component uses the same predictors as the count component (ziformula = NULL). Use `ziformula` to specify a different formula.
#> Warning: Count component: 8 events (y > 0) for 1 predictor(s) (8.0 per predictor). At least 10 events per predictor is recommended.
#> Warning: Count component: 8 events (y > 0) for 1 predictor(s) (8.0 per predictor). At least 10 events per predictor is recommended.
#> Warning: iteration limit reached
#> Warning: iteration limit reached
#> Warning: Count component: 8 events (y > 0) for 1 predictor(s) (8.0 per predictor). At least 10 events per predictor is recommended.
#> Warning: Zero-inflation component: 2 zeros for 1 predictor(s) (2.0 per predictor). At least 10 zeros per ZI predictor is recommended.
#> Warning: Count component: 8 events (y > 0) for 1 predictor(s) (8.0 per predictor). At least 10 events per predictor is recommended.
#> Warning: Zero-inflation component: 2 zeros for 1 predictor(s) (2.0 per predictor). At least 10 zeros per ZI predictor is recommended.
print(result)
#> 
#> Call:
#> countGLM(formula = y ~ x1, data = df)
#> 
#> Model comparison (sorted by AIC):
#>             model   AIC   BIC
#>           poisson 39.02 39.63
#>            negbin 41.02 41.93
#>  zeroinfl_poisson 41.43 42.64
#>   zeroinfl_negbin 43.43 44.94
#> 
#> Selected model: poisson
#> 
#> Recommendation:
#>   Poisson was selected — both AIC (39.02) and BIC (39.63) agree. The
#>   Poisson dispersion ratio is 1.17, consistent with equidispersion.
#>   There are 1.4x more zeros than expected under Poisson (observed: 2,
#>   expected: 1.4), suggesting zero-inflation.
#> 
summary(result)
#> Summary of selected model (poisson):
#> 
#> 
#> Call:
#> poissonGLM(formula = formula, data = data)
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
```
