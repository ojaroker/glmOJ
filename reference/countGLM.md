# Fit and compare count regression models

Fits all four count regression models supported by glmOJ (Poisson,
negative binomial, zero-inflated Poisson, zero-inflated negative
binomial), selects the best by the metric given in `decide`, and
provides a plain-language recommendation informed by dispersion and
zero-inflation diagnostics.

## Usage

``` r
countGLM(formula, data, ziformula = NULL, decide = "BIC", ...)
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

- decide:

  Character string specifying the model-selection criterion. One of
  `"BIC"` (default), `"AIC"`, `"LogLik"` (log-likelihood, higher is
  better), or `"McFadden"` (McFadden pseudo-R², higher is better).
  Matching is case-insensitive.

- ...:

  Additional arguments passed to each individual model fitter.

## Value

An object of class `"countGLM"`, a list with:

- `call`:

  The matched call.

- `fits`:

  A named list of successfully fitted model objects (`poisson`,
  `negbin`, `zeroinfl_poisson`, `zeroinfl_negbin`). Any model that
  failed to converge is omitted. Poisson and negative binomial fits
  include `diagnostics$zi_test` populated from a DHARMa zero-inflation
  test run internally.

- `aic_table`:

  A named numeric vector of AICs, sorted ascending.

- `bic_table`:

  A named numeric vector of BICs, sorted ascending.

- `metric_table`:

  A named numeric vector of the selection metric values (the criterion
  named by `decide`), sorted best-first.

- `decide`:

  The normalised (lower-case) name of the selection criterion actually
  used.

- `best_model`:

  Character name of the model selected by `decide`.

- `recommendation`:

  A plain-language character string explaining the selection, including
  the criterion value, dispersion context, and zero-inflation test
  results.

- `vif`:

  Named numeric vector of Variance Inflation Factors for the main-effect
  predictors in `formula` (interaction and polynomial terms are excluded
  to avoid structural-collinearity false positives). `NULL` when fewer
  than two main-effect predictors are present. A warning is issued for
  any VIF \> 5.

## Details

**Model selection:** The model with the best value of `decide` is
chosen. For `"AIC"` and `"BIC"` the model with the *lowest* value wins;
for `"LogLik"` and `"McFadden"` the model with the *highest* value wins.
AIC and BIC are always computed and displayed regardless of `decide`.
When `decide = "McFadden"`, intercept-only null models are fitted for
each family to compute the pseudo-R².

**Zero-inflation diagnostics:** DHARMa simulation tests are run on both
the Poisson and negative binomial fits. Results appear in
`result$fits$poisson$diagnostics$zi_test` and
`result$fits$negbin$diagnostics$zi_test`, and inform the recommendation
text. Individual model warnings are suppressed; the recommendation
summarises the findings instead.

Individual models support
[`print()`](https://rdrr.io/r/base/print.html),
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
result <- countGLM(y ~ x1, data = df)           # default: BIC
#> Note: zero-inflation component uses the same predictors as the count component (ziformula = NULL). Use `ziformula` to specify a different formula.
#> Warning: Count component: 8 events (y > 0) for 1 predictor(s) (8.0 per predictor). At least 10 events per predictor is recommended.
#> Warning: Count component: 8 events (y > 0) for 1 predictor(s) (8.0 per predictor). At least 10 events per predictor is recommended.
#> Warning: iteration limit reached
#> Warning: iteration limit reached
#> Warning: Count component: 8 events (y > 0) for 1 predictor(s) (8.0 per predictor). At least 10 events per predictor is recommended.
#> Warning: Zero-inflation component: 2 zeros for 1 predictor(s) (2.0 per predictor). At least 10 zeros per ZI predictor is recommended.
#> Warning: Count component: 8 events (y > 0) for 1 predictor(s) (8.0 per predictor). At least 10 events per predictor is recommended.
#> Warning: Zero-inflation component: 2 zeros for 1 predictor(s) (2.0 per predictor). At least 10 zeros per ZI predictor is recommended.
result <- countGLM(y ~ x1, data = df, decide = "AIC")
#> Note: zero-inflation component uses the same predictors as the count component (ziformula = NULL). Use `ziformula` to specify a different formula.
#> Warning: Count component: 8 events (y > 0) for 1 predictor(s) (8.0 per predictor). At least 10 events per predictor is recommended.
#> Warning: Count component: 8 events (y > 0) for 1 predictor(s) (8.0 per predictor). At least 10 events per predictor is recommended.
#> Warning: iteration limit reached
#> Warning: iteration limit reached
#> Warning: Count component: 8 events (y > 0) for 1 predictor(s) (8.0 per predictor). At least 10 events per predictor is recommended.
#> Warning: Zero-inflation component: 2 zeros for 1 predictor(s) (2.0 per predictor). At least 10 zeros per ZI predictor is recommended.
#> Warning: Count component: 8 events (y > 0) for 1 predictor(s) (8.0 per predictor). At least 10 events per predictor is recommended.
#> Warning: Zero-inflation component: 2 zeros for 1 predictor(s) (2.0 per predictor). At least 10 zeros per ZI predictor is recommended.
result <- countGLM(y ~ x1, data = df, decide = "McFadden")
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
#> countGLM(formula = y ~ x1, data = df, decide = "McFadden")
#> 
#> Model comparison (sorted by McFadden R2 (descending)):
#>             model   AIC   BIC McFadden R2
#>  zeroinfl_poisson 41.43 42.64      0.0710
#>   zeroinfl_negbin 43.43 44.94      0.0710
#>           poisson 39.02 39.63      0.0459
#>            negbin 41.02 41.93      0.0407
#> 
#> Selected model: zeroinfl_poisson
#> 
#> Recommendation:
#>   Zero-Inflated Poisson was selected by McFadden R² (McFadden R² =
#>   0.0710). The Poisson dispersion ratio is 1.17, consistent with
#>   equidispersion. No significant zero-inflation detected (Poisson p =
#>   0.890, Negative Binomial p = 0.878).
#> 
summary(result)
#> Summary of selected model (zeroinfl_poisson):
#> 
#> 
#> Call:
#> zeroinflPoissonGLM(formula = formula, data = data, ziformula = ziformula)
#> 
#> Model family: zeroinflPoissonGLM 
#> 
#> Count component (exponentiated coefficients):
#>         term exp.coef lower.95 upper.95 p.value stars
#>  (Intercept)   2.0801   1.2018   3.6002  0.0089    **
#>           x1   1.3932   0.9095   2.1342  0.1275      
#> 
#> Zero-inflation component (exponentiated coefficients):
#>         term exp.coef lower.95 upper.95 p.value stars
#>  (Intercept)   0.1201   0.0071   2.0295  0.1417      
#>           x1   1.7533   0.2286  13.4471  0.5891      
#> 
#> Dispersion ratio: 1.1578
#> AIC: 41.43
```
