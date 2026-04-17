# Fit and compare count regression models

Fits three base count regression models (Poisson, negative binomial, and
Tweedie), runs a DHARMa zero-inflation test on each, fits the
corresponding zero-inflated counterpart for any model where
zero-inflation is detected, then selects the best overall model by the
metric given in `decide`.

## Usage

``` r
countGLM(formula, data, ziformula = NULL, decide = "BIC", ...)
```

## Arguments

- formula:

  A model formula for the count component (e.g. `y ~ x1 + x2`). The
  response must be non-negative.

- data:

  A data frame containing the variables in `formula` (and `ziformula` if
  provided).

- ziformula:

  A one-sided formula for the zero-inflation component passed to
  [`zeroinflPoissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflPoissonGLM.md),
  [`zeroinflNegbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflNegbinGLM.md),
  and
  [`zeroinflTweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflTweedieGLM.md)
  when they are needed. When `NULL` (default), the same right-hand side
  as `formula` is used.

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

  A named list of successfully fitted model objects. Base models
  (`poisson`, `negbin`, `tweedie`) are always attempted.
  `zeroinfl_poisson` and `zeroinfl_negbin` are fitted only when the
  DHARMa zero-inflation test flags their base model (p \< 0.05).
  `zeroinfl_tweedie` is **always** fitted alongside `tweedie` because
  glmmTMB's flexible dispersion makes the DHARMa test unreliable for
  that family. Any model that failed to converge is omitted. Base model
  fits include `diagnostics$zi_test` populated from the DHARMa test.

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
  predictors in `formula` (interaction and polynomial terms are
  excluded). `NULL` when fewer than two main-effect predictors are
  present. A warning is issued for any VIF \> 5.

## Details

**Workflow:** `countGLM()` fits Poisson, negative binomial, and Tweedie
base models. It then runs a DHARMa simulation test for zero-inflation on
each successful base model. For Poisson and negative binomial, the
zero-inflated counterpart is fitted only when zero-inflation is detected
(p \< 0.05). For Tweedie, the zero-inflated counterpart is **always**
fitted alongside the base model: glmmTMB can absorb excess zeros by
inflating the dispersion parameter `phi`, which makes the DHARMa ZI test
unreliable for this family. All surviving models are compared by
`decide`.

**Model selection:** The model with the best value of `decide` is
chosen. For `"AIC"` and `"BIC"` the model with the *lowest* value wins;
for `"LogLik"` and `"McFadden"` the model with the *highest* value wins.
AIC and BIC are always computed and displayed regardless of `decide`.
When `decide = "McFadden"`, intercept-only null models are fitted for
each family to compute the pseudo-R².

Individual models support
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html), and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## See also

[`poissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/poissonGLM.md),
[`negbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/negbinGLM.md),
[`tweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/tweedieGLM.md),
[`zeroinflPoissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflPoissonGLM.md),
[`zeroinflNegbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflNegbinGLM.md),
[`zeroinflTweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflTweedieGLM.md)

## Examples

``` r
df <- data.frame(
  y  = c(0L, 1L, 2L, 3L, 5L, 0L, 2L, 4L, 1L, 3L),
  x1 = c(1.2, -0.4, 0.8, -1.1, 2.0, 0.3, -0.9, 1.5, -0.2, 0.7)
)
result <- suppressWarnings(countGLM(y ~ x1, data = df))      # default: BIC
result <- suppressWarnings(countGLM(y ~ x1, data = df, decide = "AIC"))
result <- suppressWarnings(countGLM(y ~ x1, data = df, decide = "McFadden"))
print(result)
#> 
#> Call:
#> countGLM(formula = y ~ x1, data = df, decide = "McFadden")
#> 
#> Model comparison (sorted by McFadden R2 (descending)):
#>    model   AIC   BIC McFadden R2
#>  poisson 39.02 39.63      0.0459
#>   negbin 41.02 41.93      0.0407
#> 
#> Selected model: poisson
#> 
#> Recommendation:
#>   Poisson was selected by McFadden R² (McFadden R² = 0.0459). The
#>   Poisson dispersion ratio is 1.17, consistent with equidispersion. No
#>   significant zero-inflation detected for Poisson or Negative Binomial.
#> 
summary(result)
#> Summary of selected model (poisson):
#> 
#> 
#> Call:
#> poissonGLM(formula = formula, data = data, assessZeroInflation = FALSE)
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
```
