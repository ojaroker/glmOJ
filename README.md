# glmOJ

**glmOJ** is an R package that provides a structured workflow for count
and semi-continuous regression modeling. It wraps `glm`,
[`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html),
[`pscl::zeroinfl`](https://rdrr.io/pkg/pscl/man/zeroinfl.html), and
[`glmmTMB::glmmTMB`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html) to
fit, diagnose, and interpret count regression models, with all output
reported on the response scale rather than the link scale.

## What the package does

Count data — non-negative integers like the number of events, species,
or visits — require specialized regression models. glmOJ guides the
analyst through four steps:

1.  **Summarize** the response variable and its relationship to
    predictors before modeling.
2.  **Fit** one or more count regression models from the supported
    families.
3.  **Assess** model conditions using randomized quantile residuals
    (RQR) and the Pearson dispersion ratio.
4.  **Interpret** all output on the response scale (exponentiated
    coefficients with confidence intervals).

## Supported model families

| Family | Function | Use when |
|----|----|----|
| Poisson | [`poissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/poissonGLM.html) | Counts with mean ≈ variance |
| Quasi-Poisson | [`quasiPoissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/quasiPoissonGLM.html) | Constant overdispersion: $\text{Var}(Y) = \phi \cdot \mu$ with $\phi > 1$, $r^2$ roughly flat across fitted values |
| Negative binomial | [`negbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/negbinGLM.html) | Counts with variance \> mean (overdispersion) |
| Tweedie | [`tweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/tweedieGLM.html) | Non-negative semi-continuous data; power parameter estimated freely |
| Zero-inflated Poisson | [`zeroinflPoissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflPoissonGLM.html) | Excess zeros + equidispersed non-zero counts |
| Zero-inflated negative binomial | [`zeroinflNegbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflNegbinGLM.html) | Excess zeros + overdispersed non-zero counts |
| Zero-inflated Tweedie | [`zeroinflTweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflTweedieGLM.html) | Excess zeros + semi-continuous non-zero values |

The general-purpose
[`countGLM()`](http://oscar.jaroker.com/glmOJ/reference/countGLM.html)
fits all six likelihood-based families and selects the best by a
user-chosen criterion (`decide = "BIC"` by default; also accepts
`"AIC"`, `"LogLik"`, or `"McFadden"`), annotated with dispersion and
zero-inflation diagnostics. When the Poisson fit shows dispersion ratio
\> 1.2 combined with a roughly flat squared-Pearson-residual cloud
sitting above 1 (the quasi-Poisson signature), `quasiPoissonGLM()` is
also fitted and reported alongside — but excluded from the AIC/BIC/
log-likelihood comparison, since quasi-likelihood has no proper
likelihood.

## Package functions

### Data summarization

- **`summarizeCountData(formula, data)`** — Produces numerical summaries
  (mean, variance, dispersion ratio, zero count) and a plot of the
  count response against predictors. The plot type adapts to the number
  and type of predictors (histogram, scatter, violin, heatmap, etc.). A GGally::ggpairs() plot is also returned.


### Model fitting

Each individual fitter returns exponentiated coefficients with 95% Wald
confidence intervals, randomized quantile residuals, a Pearson
dispersion ratio, and a two-panel diagnostic plot (fitted values vs. RQR
\| normal Q-Q of RQR).

- **`poissonGLM(formula, data)`** — Poisson regression via
  [`stats::glm`](https://rdrr.io/r/stats/glm.html).
- **`quasiPoissonGLM(formula, data)`** — Quasi-Poisson regression via
  [`stats::glm`](https://rdrr.io/r/stats/glm.html) with
  `family = quasipoisson()`. Same point estimates as Poisson, but
  standard errors (and hence CIs and p-values) are scaled by the
  estimated dispersion `phi`. AIC/BIC are `NA` (no proper likelihood).
- **`negbinGLM(formula, data)`** — Negative binomial regression via
  [`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html).
- **`zeroinflPoissonGLM(formula, data, ziformula)`** — Zero-inflated
  Poisson via
  [`pscl::zeroinfl`](https://rdrr.io/pkg/pscl/man/zeroinfl.html).
  Separate coefficient tables for the count and zero-inflation
  components.
- **`zeroinflNegbinGLM(formula, data, ziformula)`** — Zero-inflated
  negative binomial via
  [`pscl::zeroinfl`](https://rdrr.io/pkg/pscl/man/zeroinfl.html).
- **`tweedieGLM(formula, data)`** — Tweedie GLM via
  [`glmmTMB::glmmTMB`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html).
  Returns the estimated dispersion `phi` and power parameter `p`.
- **`zeroinflTweedieGLM(formula, data, ziformula)`** — Zero-inflated
  Tweedie via
  [`glmmTMB::glmmTMB`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html).
  Separate coefficient tables for the count and zero-inflation
  components.

### Model selection

- **`countGLM(formula, data, ziformula, decide = "BIC")`** — Fits all six
  families, selects the best by the criterion in `decide` (`"BIC"`,
  `"AIC"`, `"LogLik"`, or `"McFadden"`), and returns a plain-language
  recommendation informed by the Pearson dispersion ratio and observed
  vs. expected zero counts. Each zero-inflated counterpart (Poisson,
  Negative Binomial, Tweedie) is fitted only when DHARMa detects
  significant zero-inflation in its base model.

## Condition checking

All model fitters compute:

- **Randomized quantile residuals (RQR)** — For a well-fitting model,
  RQRs should be approximately standard normal. The diagnostic plot
  shows them against fitted values (checking for structure) and on a
  normal Q-Q plot.
- **Pearson dispersion ratio** — Pearson chi-squared divided by residual
  degrees of freedom. Values near 1 are consistent with the assumed
  model; values substantially above 1 (rule of thumb: \> 1.2 from a
  Poisson fit) suggest overdispersion and motivate switching to negative
  binomial and/or a zero-inflated model.
- **Sample size conditions** — The package checks that the dataset has
  enough observations relative to the number of predictors; a warning is
  issued when the effective sample-to-parameter ratio is too low to
  support reliable estimation.
- **Variance Inflation Factors (VIFs)** — VIFs are computed for each
  predictor to flag multicollinearity. Predictors with a VIF above 5 are
  flagged with a warning so users can investigate correlated predictors
  before interpreting coefficients.

## Interpretation

### `interpret_coef()`

**`interpret_coef(model, predictor, component = "count")`** generates a
plain-language sentence for a single exponentiated coefficient, with a
95% Wald CI. It works with all model types returned by the package:

``` r
interpret_coef(fit, "pctnonwhite10")
#> Holding all other predictors constant, a one-unit increase in pctnonwhite10 is
#> associated with a 12.4% increase in the expected count of y
#> (exp(β) = 1.124, 95% CI: [1.047, 1.207]).
```

Key behaviours:

- **Offset-aware** — if the model includes an offset, the outcome phrase
  becomes "the expected *rate* of `y` per unit of exposure" rather than
  the expected count.
- **Zero-inflated models** — pass `component = "count"` (default) for
  the count sub-model or `component = "zero"` for the zero-inflation
  sub-model. The zero component phrase reads "the odds of being a
  structural zero".
- **`countGLM` objects** — automatically delegates to the best-fitting
  model and prints a message indicating which one is used.
- **Non-significant note** — if the coefficient's p-value exceeds 0.05,
  a note is appended: *"this coefficient is not discernibly different
  from zero (p = …)"*.

The term name passed to `predictor` must match exactly the string in the
coefficient table (e.g. `"EPAregion2"`, not `"EPAregion"`). If the term
is not found, the function stops with a list of available terms.

### `untangle_interaction()`

**`untangle_interaction(model, interaction, average.over = NULL, standardized = FALSE, n = 100, overridePlot = FALSE)`**
produces post-hoc summaries that describe how two interacting predictors
jointly shape the fitted response. The output adapts to the types of the
two variables.

#### Categorical × Categorical

Returns a data frame of estimated marginal means
([`emmeans::emmeans()`](https://rdrr.io/pkg/emmeans/man/emmeans.html))
for every combination of the two factors, plus a ggplot of those means
with 95% error bars:

``` r
res <- untangle_interaction(fit, c("metro", "EPAregion"))
res$emmeans   # data frame: Mean, SE, df, Lower, Upper per cell
res$plot      # geom_point + geom_errorbar, faceted by extra categoricals
```

#### Categorical × Continuous

Returns the slope of the continuous predictor at each level of the
factor (on the link scale, via
[`emmeans::emtrends()`](https://rdrr.io/pkg/emmeans/man/emtrends.html)),
plus a ggplot of predicted responses across `n` evenly-spaced values of
the continuous predictor coloured by the factor:

``` r
res <- untangle_interaction(fit, c("metro", "pctnonwhite10"))
res$emtrends   # Slope, SE, df, Lower, Upper per factor level
res$plot       # geom_line + geom_ribbon, faceted by extra categoricals
```

#### Continuous × Continuous

Returns slopes of each predictor at low (mean − SD), medium (mean), and
high (mean + SD) values of the other, plus Johnson-Neyman intervals
([`interactions::johnson_neyman()`](https://rdrr.io/pkg/interactions/man/johnson_neyman.html),
FDR-corrected) in both directions:

``` r
res <- untangle_interaction(fit, c("pctnonwhite10", "gdp2017b"))
res$emtrends_pctnonwhite10          # slopes of pctnonwhite10 at 3 levels of gdp2017b
res$emtrends_gdp2017b               # slopes of gdp2017b at 3 levels of pctnonwhite10
res$johnson_neyman_pctnonwhite10_by_gdp2017b  # J-N object; $plot for the figure
res$johnson_neyman_gdp2017b_by_pctnonwhite10  # swapped direction
```

Pass `standardized = TRUE` when predictors have been pre-standardised
(mean 0, SD 1): the low/medium/high grid becomes −1, 0, 1 and the
printed interpretation refers to a "one-standard-deviation increase".

#### Controlling which categoricals are averaged over

By default, every categorical predictor in the model *other* than the
two interaction variables is kept separate — emmeans reports each level
rather than collapsing across it. Pass names to `average.over` to opt
specific variables into averaging:

``` r
# EPAregion has 10 levels; facet count would exceed 8 → plot suppressed
untangle_interaction(fit, c("metro", "pctnonwhite10"))

# Permit averaging over EPAregion → single-panel plot returned
untangle_interaction(fit, c("metro", "pctnonwhite10"),
                     average.over = "EPAregion")

# Force the plot regardless of facet count
untangle_interaction(fit, c("metro", "pctnonwhite10"),
                     overridePlot = TRUE)
```

When the number of facets would exceed 8 and `overridePlot = FALSE`, the
plot is silently replaced with `NULL` and a warning is issued. The data
frames are always returned regardless.

All three functions accept every model type returned by the package, as
well as raw `glm` and `glmmTMB` fits. For `countGLM` objects, the
best-fitting component model is used automatically.

## Installation

``` r
# Install from GitHub
# install.packages("pak")
pak::pak("ojaroker/glmOJ")
```
