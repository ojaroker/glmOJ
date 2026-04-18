# Untangle a two-way interaction in a fitted model

Produces post-hoc marginal means and/or slopes that describe how two
interacting predictors jointly shape the fitted model. The behaviour
adapts to the types of the two variables:

- **categorical x categorical:** estimated marginal means
  ([`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html))
  for every combination of the two factors, plus a faceted ggplot of
  those means with 95% error bars. Faceting is by any categorical
  predictor not listed in `average.over`. The plot is suppressed (with a
  warning) when the number of facets exceeds 8, unless
  `overridePlot = TRUE`.

- **categorical x continuous:** slopes of the continuous predictor at
  each level of the factor
  ([`emmeans::emtrends()`](https://rvlenth.github.io/emmeans/reference/emtrends.html)),
  plus a faceted ggplot of predicted responses across `n` equally-spaced
  values of the continuous predictor. Faceting is by any categorical
  predictor not listed in `average.over`. The plot is suppressed (with a
  warning) when the number of facets exceeds 8, unless
  `overridePlot = TRUE`.

- **continuous x continuous:** slopes of each predictor at low, medium
  and high values of the other
  ([`emmeans::emtrends()`](https://rvlenth.github.io/emmeans/reference/emtrends.html)),
  plus Johnson-Neyman intervals in both directions
  (`interactions::johnson_neyman()`).

## Usage

``` r
untangle_interaction(
  model,
  interaction,
  average.over = NULL,
  standardized = FALSE,
  n = 100,
  overridePlot = FALSE
)
```

## Arguments

- model:

  A fitted model. Either an object returned by
  [`countGLM()`](http://oscar.jaroker.com/glmOJ/reference/countGLM.md)
  or one of the package's individual fitters
  ([`poissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/poissonGLM.md),
  [`negbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/negbinGLM.md),
  [`tweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/tweedieGLM.md),
  [`zeroinflPoissonGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflPoissonGLM.md),
  [`zeroinflNegbinGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflNegbinGLM.md),
  [`zeroinflTweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflTweedieGLM.md)),
  or a raw `glm` / `glmmTMB` fit. For `countGLM` objects the
  best-fitting component model is used and a message is printed.

- interaction:

  The pair of interacting variables. Either a length-2 character vector
  (e.g. `c("x7", "x8")`) or a one-sided formula (e.g. `~ x7:x8` or
  `~ x7*x8`).

- average.over:

  Character vector of *categorical* predictor names that may be averaged
  over by emmeans. Default `NULL` means do not average over any
  categorical predictor; each level is reported separately.

- standardized:

  Logical; set `TRUE` when the continuous predictors have been
  standardized to mean 0 and SD 1. In that case the low / medium / high
  values used for continuous x continuous slopes are `c(-1, 0, 1)`
  (standard deviations), and the interpretation text refers to a
  "one-standard-deviation increase" rather than a "one-unit increase".
  Default `FALSE`.

- n:

  Integer; number of equally-spaced points used for the continuous
  predictor in the categorical x continuous emmeans grid backing the
  plot. Default 100.

- overridePlot:

  Logical; when `TRUE`, the faceted plot in the categorical x
  categorical and categorical x continuous cases is built and returned
  even if the number of facets exceeds 8. Default `FALSE`.

## Value

A list with class `"untangle_interaction"` whose contents depend on the
interaction type:

- `type`:

  One of `"categorical-categorical"`, `"categorical-continuous"`,
  `"continuous-continuous"`.

- `variables`:

  Character vector of the two predictor names.

- `emmeans`:

  (cat x cat) data frame with columns for each factor, `Mean`, `SE`,
  `df`, `Lower`, `Upper`.

- `plot`:

  (cat x cat) faceted ggplot of estimated means with 95% error bars.
  `NULL` when the number of facets exceeds 8 and `overridePlot = FALSE`.

- `emtrends`:

  (cat x cont) data frame with `Slope`, `SE`, `df`, `Lower`, `Upper` at
  each level of the factor.

- `plot`:

  (cat x cont) faceted ggplot of predicted responses vs the continuous
  predictor, coloured by the factor, with a 95% confidence ribbon.
  `NULL` when the number of facets exceeds 8 and `overridePlot = FALSE`.

- `emtrends_<v1>`, `emtrends_<v2>`:

  (cont x cont) data frames of slopes of each predictor at
  low/medium/high values of the other.

- `johnson_neyman_<v1>_by_<v2>`, `johnson_neyman_<v2>_by_<v1>`:

  (cont x cont) Johnson-Neyman objects from
  `interactions::johnson_neyman()`. `NULL` if the call failed (e.g.
  unsupported model class).

- `interpretation`:

  A plain-language description, also printed.

## Details

By default, the function never averages over *categorical* predictors
outside the interaction of interest: results are reported separately for
each level. Predictors listed in `average.over` are averaged over
instead. Continuous predictors outside the interaction are always held
at their mean (the emmeans default) and therefore implicitly averaged in
a linear sense.

`emmeans` and `interactions` are declared in `Suggests`; both must be
installed before calling this function.

All emmeans summaries are returned on the response scale when a known
link is present. Slopes from `emtrends` are returned on the **linear
predictor (link) scale** — on a log link, a positive slope means the
expected response grows multiplicatively with the predictor.

## See also

[`interpret_coef()`](http://oscar.jaroker.com/glmOJ/reference/interpret_coef.md),
[`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html),
[`emmeans::emtrends()`](https://rvlenth.github.io/emmeans/reference/emtrends.html),
`interactions::johnson_neyman()`

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- poissonGLM(y ~ x1 * x2 + x3, data = df)
untangle_interaction(fit, c("x1", "x2"))
untangle_interaction(fit, ~ x1:x2, average.over = "x3")
untangle_interaction(fit, c("x7", "x8"), standardized = TRUE)
} # }
```
