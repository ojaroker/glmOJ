# Title for countGLM

Short description of what countGLM does.

## Usage

``` r
countGLM(
  formula,
  data,
  family = poisson(),
  weights = NULL,
  subset = NULL,
  na.action = getOption("na.action"),
  control = glm.control(),
  offset = NULL,
  ...
)
```

## Arguments

- formula:

  A model formula (e.g. `y ~ x1 + x2`).

- data:

  A data frame containing the variables in `formula`.

- family:

  A description of the error distribution and link function to be used
  in the model (default:
  [`gaussian()`](https://rdrr.io/r/stats/family.html)).

- weights:

  Optional numeric vector of observation weights.

- subset:

  Optional expression indicating subset of rows to use.

- na.action:

  Function which indicates what should happen when the data contain
  `NA`s.

- control:

  A list of control parameters (document the elements you use; e.g. from
  `glm.control`).

- offset:

  Optional offset.

- ...:

  Additional arguments passed to lower-level methods.

## Value

An object of class `countGLM` (a list containing at least:
`coefficients`, `fitted.values`, `residuals`, `vcov`, `converged`).
Describe components precisely.

## Details

Longer description of algorithm, assumptions, and any important notes
(e.g. handling of singularities, convergence criteria, warnings
produced).

## See also

[glm](https://rdrr.io/r/stats/glm.html),
[model.frame](https://rdrr.io/r/stats/model.frame.html)

## Examples

``` r
# fm = countGLM(y ~ x1 + x2, data = dat, family = poisson())


```
