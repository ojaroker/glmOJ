# Plot count data against predictors

Internal plotting function called by
[`summarizeCountData`](http://oscar.jaroker.com/glmOJ/reference/summarizeCountData.md).
Selects an appropriate `ggplot` based on the number and type of
predictors supplied.

## Usage

``` r
plot_count_data(y, mf, pred_vars, pred_types, bins = 20)
```

## Arguments

- y:

  Numeric vector of non-negative integer counts (the response).

- mf:

  A model frame returned by
  [`model.frame`](https://rdrr.io/r/stats/model.frame.html).

- pred_vars:

  Character vector of predictor variable names to plot. If more than two
  names are supplied, only the first two are used and a warning is
  issued.

- pred_types:

  Named character vector classifying each element of `pred_vars` as
  either `"continuous"` or `"categorical"`.

- bins:

  Integer; number of bins on each axis for the two-continuous predictor
  2D bin plot. Default 20.

## Value

A `ggplot` object.

## Details

See
[`summarizeCountData`](http://oscar.jaroker.com/glmOJ/reference/summarizeCountData.md)
for the full description of which plot type is produced for each
predictor combination.

## See also

[`summarizeCountData`](http://oscar.jaroker.com/glmOJ/reference/summarizeCountData.md)
