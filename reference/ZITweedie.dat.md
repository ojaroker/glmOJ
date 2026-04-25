# Simulated zero-inflated Tweedie count data

A simulated dataset with a strong zero-inflation component, designed to
illustrate where
[`zeroinflTweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/zeroinflTweedieGLM.md)
fits clearly better than the base
[`tweedieGLM()`](http://oscar.jaroker.com/glmOJ/reference/tweedieGLM.md).
The count component depends only on `x1`; the zero-inflation component
is driven solely by `x2` (which is independent of `x1`). Generated from
a compound Poisson-Gamma (Tweedie, p = 1.5) with structural zeros, then
ceiling()-ed to non-negative integers.

## Usage

``` r
ZITweedie.dat
```

## Format

A data frame with 400 rows and 3 columns:

- y:

  Non-negative integer count response.

- x1:

  Continuous predictor for the count component.

- x2:

  Continuous predictor for the zero-inflation component (independent of
  `x1`).

## Source

Simulated; see `data-raw/DATASET.R` in the package source for the
generating script.

## Details

True data-generating model:

- Count component: `log(mu) = 1.8 + 0.9 * x1`, with `phi = 2.0` and
  Tweedie power `p = 1.5`.

- Zero-inflation component: `logit(pi) = 0.5 - 2.5 * x2`.

Approximately 61% of observed responses are zero.
