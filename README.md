# glmOJ

**glmOJ** is an R package that provides a structured workflow for count regression modeling. It wraps `glm`, `MASS::glm.nb`, and `pscl::zeroinfl` to fit, diagnose, and interpret count regression models, with all output reported on the response scale rather than the link scale.

## What the package does

Count data — non-negative integers like the number of events, species, or visits — require specialized regression models. glmOJ guides the analyst through four steps:

1. **Summarize** the response variable and its relationship to predictors before modeling.
2. **Fit** one or more count regression models from the supported families.
3. **Assess** model conditions using randomized quantile residuals (RQR) and the Pearson dispersion ratio.
4. **Interpret** all output on the response scale (exponentiated coefficients with confidence intervals).

## Supported model families

| Family | Function | Use when |
|--------|----------|----------|
| Poisson | `poissonGLM()` | Counts with mean ≈ variance |
| Negative binomial | `negbinGLM()` | Counts with variance > mean (overdispersion) |
| Zero-inflated Poisson | `zeroinflPoissonGLM()` | Excess zeros + equidispersed non-zero counts |
| Zero-inflated negative binomial | `zeroinflNegbinGLM()` | Excess zeros + overdispersed non-zero counts |

The general-purpose `countGLM()` fits all four families and selects the best by AIC, annotated with dispersion and zero-inflation diagnostics.

## Package functions

### Data summarization

- **`summarizeCountData(formula, data)`** — Produces numerical summaries (mean, variance, variance-to-mean ratio, zero count) and a plot of the count response against predictors. The plot type adapts to the number and type of predictors (histogram, scatter, violin, heatmap, etc.).

### Model fitting

Each individual fitter returns exponentiated coefficients with 95% Wald confidence intervals, randomized quantile residuals, a Pearson dispersion ratio, and a two-panel diagnostic plot (fitted values vs. RQR | normal Q-Q of RQR).

- **`poissonGLM(formula, data)`** — Poisson regression via `stats::glm`.
- **`negbinGLM(formula, data)`** — Negative binomial regression via `MASS::glm.nb`.
- **`zeroinflPoissonGLM(formula, data, ziformula)`** — Zero-inflated Poisson via `pscl::zeroinfl`. Separate coefficient tables for the count and zero-inflation components.
- **`zeroinflNegbinGLM(formula, data, ziformula)`** — Zero-inflated negative binomial via `pscl::zeroinfl`.

### Model selection

- **`countGLM(formula, data, ziformula)`** — Fits all four families, selects the best by AIC, and returns a plain-language recommendation informed by the Pearson dispersion ratio and observed vs. expected zero counts.

## Condition checking

All model fitters compute:

- **Randomized quantile residuals (RQR)** — For a well-fitting model, RQRs should be approximately standard normal. The diagnostic plot shows them against fitted values (checking for structure) and on a normal Q-Q plot.
- **Pearson dispersion ratio** — Pearson chi-squared divided by residual degrees of freedom. Values near 1 are consistent with the assumed model; values substantially above 1 (rule of thumb: > 1.5 from a Poisson fit) suggest overdispersion and motivate switching to negative binomial.

## Coefficient interpretation

All coefficients are reported on the **response scale** (exponentiated). For the count component of any model, exponentiating a coefficient gives the multiplicative change in the expected count for a one-unit increase in the predictor, adjusting for simultaneous linear changes in other predictors. For example, a value of 1.5 means a 50% higher expected count.

For the zero-inflation component of ZI models, the exponentiated coefficient gives the multiplicative change in the odds of being a structural zero.

## Installation

```r
# Install from GitHub
# install.packages("pak")
pak::pak("ojaroker/glmOJ")
```
