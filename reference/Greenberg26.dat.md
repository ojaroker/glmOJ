# County-level environmental enforcement penalties (Greenberg 2026)

County-level data combining U.S. EPA Environmental Quality Index (EQI)
summary scores with the count of EPA enforcement penalties levied
against regulated facilities in each county over a multi-year window.
Used in glmOJ's vignette to demonstrate Poisson and negative-binomial
count regression with continuous EQI exposure and categorical region
covariates.

## Usage

``` r
Greenberg26.dat
```

## Format

A data frame with 3,139 rows and 27 columns:

- countyname:

  County name (character).

- statename:

  State name (character).

- fips_1:

  County FIPS code (integer).

- sociod_eqi_2jan2018_vc:

  Sociodemographic EQI summary score (z-scored).

- air_eqi_2jan2018_vc:

  Air-quality EQI summary score (z-scored).

- land_eqi_2jan2018_vc:

  Land EQI summary score (z-scored).

- water_eqi_2jan2018_vc:

  Water EQI summary score (z-scored).

- eqi_2jan2018_vc:

  Overall EPA Environmental Quality Index (z-scored).

- pctnonwhite10:

  Percent non-white population (2010 census).

- gdp2017b:

  County GDP, 2017 (billions of USD).

- metro:

  Factor: `0` non-metropolitan, `1` metropolitan.

- fac_penalty_count:

  Integer count of facility-level EPA penalties (the response used in
  the vignette).

- CIDDist:

  Distance to the nearest community-information district.

- FinalEC:

  Final environmental-compliance indicator (binary).

- estab:

  Number of regulated establishments in the county.

- EPAregion:

  Factor with 10 levels (EPA administrative regions 1-10).

- cases_2011:

  Year-2011 case count.

- cases_2012:

  Year-2012 case count.

- cases_2013:

  Year-2013 case count.

- cases_2014:

  Year-2014 case count.

- cases_2015:

  Year-2015 case count.

- cases_2016:

  Year-2016 case count.

- cases_2017:

  Year-2017 case count.

- cases_2018:

  Year-2018 case count.

- cases_2019:

  Year-2019 case count.

- cases_2020:

  Year-2020 case count.

- proportionObama:

  Indicator / proportion of Obama vote at the county level (binary in
  this extract).

## Source

Compiled from the U.S. EPA Environmental Quality Index and the ECHO
enforcement database; see Greenberg (2026).
