# Census-tract street-camera and crime data (Dahir 2025)

Census-tract-level observations of CCTV / surveillance-camera counts
paired with sociodemographic and crime indicators from ten U.S. cities.
Continuous covariates are city-centred and scaled. Used in glmOJ's
vignette to demonstrate count regression with an exposure offset
(`offset(log_road_length)`) and high-cardinality factor predictors.

## Usage

``` r
Dahir25.dat
```

## Format

A data frame with 11,620 rows and 19 columns:

- city:

  Factor with 10 U.S. city levels.

- GEOID:

  Census tract GEOID.

- cam_count:

  Integer count of street cameras in the tract (the response used in the
  vignette).

- image_count:

  Integer count of street-view images sampled.

- pnhwht:

  Percent non-Hispanic white population (city-scaled).

- pnhblk:

  Percent non-Hispanic Black population (city-scaled).

- pasian:

  Percent Asian population.

- phisp:

  Percent Hispanic population.

- entropy:

  Race-ethnicity diversity entropy (city-scaled).

- entropy_rank:

  Within-city percentile rank of entropy.

- total_crime_rate:

  Total crime rate (city-scaled).

- viol_veh_crime_rate:

  Violent and vehicle crime rate.

- pop:

  Tract population (city-scaled).

- hinc:

  Median household income (city-scaled).

- pvac:

  Vacancy rate (city-scaled).

- mhmval:

  Median home value (city-scaled).

- modal_zone:

  Factor: dominant land-use zoning (`commercial`, `industrial`, `mixed`,
  `public`, `residential`, `roads`).

- road_length_km:

  Tract road length in kilometres.

- log_road_length:

  Natural log of `road_length_km`, used as an exposure offset.

## Source

Replication data from Dahir et al. (2025).
