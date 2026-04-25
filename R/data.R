# Documentation for bundled datasets.

#' County-level environmental enforcement penalties (Greenberg 2026)
#'
#' County-level data combining U.S. EPA Environmental Quality Index (EQI)
#' summary scores with the count of EPA enforcement penalties levied against
#' regulated facilities in each county over a multi-year window. Used in
#' \pkg{glmOJ}'s vignette to demonstrate Poisson and negative-binomial
#' count regression with continuous EQI exposure and categorical region
#' covariates.
#'
#' @format A data frame with 3,139 rows and 27 columns:
#'   \describe{
#'     \item{countyname}{County name (character).}
#'     \item{statename}{State name (character).}
#'     \item{fips_1}{County FIPS code (integer).}
#'     \item{sociod_eqi_2jan2018_vc}{Sociodemographic EQI summary score (z-scored).}
#'     \item{air_eqi_2jan2018_vc}{Air-quality EQI summary score (z-scored).}
#'     \item{land_eqi_2jan2018_vc}{Land EQI summary score (z-scored).}
#'     \item{water_eqi_2jan2018_vc}{Water EQI summary score (z-scored).}
#'     \item{eqi_2jan2018_vc}{Overall EPA Environmental Quality Index (z-scored).}
#'     \item{pctnonwhite10}{Percent non-white population (2010 census).}
#'     \item{gdp2017b}{County GDP, 2017 (billions of USD).}
#'     \item{metro}{Factor: `0` non-metropolitan, `1` metropolitan.}
#'     \item{fac_penalty_count}{Integer count of facility-level EPA penalties
#'       (the response used in the vignette).}
#'     \item{CIDDist}{Distance to the nearest community-information district.}
#'     \item{FinalEC}{Final environmental-compliance indicator (binary).}
#'     \item{estab}{Number of regulated establishments in the county.}
#'     \item{EPAregion}{Factor with 10 levels (EPA administrative regions 1-10).}
#'     \item{cases_2011}{Year-2011 case count.}
#'     \item{cases_2012}{Year-2012 case count.}
#'     \item{cases_2013}{Year-2013 case count.}
#'     \item{cases_2014}{Year-2014 case count.}
#'     \item{cases_2015}{Year-2015 case count.}
#'     \item{cases_2016}{Year-2016 case count.}
#'     \item{cases_2017}{Year-2017 case count.}
#'     \item{cases_2018}{Year-2018 case count.}
#'     \item{cases_2019}{Year-2019 case count.}
#'     \item{cases_2020}{Year-2020 case count.}
#'     \item{proportionObama}{Indicator / proportion of Obama vote at the
#'       county level (binary in this extract).}
#'   }
#' @source Compiled from the U.S. EPA Environmental Quality Index and the
#'   ECHO enforcement database; see Greenberg (2026).
#' @keywords datasets
"Greenberg26.dat"

#' Census-tract street-camera and crime data (Dahir 2025)
#'
#' Census-tract-level observations of CCTV / surveillance-camera counts paired
#' with sociodemographic and crime indicators from ten U.S. cities. Continuous
#' covariates are city-centred and scaled. Used in \pkg{glmOJ}'s vignette to
#' demonstrate count regression with an exposure offset
#' (`offset(log_road_length)`) and high-cardinality factor predictors.
#'
#' @format A data frame with 11,620 rows and 19 columns:
#'   \describe{
#'     \item{city}{Factor with 10 U.S. city levels.}
#'     \item{GEOID}{Census tract GEOID.}
#'     \item{cam_count}{Integer count of street cameras in the tract
#'       (the response used in the vignette).}
#'     \item{image_count}{Integer count of street-view images sampled.}
#'     \item{pnhwht}{Percent non-Hispanic white population (city-scaled).}
#'     \item{pnhblk}{Percent non-Hispanic Black population (city-scaled).}
#'     \item{pasian}{Percent Asian population.}
#'     \item{phisp}{Percent Hispanic population.}
#'     \item{entropy}{Race-ethnicity diversity entropy (city-scaled).}
#'     \item{entropy_rank}{Within-city percentile rank of entropy.}
#'     \item{total_crime_rate}{Total crime rate (city-scaled).}
#'     \item{viol_veh_crime_rate}{Violent and vehicle crime rate.}
#'     \item{pop}{Tract population (city-scaled).}
#'     \item{hinc}{Median household income (city-scaled).}
#'     \item{pvac}{Vacancy rate (city-scaled).}
#'     \item{mhmval}{Median home value (city-scaled).}
#'     \item{modal_zone}{Factor: dominant land-use zoning
#'       (`commercial`, `industrial`, `mixed`, `public`, `residential`,
#'       `roads`).}
#'     \item{road_length_km}{Tract road length in kilometres.}
#'     \item{log_road_length}{Natural log of `road_length_km`, used as an
#'       exposure offset.}
#'   }
#' @source Replication data from Dahir et al. (2025).
#' @keywords datasets
"Dahir25.dat"

#' Simulated zero-inflated Tweedie count data
#'
#' A simulated dataset with a strong zero-inflation component, designed to
#' illustrate where [zeroinflTweedieGLM()] fits clearly better than the base
#' [tweedieGLM()]. The count component depends only on `x1`; the
#' zero-inflation component is driven solely by `x2` (which is independent of
#' `x1`). Generated from a compound Poisson-Gamma (Tweedie, p = 1.5) with
#' structural zeros, then ceiling()-ed to non-negative integers.
#'
#' True data-generating model:
#' \itemize{
#'   \item Count component: `log(mu) = 1.8 + 0.9 * x1`, with `phi = 2.0`
#'         and Tweedie power `p = 1.5`.
#'   \item Zero-inflation component: `logit(pi) = 0.5 - 2.5 * x2`.
#' }
#' Approximately 61\% of observed responses are zero.
#'
#' @format A data frame with 400 rows and 3 columns:
#'   \describe{
#'     \item{y}{Non-negative integer count response.}
#'     \item{x1}{Continuous predictor for the count component.}
#'     \item{x2}{Continuous predictor for the zero-inflation component
#'       (independent of `x1`).}
#'   }
#' @source Simulated; see `data-raw/DATASET.R` in the package source for the
#'   generating script.
#' @keywords datasets
"ZITweedie.dat"
