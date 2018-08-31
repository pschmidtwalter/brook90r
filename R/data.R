#' Meteorological Data from the Solling Beech and Spruce experimental site
#'
#' A dataset containing daily weather variables for the period 1960-2013
#'
#' @format A data.frame with 19724 rows and 9 variables
#' \describe{
#'   \item{dates}{date}
#'   \item{tmin}{daily minimum temperature, degC}
#'   \item{tmax}{daily maximum temperature, degC}
#'   \item{tmean}{daily mean temperature, degC}
#'   \item{prec}{daily sum of precipitation, mm}
#'   \item{vappres}{daily vapour pressure, kPa, }
#'   \item{globrad}{daily sum of global radiation, MJ m-2}
#'   \item{wind}{daily mean wind speed measured at 10 m above ground, m s-2}
#' }
"meteo_slb1"

#' Soil profile data from the Solling Beech experimental site 'SLB1'
#'
#' A dataset containing the soil horizons' physical properties
#'
#' @format A data.frame with 20 rows and 11 variables
#' \describe{
#'   \item{horizon}{horizon symbol}
#'   \item{upper}{upper layer boundary, m}
#'   \item{lower}{lower layer boundary, m}
#'   \item{texture}{soil texture according to German soil texture classification system}
#'   \item{bd}{bulk density of the fine earth, g cm-3}
#'   \item{gravel}{fraction of coarse material }
#'   \item{sand}{sand content, mass-\% }
#'   \item{silt}{silt content, mass-\% }
#'   \item{clay}{clay content, mass-\% }
#'   \item{c_org}{organic carbon content, mass-\%}
#' }
"soil_slb1"



