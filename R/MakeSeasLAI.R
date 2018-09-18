#' Construct the seasonal course of leaf area index from parameters
#'
#' A daily sequence of leaf area index is derived from maximum and minimum values,
#' dates and shape parameters using different methods.
#'
#' @param method name of method for generating the sequence. Must be one of "b90", "linear", "Coupmodel"
#' @param year year to identify leap years
#' @param maxlai maximum value during summer
#' @param winlaifrac minimum lai as fraction of maxlai during winter (not used when method = 'linear')
#' @param budburst.doy budburst day of year (not used when method = 'linear')
#' @param leaffall.doy day of year when leaf fall begins (not used when method = 'linear')
#' @param emerge.dur number of days from budburst until maximum leaf area index is reached
#' @param leaffall.dur number of days until minimum leaf are index is reached
#' @param opt.doy day of year when optimum value is reached (required when method = "Coupmodel")
#' @param shape.budburst shape parameter for the growth phase (required when method = "Coupmodel")
#' @param shape.leaffall shape parameter growth cessation (required when method = "Coupmodel")
#' @param lai.doy integer vector of days of years
#' @param lai.frac vector of values of fractional leaf area index corresponding
#' to lai.doy (required when method = "linear")
#'
#' @return  a vector of daily lai values for one year
#' @export
MakeSeasLAI <- function(method="b90",
                        year,
                        maxlai,
                        winlaifrac = 0, #LAI in Winter
                        budburst.doy = 121, #Budburst DOY
                        leaffall.doy = 279, # DOY when leaffall is begins
                        emerge.dur = 28, # Duration until maximum LAI is reached (b90)
                        leaffall.dur = 58, # Duration until Winter LAI is reached  (b90)
                        shape.optdoy=220, #MaxLAI DOY (Coupmodel)
                        shape.budburst=0.5, #Form parameter for leaf expension period (Coupmodel)
                        shape.leaffall=10, #Form parameter for leaf Fall (Coupmodel)
                        lai.doy = c(1,121,150,280,320,365),
                        lai.frac = c(0,0,0.5,1,0.5,0)) {
  method <- match.arg(method, choices = c("b90", "linear", "Coupmodel"))

  if (method %in% c("b90", "Coupmodel")) {

    dat <- suppressWarnings(
      data.table(year = year, maxlai = maxlai, winlaifrac = winlaifrac,
                 budburst.doy = budburst.doy, leaffall.doy = leaffall.doy,
                 emerge.dur = emerge.dur, leaffall.dur = leaffall.dur,
                 shape.optdoy = shape.optdoy, shape.budburst = shape.budburst,
                 shape.leaffall = shape.leaffall)
    )

    dat[,maxdoy := ifelse( ((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0),
                           366, 365)]
    dat[, minlai := winlaifrac*maxlai]

    if (method == "b90") {
      out <- dat[, brook90r:::plant.b90(minval = minlai, maxval = maxlai,
                                        doy.incr = budburst.doy, incr.dur = emerge.dur,
                                        doy.decr = leaffall.doy, decr.dur = leaffall.dur,
                                        maxdoy = maxdoy),
                 by = year]$V1
    }
    if (method == "Coupmodel") {
      out <- dat[, brook90r:::plant.coupmodel(minval = minlai, maxval = maxlai,
                                              doy.incr = budburst.doy,
                                              doy.max = shape.optdoy,
                                              doy.min = leaffall.doy + leaffall.dur,
                                              shape.incr = shape.budburst, shape.decr = shape.leaffall,
                                              maxdoy = maxdoy),
                 by = year]$V1
    }

  } else {

    dat <- data.table(year,
                      maxdoy = ifelse( ((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0),
                                       366, 365),
                      maxlai = maxlai)
    out <- dat[, brook90r:::plant.linear(doys = lai.doy, values = lai.frac*maxlai, maxdoy = maxdoy),
               by = year]$V1

  }

  return(out)

}






