
#' Create daily seasonal plant dynamics sequence (e.g. leaf area index development)
#' for one year
#'
#' Creates the seasonal course of leaf area index, based
#' on minimum lai, maximum lai, budburst and leaffall day of year (doy) and shape
#' parameters.
#'
#' @param method name of method for generating the sequence. Must be one of "b90", "linear", "Coupmodel"
#' @param maxlai maximum value during summer (not used when method = 'linear')
#' @param winlaifrac minimum lai as fraction of maxlai during winter (not used when method = 'linear')
#' @param budburst.doy day of year when growth begins (not used when method = 'linear')
#' @param leaffall.doy day of year when growth cessates (not used when method = 'linear')
#' @param emerge.dur number of days until maximum value is reached (required when method = "Coupmodel")
#' @param leaffall.dur number of days until minimum value is reached (required when method = "b90")
#' @param opt.doy day of year when optimum value is reached (required when method = "Coupmodel")
#' @param shape.budburst shape parameter for the growth phase (required when method = "Coupmodel")
#' @param shape.leaffall shape parameter growth cessation (required when method = "Coupmodel")
#' @param lai.doy vector of integers (required when method = "linear")
#' @param lai.frac vector of values corresponding to doy.values (required when method = "linear")
#' @param maxdoy last day of year (leapyear: 366, else 365)
#'
#' @return Returns a seasonal sequence of length maxdoy
#' @export
MakeSeasLAI <- function(method="b90",
                        maxlai,
                        winlaifrac = 0, #LAI in Winter
                        budburst.doy = 121, #Budburst DOY
                        leaffall.doy = 279, # DOY when leaffall is begins
                        emerge.dur = 28, # Duration until maximum LAI is reached (b90)
                        leaffall.dur = 58, # Duration until Winter LAI is reached  (b90)
                        shape.optdoy=220, #MaxLAI DOY (Coupmodel)
                        shape.budburst=0.5, #Formparameter for leaf expension period (Coupmodel)
                        shape.leaffall=10, #Formparameter for leaf Fall (Coupmodel)
                        lai.doy = c(1,121,150,280,320,maxdoy),
                        lai.frac = c(0,0,0.5,1,0.5,0),
                        maxdoy) {

  method <- match.arg(method, choices = c("b90", "linear", "Coupmodel"))
  minlai <- winlaifrac*maxlai
  if (method == "b90") {
    IndDays <- c(1,budburst.doy,budburst.doy + emerge.dur,leaffall.doy ,leaffall.doy+leaffall.dur,maxdoy)
    LAIvalues <- c(minlai,minlai,maxlai,maxlai,minlai,minlai)
    LAI <- approx(x = IndDays, y = LAIvalues,method = "linear", xout = 1:maxdoy)$y
  }

  if (method == "linear") {
    IndDays <- unique(c(1,lai.doy,maxdoy))
    LAIvalues <- lai.frac * maxlai
    LAI <- approx(x = IndDays, y = LAIvalues, method = "linear", rule = 2, xout = 1:maxdoy)$y
  }

  if (method == "Coupmodel") {

    IndDays <- c(1,budburst.doy,shape.optdoy,leaffall.doy+leaffall.dur,maxdoy)
    LAIvalues <- c(minlai,minlai,maxlai,minlai,minlai)
    #IndDays <- c(1,budburst.doy,opt.doy,leaffall.doy + leaffall.dur,maxdoy)
    Forms <- c(1,shape.budburst, shape.leaffall,1,1)

    doy <- 1:maxdoy

    Index <- rep(1,maxdoy)
    Index <- ifelse( (doy >= budburst.doy & doy <= leaffall.doy+leaffall.dur), 2, Index)
    Index <- ifelse( doy >= shape.optdoy & Index == 2, 3, Index)
    Index <- ifelse( doy > leaffall.doy+leaffall.dur & Index == 1, 4, Index)


    alpha <-  sin((doy - IndDays[Index] + 1) /  (IndDays[Index + 1] + 1 - IndDays[Index] + 1 ) * pi / 2) ^ (Forms[Index])
    LAI <- (1 - alpha)*LAIvalues[Index] + alpha * LAIvalues[Index + 1]

  }

  return(LAI)

}




