
#' Create daily seasonal plant dynamics sequence (e.g. leaf area index development)
#' for one year
#'
#' Creates the seasonal course of leaf area or other plant traits, based
#' on minimum lai, maximum lai, budburst and leaffall day of year (doy) and shape
#' parameters.
#' Sollte Vielleicht noch etwas genereller gehalten werden, nicht von LAI reden in den Parameters!
#'
#' @param maxdoy last day of year (leapyear: 366, else 365)
#' @param minlai minimum value during winter
#' @param maxlai maximum value during summer
#' @param budburst.doy day of year when growth begins
#' @param leaffall.doy day of year when growth cessates [applies when method = "b90"]
#' @param emerge.dur number of days until maximum value is reached [applies when method = "b90"]
#' @param leaffall.dur number of days until minimum value is reached
#' @param opt.doy day of year when optimum value is reached [applies when method = "Coupmodel"]
#' @param shape.budburst shape parameter for the growth phase [applies when method = "Coupmodel"]
#' @param shape.leaffall shape parameter growth cessation [applies when method = "Coupmodel"]
#' @param doy.values vector of integers [required when method = "linear"]
#' @param lai.values vector of values corresponding to doy.values [required when method = "linear"]
#' @param method name of method for generating hte sequence. Must be one of "b90", "linear", "Coupmodel"
#'
#' @return Returns a seasonal sequence of length maxdoy
#' @export
MakeSeasLAI <- function(maxdoy,
                        minlai, #LAI in Winter
                        maxlai, #LAI in Summer
                        budburst.doy = 121, #Budburst DOY
                        leaffall.doy = 279, # DOY when leaffall is finished
                        emerge.dur = 28, # Duration until maximum LAI is reached (linear)
                        leaffall.dur = 58, # Duration until Winter LAI is reached  (linear)
                        opt.doy=220, #MaxLAI DOY (Coupmodel)
                        shape.budburst=0.5, #Formparameter for leaf flush (Coupmodel)
                        shape.leaffall=10, #Formparameter for leaf Fall (Coupmodel)
                        doy.values = c(1,120,150,280,320,maxdoy),
                        lai.values = c(0,0,1,2,0,0),
                        method="b90") { # possible: "linear", "b90", or "Coupmodel"

  method <- match.arg(method, choices = c("b90", "linear", "Coupmodel"))

  if (method == "b90") {
    IndDays <- c(1,budburst.doy,budburst.doy + emerge.dur,leaffall.doy ,leaffall.doy+leaffall.dur,maxdoy)
    LAIvalues <- c(minlai,minlai,maxlai,maxlai,minlai,minlai)
    LAI <- approx(x = IndDays, y = LAIvalues,method = "linear", xout = 1:maxdoy)$y
  }

  if (method == "linear") {
    IndDays <- unique(c(1,doy.values,maxdoy))
    LAIvalues <- lai.values
    LAI <- approx(x = IndDays, y = LAIvalues,method = "linear", xout = 1:maxdoy)$y
  }

  if (method == "Coupmodel") {

    IndDays <- c(1,budburst.doy,opt.doy,leaffall.doy,maxdoy)
    LAIvalues <- c(minlai,minlai,maxlai,minlai,minlai)
    #IndDays <- c(1,budburst.doy,opt.doy,leaffall.doy + leaffall.dur,maxdoy)
    Forms <- c(1,shape.budburst, shape.leaffall,1,1)

    doy <- 1:maxdoy

    Index <- rep(1,maxdoy)
    Index <- ifelse( (doy >= budburst.doy & doy <= leaffall.doy), 2, Index)
    Index <- ifelse( doy >= opt.doy & Index == 2, 3, Index)
    Index <- ifelse( doy > leaffall.doy+leaffall.dur & Index == 1, 4, Index)


    alpha <-  sin((doy - IndDays[Index] + 1) /  (IndDays[Index + 1] + 1 - IndDays[Index] + 1 ) * pi / 2) ^ (Forms[Index])
    LAI <- (1 - alpha)*LAIvalues[Index] + alpha * LAIvalues[Index + 1]

  }

  return(LAI)

}




