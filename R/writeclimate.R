#' Writes the LWF-Brook90 input file Climate.in
#'
#' Creates the Climate.in file from daily climate data and vegetation properties,
#' providing some flexibility with regards to radiation (solar radiation or sunshine hours)
#' and precipitation correction
#'
#' @param dates date sequence with daily resolution.
#' @param tmax daily maximum air temperature, \eqn{°C}.
#' @param tmin daily minimum air temperature, \eqn{°C}.
#' @param vappres daily mean air vapor pressure, \eqn{kPa}.
#' @param wind daily mean wind speed, \eqn{m s^{-1}}.
#' @param prec daily precipitation, {mm d^{-1}}.
#' @param globrad solar radiation on the zero plane, \eqn{MJm^{-2}d^{-1}}
#' @param sunhours sunshine duration, hours per day
#' @param densef  canopy density multiplier between 0.05 and 1, dimensionless
#' @param height canopy height, m
#' @param lai projected leaf area index, \eqn{m^{2} m^{-2}}
#' @param sai projected stem area index, \eqn{m^{2} m^{-2}}
#' @param age stand age, years
#' @param latitude geographic latitude of the simulated site, decimal degrees
#' @param mesfl measured stream flow,  \eqn{mm d^{-1}}
#' @param filename filename for writing. Defaults to '~in/Climate.in
#' @param use.sunhours Use sunshine duration hours rather than global radiation?
#' @param richter.prec.corr Correct precipitation values for evaporation and wind losses?
#' @param prec.int number of precipitation intervals per day. Currently only 1 interval per day is supported.
#' @param snow.ini initial amount of snow on the ground, mm water equivalent
#' @param gwat.ini initial groundwater storage below soil layers, mm
#'
#' @return no return value
#' @export
writeClimate.in <- function(dates, tmax, tmin, vappres, wind, prec, globrad,
                            sunhours,densef, height, lai, sai,age,latitude,
                            mesfl=rep(0, length(dates)),
                            filename = "in/Climate.in", use.sunhours = F,
                            richter.prec.corr= F, prec.int = 1, snow.ini = 0, gwat.ini = 0
                            ) {

  if (missing(globrad) & missing(sunhours)) {
    stop("Supply global radiation or sunshine duration hours!")
  }

  if (use.sunhours == T) {
    dat <- data.frame(dates, tmax, tmin, vappres, wind, prec, sunhours,
                      mesfl, densef, height, lai, sai, age)
  } else {
    dat <- data.frame(dates, tmax, tmin, vappres, wind, prec, globrad,
                      mesfl, densef, height, lai, sai, age)
  }

  dat$year <- as.integer(format(dates, "%Y"))
  dat$month <- as.integer(format(dates, "%m"))
  dat$mday <- as.integer(format(dates, "%d"))
  dat$doy <- as.integer(format(dates, "%j"))

  #Richter korrektur
#   if(b90ini$richterpreccorr==TRUE){
#     clim$Precip <- with(clim, RichterKorrPrec(dates=datum,tavg=tmean,precip=prec,exp=b90ini$richterexposition) )
#   }

  #Globalstrahlung berechnen, wenn als Sonnenscheindauer angegeben
  if (use.sunhours == T) {
      #Calculate GlobalRadiation
      dat$globrad <- with(dat, CalcGlobRad(as.integer(doy), sunhours, as.integer(latitude)))
  }

  #File Header
  ClimStr <- c(
    "'** First year First DOY  Latitude  Snow[mm]  GWat[mm]     NPINT'",

    paste("     ", dat$year[1], dat$doy[1], round(latitude,2), snow.ini, gwat.ini, prec.int, sep = "       " )
  )

  #format Climate Table
  dat$spaces <- " " #better output format with write.table

  dat <- dat[,c("spaces","year","month", "mday","globrad","tmax","tmin","vappres","wind","prec","mesfl","densef","height", "lai", "sai", "age")]
  dat[,c("tmax","tmin","prec")] <- round(dat[,c("tmax","tmin","prec")],2)
  dat[,c("wind","height")] <- round(dat[,c("wind","height")],2)
  dat[,c("globrad","vappres","densef","lai","sai","age")] <- round(dat[,c("globrad","vappres","densef","lai","sai","age")],3)
  names(dat)[c(1,ncol(dat))] <- c("'**","age'")

  # write Climate And VegetationTable (CLIMATE.IN)
  climate.in <- file(filename)
  writeLines(ClimStr, climate.in)
  close(climate.in)

  suppressWarnings(write.table(as.matrix(dat),row.names = F,col.names = T, quote = F, sep = "\t",file = filename, append = TRUE))


}


