#' Create a daily sequence of stand properties (lai, sai, height, age)
#'
#' Uses yearly values of inter-annual vegetation development values (maxlai, sai, height, densef, age)
#' and interpolates them to a daily sequence.
#' properties
#'
#' @param stand.years A sequence of years or a single year, to which the stand properties were measured
#' @param sai Stem area index, either a fixed single value or corresponding to
#' stand.years.
#' @param height Vegetation height, either a fixed single value or corresponding
#' to stand.years. m
#' @param densef Relative vegetation density values, either a fixed single
#' value orcorresponding to stand.years.
#' to the beginning of stand.years, or corresponding to stand.years.
#' @param years.out years for which ouput is generated, maybe longer or shorter than
#' stand.years, but must include at least one of the values in stand.year.
#' @param approx.method name of method for interpolation, 'constant' or 'linear'.
#' The age variable is always linearly interpolated.
#'
#' @return Returns a data.frame with columns lai, sai, height,
#' densef at daily resolution.
#'
#' @details The yearly values of sai, height, densef and age are interpreted to be valid
#' at the 1st of January of the respective years. Interpolation is made with rule = 2,
#' so that for daily values outside the interpolation interval the value at the closest
#' data extreme is used. For \code{approx.method = 'linear'}, this results in a constant value
#' for all \code{years.out >= max(stand.years)} or \code{years.out < min(stand.years)}.
#'
#'@examples
stand_c <- MakeStand(stand.years = 2001:2003,
                     sai = c(0.6,0.5,0.45),
                     height = 20,
                     densef = 1,
                     age = 100:102,
                     approx.method = "constant")
plot(stand_c$sai)
#' @export
MakeStand <- function(stand.years,
                      sai,
                      height,
                      densef,
                      age,
                      years.out = stand.years,
                      approx.method){


  # data
  longterm.stand.dyn <- data.frame(year = stand.years, height, sai, densef, age)

  #if necessary prolongate standproperties to years.out (only happens if longtermdev-table is supplied)
  #beginning
  if (min(stand.years) > min(years.out)) {
    longterm.stand.dyn <- rbind(
      longterm.stand.dyn[rep(1,min(longterm.stand.dyn$year) - min(years.out)),],
      longterm.stand.dyn)
  }
  #end
  if (max(stand.years) < max(years.out)) {
    longterm.stand.dyn <- rbind(
      longterm.stand.dyn,
      longterm.stand.dyn[rep(nrow(longterm.stand.dyn),max(years.out) - max(longterm.stand.dyn$year)),])
  }

  if (length(unique(longterm.stand.dyn$year)) < length(years.out)) {
    #recalculate year and age (if period was prolonged)
    longterm.stand.dyn$year <- years.out
    longterm.stand.dyn$age <-  seq(from = min(age) - (min(stand.years) - min(years.out)),
                                   by = 1, length.out = nrow(longterm.stand.dyn))
  }

  longterm.stand.dyn$datum <- as.Date(paste(longterm.stand.dyn$year,"-01-01",sep = ""), "%Y-%m-%d")

  ########
  # interpolate and extrapolate stand characteristics

  # make output
  out <- data.frame(dates = seq.Date(from = as.Date(paste0(min(years.out),"-01-01")),
                                     to = as.Date(paste0(max(years.out),"-12-31")),
                                     by = "day"))

  out$height <- approx(longterm.stand.dyn$datum,
                       longterm.stand.dyn$height,
                       rule = 2, f = 0,
                       method = approx.method,
                       xout = out$dates)$y

  out$sai <-   approx(longterm.stand.dyn$datum,
                      longterm.stand.dyn$sai,
                      rule = 2, f = 0,
                      method = approx.method,
                      xout = out$dates)$y

  out$densef <- approx(longterm.stand.dyn$datum,
                       longterm.stand.dyn$densef,
                       rule = 2, f = 0,
                       method = approx.method,
                       xout = out$dates)$y

  out$age <- approx(longterm.stand.dyn$datum,
                    longterm.stand.dyn$age,
                    rule = 2, method = "linear",
                    xout = out$dates)$y


  return(out)
}
