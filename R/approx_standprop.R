#' Create a daily sequence from yearly values using interpolation
#'
#' Uses yearly values of inter-annual vegetation development values (maxlai, sai, height, densef, age)
#' and interpolates them to a daily sequence.
#'
#' @param x.years A sequence of years or a single year
#' @param y vector of the same length as x.years
#' @param xout.years years for which ouput is generated, maybe longer or shorter than
#' x.years
#' @param approx.method name of interpolation method 'constant' or 'linear'.
#'
#' @return A vector of daily values corresponding to the dates in years.out
#'
#' @details The y values are interpreted to be valid at the 1st of January of the respective x.years.
#' Interpolation is made with rule = 2, so that for daily values outside the interpolation interval
#' the value at the closest data extreme is used. For \code{approx.method = 'linear'}, this results in
#' constant values for all dates within \code{xout.years >= max(x.years)} or \code{xout.years < min(x.years)}.
#' If a single value is passed via y, then no interpolation is made and the value is simply repeated with
#' length of Dates within xout.years.
#'
#'@examples
#' @export
approx_standprop <- function(x.years,
                             y,
                             xout.years = x.years,
                             approx.method){


  # data
  x.dates <- as.Date(paste(x.years,"-01-01",sep = ""), "%Y-%m-%d")
  xout.dates <- seq.Date(from = as.Date(paste0(min(xout.years),"-01-01")),
                         to = as.Date(paste0(max(xout.years),"-12-31")),
                         by = "day")
  if (length(y) == 1) {
    yout <- rep(y, times = length(xout.dates))

  } else{
    yout <- approx(x.dates, y, xout = xout.dates, rule = 2, f = 0, method = approx.method)$y
  }

  return(yout)

}
