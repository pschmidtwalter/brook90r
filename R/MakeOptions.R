#' Create a list of model control options
#'
#' @param ... named arguments to be included in return value
#' @return returns a list of default model options. Can be used as inicontrol-argument in \code{\link{Run.B90}}.
#' @details
#' \describe{
#'   \item{startdate}{Startdate of the simulation.}
#'   \item{enddate}{Enddate of the simulation.}
#'   \item{fornetrad}{Use global solar radiation (="globrad") or sunshine duration
#'   hours (="sunhour") for net radiation calculation?}
#'   \item{prec.interval}{Number of precipitation intervals (default is 1).
#'   If prec.interval > 1, a separate file ("in/PRFILE.DAT") has to be provided manually!}
#'   \item{richter.prec.corr}{ Correct Precipitation for wind and evaporation losses (not implented yet)}
#'   \item{budburst}{Calculate budburst day of year dynamically (="dynamic") or use fixed values (="fixed") defined in parameters (budburstdoy)?}
#'   \item{budburst.method}{Name of method for budburst calculation. If
#'   'constant' or 'fixed', budburst day of year from parameters is used.
#'   All other methods calculate budburst day of year dynamically from temperatures, and
#'   the method name is passed to the 'start.method'-argument of  \code{\link[vegperiod]{vegperiod}}.}
#'   \item{leaffall.method}{Name of method for leaffall calculation. If
#'   'constant' or 'fixed', beginning of leaffall (day of year) from parameters is used.
#'   All other methods calculate budburst day of year dynamically from temperatures, and
#'   the method name is passed to the 'end.method'-argument of  \code{\link[vegperiod]{vegperiod}}.}
#'   \item{standprop.input}{Name of input for longterm (interannual) plant development.
#'   'parameters': yearly values of stand properties height, sai, densef, lai come from
#'   parameters, 'table':  values come from a table, see 'longtermdev'-argument of \code{\link{Run.B90}.}}
#'   \item{standprop.interp}{Interpolation method for stand properties.
#'   'linear' or 'constant', see 'approx.method'-argument of \code{\link{MakeStand}}.}
#'   \item{lai.method}{Name of method method for generating daily plant development.
#'   Passed to 'method'-argument of \code{\link{MakeSeasLAI}}. }
#'   \item{imodel}{Name of hydraulic parameterization: "CH" for Clapp/Hornberger, "MvG" for Mualem/van Genuchten}
#'   \item{rootmodel}{Model name of the root length density depth distribution function. Any of the names accepted by \code{\link{MakeRelRootDens}} are allowed.
#'   Assign "soilvar", if the root length density should be taken from the soil-data.frame (colname relrootlength)     }
#'   \item{humusroots}{Shall roots be defined for humuslayer? If TRUE, the humus layers will have the same root length density as the uppermost mineral soil layer.}
#' }
#' @examples
#' # Default options
#' options.b90 <- MakeOptions.B90()
#' # Include specific options
#' options.b90_dynamic_phenology <- MakeOptions.B90(budburst = 'Menzel', leaffall ='vonWilpert')
#' @export
MakeOptions.B90 <- function(...) {
  ctrl <- list(startdate = as.Date("2001-1-1"),
               enddate = as.Date("2003-12-31"),
               fornetrad = "globrad", #"sunhour"
               prec.interval = 1,
               richter.prec.corr = FALSE,
               budburst.method = "fixed", #any of the names accepted by 'start.method'-argument of vegperiod::vegperiod()
               leaffall.method = "fixed", #any of the names accepted by 'start.method'-argument of vegperiod::vegperiod()
               standprop.input = "parameters", #table
               standprop.interp = "constant",
               lai.method = "b90", #any of the names accepted by 'method'-argument of MakeSeasLAI()
               imodel = "MvG", # the parameterization of rentention & conductivity function. CH = Clapp-Hornberger, MvG: Mualem van Genuchten
               rootmodel = "betamodel", #any of the names accepted by the MakeRoots()
               humusroots = FALSE #parameter of MakeRoots()
  )
  dots <- list(...)

  if (length(dots) > 0 ) {
    if (length(dots[which(names(dots) %in% names(ctrl))]) < length(dots)) {
      warning(paste("Not all arguments found in list! Check names:",
                    names(dots[which(!names(dots) %in% names(ctrl))])
      ))
    }
    ctrl[match(names(dots),names(ctrl))] <- dots
  }
  return(ctrl)

}
