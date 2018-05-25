#' Create a list of model control options
#'
#' @param ... named arguments to be included in return value
#' @return returns a list of default model options. Can be used as inicontrol-argument in \code{\link[Run.B90]{Run.B90}}.
#' @details
#' \describe{
#'   \item{startdate}{Startdate of the simulation.}
#'   \item{enddate}{Enddate of the simulation.}
#'   \item{fornetrad}{Use global solar radiation (="globrad") or sunshine duration hours (="sunhour") for net radiation calculation?}
#'   \item{precinterval}{Number of precipitation intervals (default is 1). If prec.interval > 1, a separate file ("in/PRFILE.DAT") has to be provided manually!}
#'   \item{richter.corr.}{ Correct Precipitation for wind and evaporation losses (not implented yet)}
#'   \item{coords_x}{Longitude value (decimal degrees) of the simulation location (has no effect on simulation results).}
#'   \item{coords_y}{Latitude value (decimal degrees) of the simulation location.}
#'   \item{budburst}{Calculate budburst day of year dynamically (="dynamic") or use fixed values (="fixed") defined in parameters (budburstdoy)?}
#'   \item{budburst.method}{Name of method for dynamic budburst calculation. Passed to 'start.method'-argument of  \code{\link[vegperiod]{vegperiod}}.}
#'   \item{leaffall}{ Calculate leaffall day of year dynamically (="dynamic") or use fixed values (="fixed") defined in parameters (leaffalldoy)?}
#'   \item{species_dynbudburst}{name of tree species [required if budburst.method='Menzel']. Passed to 'species'-argument of \code{\link[vegperiod]{vegperiod}} .}
#'   \item{longtermdyn}{Name of method for longterm plant development: either constant ("const") or yearly changing values ("table") of plant development.}
#'   \item{annuallaidyn}{Name of method method for generating seasonal plant development. Passed to 'method'-argument of \code{\link{MakeSeasLAI}}. }
#'   \item{imodel}{Name of hydraulic parameterization: "CH" for Clapp/Hornberger, "MvG" for Mualem/van Genuchten}
#'   \item{rootmodel}{Model name of the root length density depth distribution function. Any of the names accepted by \code{\link{MakeRelRootDens}} are allowed.
#'   Assign "soilvar", if the root length density should be taken from the soil-data.frame (colname relrootlength)     }
#'   \item{humusroots}{Shall roots be defined for humuslayer? If TRUE, the humus layers will have the same root length density as the uppermost mineral soil layer.}
#' }
#' @examples
#' # Default options
#' options.b90 <- MakeIniControl.B90()
#' # Include specific options
#' options.B90_fixed_phenology <- MakeIniControl.B90(budburst = 'fixed', leaffall ='fixed')
#' @export
MakeIniControl.B90 <- function(...) {
  ctrl <- list(startdate = as.Date("2001-1-1"),
               enddate = as.Date("2003-12-31"),
               fornetrad = "globrad", #"sunhour"
               prec.interval = 1,
               richter.corr. = FALSE,
               coords_x = as.numeric(9.9095),
               coords_y = as.numeric(51.544),
               budburst = "dynamic", #fixed
               budburst.method = "Menzel", #any of the names accepted by 'start.method'-argument of vegperiod::vegperiod()
               leaffall = "dynamic", #fixed
               leaffall.method = "vonWilpert", #any of the names accepted by 'start.method'-argument of vegperiod::vegperiod()
               species_dynbudburst = "Fagus sylvatica", # any of the names accepted by 'species'-argument in vegperiod::vegperiod()
               longtermdyn = "const", #table
               annuallaidyn = "b90", #any of the names accepted by 'method'-argument of MakeSeasLAI()
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
