#' Run the LWF-Brook90 hydrological model and return results
#'
#' Takes all necessary information needed to run the LWF-Brook90 hydrological model,
#' writes input files, starts the external executable via a system-call and returns
#' the results.
#' @param directory Directoryname of the project where the input and output files
#' are located. Will be created, if not existing.
#' @param inicontrol Named list of model control options. Use
#' \code{\link{MakeInicontrol.B90}} to generate a list with default model control options.
#' @param param Named list of model input parameters. Use
#' \code{\link{MakeParam.B90}} to generate a list with default model parameters.
#' @param climate Data.frame with daily climate data. All names(climate) should be
#' found in and corrrespond to arguments dates, tmax, tmin, wind, prec, vappres,
#' globrad, sunhours) of \code{\link{writeClimate.in}}.
#' @param soil Data.frame containing the hydraulic properties of the soil layers.
#' Each row represents one layer, containing the layer's boundaries
#' ['upper' and 'lower' negative downwards, m], van Genuchten retention parameters 'alpha' [m-1], 'n', 'ths',
#' 'thr', Mualem conductivity function parameters ksat [mm d-1] and t, and the volume fraction
#' of gravel content.
#' @param longtermdev Data.frame with yearly values of vegetation properties
#' (years, age, lai, sai, height, densef) that are passed to \code{\link{MakeStand}}.
#' @param outputmat A [10,5]-matrix marking the desired model-output. Use
#' \code{\link{Choose_output.B90}} to generate and edit it.
#' @param write.climate.in  Should the climate file be written? Ignored if no
#' Climate.in is found in directory, Climate.in is written anyway.
#' @param write.param.in Should the climate file be written? Ignored if
#' write.climate.in == TRUE and written anyway.
#' @param output.plant.devt Return daily values of aboveground plant properties?
#' @param output_log Logical or name where stdout from \code{\link[base]{system2}}-call
#' is sent. The default output_log = FALSE discards stdout and slightly reduces execution
#' time of LWF Brook90. output_log = "" sents all command-line model-output to the R
#' console, all other names will create a text file containing the model runtime output.
#' @param keepoutputfiles Keep the model output files after running and
#' returning output? Default is TRUE.
#' @param verbose Print messages to screen? Default is TRUE.
#' @param path_b90.exe Filename of the executable. The default setting looks for the
#' executable 'b90.exe' within 'directory'.
#' @param run.model Run the model or only create input files? Default is TRUE.
#'
#' @return Returns a list of data.frames (data.tables) containing the model results
#' and some additional informations including the execution time.
#'
#' @export
#'
#' @examples
#'
#' #Set up lists containing model control options and model parameters:
#'
#' param.b90 <- MakeParam.B90()
#' options.b90 <- MakeIniControl.B90()
#'
#' Set start and end Dates for the simulation
#'
#' options.b90$startdate <- as.Date("2000-01-01")
#' options.b90$enddate <- as.Date("2004-12-31")
#'
#' # Derive soil hydaulic properties from soil physical properties
#' # using pedotransferfunctions
#'
#' soil <- cbind(soil_slb1[-c(1:3),], hydpar_wessolek_mvg(soil_slb1$texture[-c(1:3)]))
#'
#' Run LWF-Brook90
#'
#' names(soil) <- tolower(names(soil))
#' names(soil)[c(14,15)] <- c("npar", "mpar")
#' b90.result <- Run.B90(directory = "example_run_b90",
#'                       inicontrol = options.b90,
#'                       param = param.b90,
#'                       climate = meteo_slb1,
#'                       soil = soil,
#'                       path_b90.exe = "H:/B90/MultiControl/b90.exe")


Run.B90 <- function(directory,
                    inicontrol,
                    param,
                    climate,
                    soil,
                    longtermdev,
                    outputmat = choose_output.B90(edit = FALSE),
                    write.climate.in = TRUE,
                    write.param.in = TRUE,
                    output.plant.devt = TRUE,
                    output_log = "",
                    keepoutputfiles = TRUE,
                    verbose = TRUE,
                    path_b90.exe = "b90.exe",
                    run.model = TRUE
){

  oldWD <- getwd()
  on.exit(setwd(oldWD))


  #name-checks
  names(inicontrol) <- tolower(names(inicontrol))
  names(param) <- tolower(names(param))
  names(soil) <- tolower(names(soil))
  names(climate) <- tolower(names(climate))

  inicontrol$fornetrad <- match.arg(inicontrol$fornetrad, choices = c("globrad","sunhour"))
  inicontrol$budburst <- match.arg(inicontrol$budburst, choices = c("fixed", "dynamic"))
  inicontrol$budburst.method <- match.arg(inicontrol$budburst.method,
                                          choices = c(c("Menzel","StdMeteo", "ETCCDI", "Ribes uva-crispa")))
  inicontrol$leaffall <- match.arg(inicontrol$leaffall, choices = c("fixed", "dynamic"))
  inicontrol$longtermdyn <- match.arg(inicontrol$longtermdyn, choices = c("constant", "table"))
  inicontrol$annuallaidyn <- match.arg(inicontrol$annuallaidyn, choices =c("b90", "linear", "Coupmodel"))
  # Input checks +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  if (missing(directory)) {
    stop("Missing argument: 'directory'")}
  if (missing(param)) {
    stop("Missing argument: 'param'")}
  if (missing(inicontrol)) {
    stop("Missing argument: 'inicontrol'")}
  if (missing(climate) & write.climate.in == T) {
    stop("Missing argument: 'climate'")}
  if (missing(longtermdev) & tolower(inicontrol$longtermdyn) == "table") {
    stop("Missing argument: 'longtermdev'")}
  if (missing(soil) & write.param.in == T) {
    stop("Missing argument: 'soil'")}
  if (!file.exists(path_b90.exe)) {
    stop("Invalid argument: Executable file '",path_b90.exe, "' doesn't exist! Check argument 'path_b90.exe'")}
  if (!inherits(inicontrol$startdate, "Date")) {
    stop("Invalid argument: 'inicontrol$startdate'")}
  if (!inherits(inicontrol$startdate, "Date")) {
    stop("Invalid argument: 'inicontrol$enddate'")}
  if (!(inicontrol$startdate < inicontrol$enddate)) {
    stop("Invalid arguments: 'startdate > enddate ")}
  if (run.model == F & write.climate.in == F & write.param.in == F) {
    stop("No writing of inputfiles and no simulation?? Won't do anything, alright.")
  }


  # create project-directory
  if (!dir.exists(directory)) {
    if (verbose == TRUE) {print("Creating project-directory...")}
    dir.create(directory)
  }
  setwd(directory)


  #Simulation period
  climyears <- as.integer(unique(format(climate$dates, "%Y")))
  simyears <- seq(from = as.integer(format(inicontrol$startdate,"%Y")),
                  to = as.integer(format(inicontrol$enddate,"%Y")),
                  by = 1)

  if (length(simyears[which(simyears %in% climyears)]) < length(simyears)) {
    if (length(simyears[which(simyears %in% climyears)]) == 0) {
      stop("Your climate data does not include the simulation period. Change startdate and enddate!")
    } else {
      warning("climate not covering simulation period completely, period was cut!")
    }
  }

  #input und output directories +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  inpath <- file.path(getwd(),"in")
  outpath <- file.path(getwd(),"out")

  if (file.exists(inpath) == FALSE) {
    dir.create(inpath)
  }
  if (file.exists(outpath) == FALSE) {
    dir.create(outpath)
  }

  #clear output and log
  try(file.remove(list.files(outpath, pattern = ".ASC", full.names = T)))
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Climate.in
  # check ob climate.in vorhanden und mit start und end kompatibel  :
  if (!write.climate.in & file.exists(file.path(inpath, "Climate.in"))) {

    climate.in <- read.table(file.path(inpath, "Climate.in"), sep = "\t", skip = 3, header = F)[2:16]
    names(climate.in) <- c("year","month","mday","globrad","tmax","tmin","vap","wind","prec","mesfl","densef","height","lai","sai","age")

    if (inicontrol$startdate != with(climate.in[1,], as.Date(paste(year, month, mday,sep = "-")))) {
      write.climate.in <- TRUE}

    if (inicontrol$enddate > with(climate.in[(nrow(climate.in)),], as.Date(paste(year, month, mday, sep = "-")))) {
      write.climate.in <- TRUE}

  } else {write.climate.in <- TRUE}

  #Number of Simulation days
  inicontrol$ndays <-   as.integer(difftime(inicontrol$enddate,inicontrol$startdate)) + 1

  #Vegetation-Period+++++++++++++++++++++++++++++++++
  if (inicontrol$budburst == "dynamic" | inicontrol$leaffall == "dynamic") {
    budburst_leaffall <- with(climate, vegperiod::vegperiod(dates = dates,
                                                            Tavg = tmean,
                                                            start.method = as.character(inicontrol$budburst.method),
                                                            species = as.character(inicontrol$species_dynbudburst),
                                                            end.method = as.character(inicontrol$leaffall.method),
                                                            est.prev = ifelse(length(climyears) <= 5, length(climyears) - 1, 5))
    )
    if (inicontrol$budburst == "fixed") { budburst_leaffall$start <- param$budburstdoy }
    if (inicontrol$leaffall == "fixed") { budburst_leaffall$end <- param$leaffalldoy }

    budburst_leaffall <- budburst_leaffall[which(budburst_leaffall$year %in% simyears),]

  } else {
    budburst_leaffall <- data.frame(year = simyears,
                                    start = rep(param$budburstdoy, length(simyears)),
                                    end = rep(param$leaffalldoy, length(simyears)))
  }

  #

  # Make Stand +++++++++++++++++++++++++++++++++++++++
  #
  if (write.climate.in == TRUE) {
    if (tolower(inicontrol$longtermdyn) == "table") {
      stand <- longtermdev
      if (verbose == T) {print("Creating long term stand dynamics from table 'longtermdev'...")}
    } else {
      stand <- data.frame(year = simyears[1], age = param$age, height = param$height,
                          maxlai = param$maxlai, sai = param$sai,densef = param$densef)
        if (verbose == T) {print("Creating constant stand properties from parameters...")}
    }

    stand <- MakeStand(stand.years = stand$year,
                       maxlai = stand$maxlai,
                       minlai = stand$maxlai*param$winlaifrac,
                       sai = stand$sai,
                       height = stand$height,
                       densef = stand$densef,
                       age = stand$age,
                       yearsout = simyears,
                       budburst.doy = budburst_leaffall$start,
                       leaffall.doy = budburst_leaffall$end,
                       emerge.dur = param$emergedur,  leaffall.dur = param$leaffalldur,
                       opt.doy = param$optdoy, shape.budburst = param$shapestart,
                       shape.leaffall = param$shapeend, method = inicontrol$annuallaidyn
    )

    stand <- stand[which(stand$dates >= inicontrol$startdate
                         & stand$dates <= inicontrol$enddate),]

    if (verbose == T) {
      print("Standproperties created succesfully")
    }
  }
  #write StandProperties & Climate
  if (write.climate.in) {
    if (tolower(inicontrol$fornetrad) == "globrad") {
      with(climate[which(climate$dates >= inicontrol$startdate & climate$dates <= inicontrol$enddate), ],
           writeClimate.in(dates = dates, tmax = tmax, tmin = tmin, vappres = vappres,
                           wind = wind, prec = prec, globrad = globrad, sunhours = NULL,
                           densef = stand$densef, height = stand$height,
                           lai = stand$lai, sai = stand$sai, age = stand$age,
                           latitude = inicontrol$coords_y, use.sunhours = F,
                           filename = file.path(getwd(),"in/Climate.in"),
                           richter.prec.corr = inicontrol$richter.corr., prec.int = inicontrol$prec.interval,
                           snow.ini = param$snowini, gwat.ini = param$gwatini)
      )
      if (verbose == T) {print("'Climate.in' created succesfully using global radiation")}
    } else {
      with(climate[which(climate$dates >= inicontrol$startdate & climate$dates <= inicontrol$enddate), ],
           writeClimate.in(dates = dates, tmax = tmax, tmin = tmin, vappres = vappres,
                           wind = wind, prec = prec, sunhours = sunhours, globrad = NULL,
                           densef = stand$densef, height = stand$height,
                           lai = stand$lai, sai = stand$sai, age = stand$age,
                           latitude = inicontrol$coords_y, use.sunhours = T,
                           filename = file.path(getwd(),"in/Climate.in"),
                           richter.prec.corr = inicontrol$richter.corr., prec.int = inicontrol$prec.interval,
                           snow.ini = param$snowini, gwat.ini = param$gwatini)
      )
      if (verbose == T) {print("'Climate.in' created succesfully using sunshine duration hours")}
    }
  }

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Wurzeln dran mergen, wenn nicht aus soil-data.frame
  if( inicontrol$rootmodel != "soilvar"){
  soil$relrootlength <- MakeRelRootDens(soil$lower * (-100), param$maxrootdepth,
                                        method = inicontrol$rootmodel, param = param$betaroot,
                                        humusroots = inicontrol$humusroots
  )}


  #  Create materials for writeparam.in aus Soil-Table
  ## TODO: materials and layers as list-elements in param

  dubl <- duplicated(soil[,c("ths","thr","alpha","npar","ksat","tort","gravel")])
  materials <- soil[!dubl,c("ths","thr","alpha","npar","ksat","tort","gravel")]
  materials$mat <- 1:nrow(materials)
  materials <- materials[,c("mat","ths","thr","alpha","npar","ksat","tort","gravel")] #Reihenfolge im Output
  #add material-identifier to soil
  seqalong <- 2:length(dubl)
  soil$mat[1] <- 1
  m = 1
  for (i in seqalong) {
    if (dubl[i] == FALSE) {
      m = m + 1
      soil$mat[i] <- m
    } else soil$mat[i] <- m
  }
  soil$thick <- soil$upper - soil$lower
  soil$midpoint <- soil$lower + soil$thick/2
  soil$thick <- soil$thick * 1000
  soil$layer <- 1:nrow(soil)
  soil$psiini <- param$psiini

  #####

  if (write.param.in || write.climate.in) {
    writeParam.in(b90ini = inicontrol,
                  parameters = param,
                  materials = materials,
                  soil = soil,
                  outmat = outputmat,
                  filename = file.path(inpath, "Param.in"))
  }
  if (verbose == T) {
    print("Param.in created succesfully!")
  }
  # run, if required ################################################################
  if (run.model == T) {

    if (verbose == T) {
      print("Running model..." )
    }
    start <- Sys.time()
    simres <- tryCatch( {
      system2(path_b90.exe,
              stdout = output_log,
              invisible = TRUE,
              wait = TRUE)
    }, warning = function(wrn){
      return(wrn)
    })

    simtime <- Sys.time() - start
    units(simtime) <- "secs"

    #check for errors :
    if (inherits(simres, "warning")) {
      print("Simulation Error, check input and log file!")
    } else {

      if (verbose == T) {
        print(paste("Simulation successful! Duration:", round(simtime,2), "seconds"))
        print("Reading output...")
      }

      simres <- readOutput.B90(outpath)
      simres$param <- param
      simres$inicontrol <- inicontrol
      simres$soil <- soil
      if (output.plant.devt == TRUE) {
        simres$plant.devt <- data.table(stand)
      }

      #remove output
      if (!keepoutputfiles) { try(file.remove(list.files(outpath, pattern = ".ASC", full.names = T))) }
      #remove log-file (only if simulation had no errors)
      if (output_log != "" & output_log != FALSE) { try(file.remove(output_log))}

      if (verbose == T) {
        print("Finished!")
      }
    }
    simres$finishing.time <- Sys.time()
    simres$sim_time <- simtime
    return(simres)
  }

}



