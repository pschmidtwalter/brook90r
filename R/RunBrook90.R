#' Run the LWF-Brook90 hydrological model and return results
#'
#' Takes all necessary information needed to run the LWF-Brook90 hydrological model,
#' writes input files, starts the external executable via a system-call and returns
#' the results.
#' @param project.dir directory-name of the project where the input and output files
#' are located. Will be created, if not existing.
#' @param options.b90 named list of model control options. Use
#' \code{\link{MakeOptions.B90}} to generate a list with default model control options.
#' @param param.b90 Named list of model input parameters. Use
#' \code{\link{MakeParam.B90}} to generate a list with default model parameters.
#' @param climate data.frame with daily climate data. The names of climate have to
#' correspond to arguments \emph{dates}, \emph{tmax}, \emph{tmin}, \emph{wind}, \emph{prec}, \emph{vappres},
#' \emph{globrad}, \emph{sunhours}) of \code{\link{writeClimate.in}}.
#' @param soil data.frame containing the hydraulic properties of the soil layers.
#' Each row represents one layer, containing the layers' boundaries and soil hydraulic parameters.
#' The columns names for the upper and lower layer boundaries are \emph{upper} and \emph{lower} (m, negative downwards),
#' the parameters of the van Genuchten retention functions are \emph{ths}, \emph{thr},
#'  \emph{alpha} [m-1], \emph{npar}, and the parameters of the Mualem conductivity function
#'  \emph{ksat} [mm d-1] and \emph{tort}. The volume fraction of stones has to be named \emph{gravel}.
#' @param outputmat a [10,5]-matrix flagging the desired model-output. Use
#' \code{\link{choose_output.B90}} to generate and edit default output matrix..
#' @param output.log Logical or filename where 'stdout' of \code{\link[base]{system2}}-call
#' is sent. The default \emph{output.log} = "" sents all command-line model-output to
#' the R console, \emph{output.log} = FALSE discards stdout.
#' All other names will create a text file containing the model runtime output.
#' @param out.dir path where to write the output-files.
#' @param path_b90.exe filename of the executable code. The default setting looks for the
#' executable 'b90.exe' within 'project.dir'.
#' @param output.param.options append 'param.b90', 'options.b90', 'soil' and daily plant
#' properties ('plant.devt', as derived from parameters and written to 'climate.in') to the result?
#' @param keep.log.on.success keep the file 'output.log' after a successful simulation?
#' In case of simulation errors the 'output.log' file (if specified) is kept anyway for inspection purposes.
#' @param keep.outputfiles keep the model .asc output files after running and
#' returning the output?
#' @param verbose print messages to the console? Default is TRUE.
#' @param write.climate.in  should the climate file be written or not to save execution time?
#' Ignored if no 'Climate.in' is found in 'project.dir/in'.
#' @param run.model run the executable or only create input files? Default is TRUE.
#'
#' @return Returns the model-output from the files found in 'out.dir' as a list of data.frames (data.tables),
#' along with the execution time of the simulation, and input-parameters and options if desired.
#' @export
#'
#' @examples
#'
#' #Set up lists containing model control options and model parameters:
#'
#' param.b90 <- MakeParam.B90()
#' options.b90 <- MakeOptions.B90()
#'
#' Set start and end Dates for the simulation
#'
#' options.b90$startdate <- as.Date("2000-01-01")
#' options.b90$enddate <- as.Date("2004-12-31")
#'
#' # Derive soil hydraulic properties from soil physical properties
#' # using pedotransfer functions
#'
#' soil <- cbind(soil_slb1, hydpar_wessolek_mvg(soil_slb1$texture))
#'
#' Run LWF-Brook90
#' b90.result <- Run.B90(project.dir = "example_run_b90",
#'                       options.b90 = options.b90,
#'                       param.b90 = param.b90.b90,
#'                       climate = meteo_slb1,
#'                       soil = soil,
#'                       path_b90.exe = "b90.exe")

Run.B90 <- function(project.dir,
                    options.b90,
                    param.b90,
                    climate,
                    soil,
                    outputmat = choose_output.B90(edit = FALSE),
                    output.log = "",
                    out.dir = "out/",
                    path_b90.exe = "b90.exe",
                    output.param.options = TRUE,
                    keep.log.on.success = TRUE,
                    keep.outputfiles = TRUE,
                    verbose = TRUE,
                    write.climate.in = TRUE,
                    run.model = TRUE
){

  oldWD <- getwd()
  on.exit(setwd(oldWD))

  #name-checks ----------------------------------------------------------------------
  names(options.b90) <- tolower(names(options.b90))
  names(param.b90) <- tolower(names(param.b90))
  names(soil) <- tolower(names(soil))
  names(climate) <- tolower(names(climate))

  options.b90$fornetrad <- match.arg(options.b90$fornetrad, choices = c("globrad","sunhour"))

  options.b90$budburst.method <- match.arg(options.b90$budburst.method,
                                           choices = c("fixed", "constant","Menzel","StdMeteo", "ETCCDI", "Ribes uva-crispa"))

  options.b90$leaffall.method <- match.arg(options.b90$leaffall.method,
                                           choices = c("fixed", "constant","vonWilpert", "LWF-BROOK90", "NuskeAlbert", "StdMeteo","ETCCDI"))

  options.b90$standprop.input <- match.arg(options.b90$standprop.input, choices = c("parameters", "table"))

  options.b90$lai.method <- match.arg(options.b90$lai.method, choices = c("b90", "linear", "Coupmodel"))

  # Check suggested packages --------------------------------------------------------
  if (!options.b90$budburst.method %in% c("constant", "fixed") || !options.b90$leaffall.method %in% c("constant", "fixed")) {
    if (!requireNamespace("vegperiod", quietly = TRUE)) {
      stop("In 'options.b90' you chose dynamic buburst or leaf fall, for which the
           package \"vegperiod\" is required. Please install it:
           install.packages('vegperiod', repos='https://www.nw-fva.de/r-pkgs')")
    }
  }

  # Input checks --------------------------------------------------------------------
  if (missing(project.dir)) {
    stop("Missing argument: 'project.dir'")}
  if (missing(param.b90)) {
    stop("Missing argument: 'param.b90'")}
  if (missing(options.b90)) {
    stop("Missing argument: 'options.b90'")}
  if (missing(climate) & write.climate.in == T) {
    stop("Missing argument: 'climate'")}
  if (missing(soil)) {
    stop("Missing argument: 'soil'")}
  if (!file.exists(file.path(path_b90.exe))) {
    stop("Invalid argument: Executable file '",normalizePath(path_b90.exe), "' doesn't exist! Check argument 'path_b90.exe'")}
  if (!inherits(options.b90$startdate, "Date")) {
    stop("Invalid argument: 'options.b90$startdate'")}
  if (!inherits(options.b90$startdate, "Date")) {
    stop("Invalid argument: 'options.b90$enddate'")}
  if (!(options.b90$startdate < options.b90$enddate)) {
    stop("Invalid arguments: 'startdate > enddate ")}
  if (run.model == F & write.climate.in == F) {
    stop("No writing of inputfiles and no simulation? Won't do anything, alright.")
  }

  # clean file paths and set up directories -----------------------------------------

  path_b90.exe <- normalizePath(path_b90.exe, mustWork = TRUE)
  project.dir <- normalizePath(project.dir, mustWork = FALSE)

  # create project-directory
  if (!dir.exists(project.dir)) {
    if (verbose == TRUE) {print("Creating project-directory...")}
    tryCatch( {
      dir.create(project.dir)
    }, warning = function(wrn){
      stop(paste0("The specified  project directory (",
                  project.dir,
                  ") could not be created.)"))
    },
    error = function(err){
      return(err)
    })
  }

  setwd(project.dir) # set the working directory to the project folder

  # input directory: always "project.dir/in/
  in.dir <- normalizePath(file.path(getwd(),"in"), mustWork = FALSE)
  if (!dir.exists(in.dir)) {
    dir.create(in.dir)
  }

  # output-directory: variable! But, in Param.in needs to be shorter than 80 characters!
  options.b90$out.dir <- normalizePath(out.dir, mustWork = FALSE)

  # if normalized output-name too long, and not inside 'project.dir' make out.dir within 'project.dir':
  if (nchar(options.b90$out.dir) > 80 &
      options.b90$out.dir != normalizePath(file.path(getwd(), basename(options.b90$out.dir)), mustWork = F) ) {
    warning(paste0("The specified output directory (",options.b90$out.dir,") is too long
                    and could not be read by b90.exe. Find the results in ",basename(options.b90$out.dir),
                   " within the project directory instead!"))
    options.b90$out.dir <- basename(options.b90$out.dir)
  }

  #Create output directory:
  if (!dir.exists(options.b90$out.dir) ) {
    tryCatch( {
      dir.create(options.b90$out.dir)
    }, warning = function(wrn){
      options.b90$out.dir <- basename(options.b90$out.dir)
      dir.create(options.b90$out.dir)
      warning(paste0("The specified output directory (",options.b90$out.dir,") could
                     not be created. Find the results in ",basename(options.b90$out.dir),
                     " within the project directory instead!"))
    })
  }

  # file path for stdout in system2 call
  if (output.log == TRUE || (is.character(output.log) & nchar(output.log) > 0)) {
    output.log <- normalizePath(ifelse(output.log == TRUE, "b90.log", output.log),
                                mustWork = FALSE)
  } else {
    if (is.null(output.log)) {
      output.log = FALSE
    }
  }



  #clear output and log
  try(file.remove(list.files(options.b90$out.dir, pattern = ".ASC", full.names = T)))


  # Simulation period ----------------------------------------------------------------
  climyears <- as.integer(unique(format(climate$dates, "%Y")))
  simyears <- seq(from = as.integer(format(options.b90$startdate,"%Y")),
                  to = as.integer(format(options.b90$enddate,"%Y")),
                  by = 1)

  if (length(simyears[which(simyears %in% climyears)]) < length(simyears)) {
    if (length(simyears[which(simyears %in% climyears)]) == 0) {
      stop("Your climate data does not include the simulation period. Change startdate and enddate!")
    } else {
      warning("climate not covering simulation period completely, period was cut!")
    }
  }


  #Climate.in -----------------------------------------------------------------------
  # check ob climate.in vorhanden und mit start und end kompatibel  :
  if (!write.climate.in & file.exists(file.path(in.dir, "Climate.in"))) {

    climate.in <- read.table(file.path(in.dir, "Climate.in"), sep = "\t", skip = 3, header = F)[2:16]
    names(climate.in) <- c("year","month","mday","globrad","tmax","tmin","vap","wind","prec","mesfl","densef","height","lai","sai","age")

    if (options.b90$startdate != with(climate.in[1,], as.Date(paste(year, month, mday,sep = "-")))) {
      write.climate.in <- TRUE}

    if (options.b90$enddate > with(climate.in[(nrow(climate.in)),], as.Date(paste(year, month, mday, sep = "-")))) {
      write.climate.in <- TRUE}

  } else {write.climate.in <- TRUE}

  #Number of Simulation days
  options.b90$ndays <-   as.integer(difftime(options.b90$enddate,options.b90$startdate)) + 1

  # ---- Vegetation-Period  ----------------------------------------------------------
  # check length of fixed leaffall
  if (options.b90$leaffall.method %in% c("constant", "fixed")) {
    if (length(param.b90$leaffalldoy) > 1 & length(param.b90$leaffalldoy) != length(simyears)) {
      stop("When options.b90$leaffall.method == 'fixed', either provide a single value
                for param.b90$leaffall or a sequence of values, one for each year of the simulation period.")
    }
  }
  #check length of fixed budburst
  if (options.b90$budburst.method %in% c("constant", "fixed") ) {
    if (length(param.b90$budburstdoy) > 1 & length(param.b90$budburstdoy) != length(simyears)) {
      stop("When options.b90$budburst.method == 'fixed', either provide a single value
         for param.b90$budburstdoy or a sequence of values, one for each year of the simulation period.")
    }
  }

  # Extend budburst
  if (length(param.b90$budburstdoy) == 1) {
    param.b90$budburstdoy <- rep(param.b90$budburstdoy,times = length(simyears))
  }
  if (length(param.b90$leaffalldoy) == 1) {
    param.b90$leaffalldoy <- rep(param.b90$leaffalldoy,times = length(simyears))
  }

  # start and end dynamic
  if (!options.b90$budburst.method %in% c("constant", "fixed") &
      !options.b90$leaffall.method %in% c("constant", "fixed")) {
    budburst_leaffall <- with(climate, vegperiod::vegperiod(dates = dates,
                                                            Tavg = tmean,
                                                            start.method = as.character(options.b90$budburst.method),
                                                            species = as.character(param.b90$budburst.species),
                                                            end.method = as.character(options.b90$leaffall.method),
                                                            est.prev = ifelse(length(climyears) <= 5, length(climyears) - 1, 5))
    )
    budburst_leaffall <- budburst_leaffall[which(budburst_leaffall$year %in% simyears),]
    param.b90$budburstdoy <- budburst_leaffall$start
    param.b90$leaffalldoy <- budburst_leaffall$end
  } else {
    # only budburst is dynamic:
    if (!options.b90$budburst.method %in% c("constant", "fixed") & options.b90$leaffall.method %in% c("constant", "fixed"))   {
      budburst_leaffall <- with(climate,
                                vegperiod::vegperiod(dates = dates,
                                                     Tavg = tmean,
                                                     start.method = as.character(options.b90$budburst.method),
                                                     species = as.character(param.b90$budburst.species),
                                                     end.method = "StdMeteo",
                                                     est.prev = ifelse(length(climyears) <= 5, length(climyears) - 1, 5))
      )
      param.b90$budburstdoy <- budburst_leaffall$start[which(budburst_leaffall$year %in% simyears)]
    } else {
      # only end dynamic
      if (options.b90$budburst.method %in% c("constant", "fixed") & !options.b90$leaffall.method %in% c("constant", "fixed"))   {
        budburst_leaffall <- with(climate,
                                  vegperiod::vegperiod(dates = dates,
                                                       Tavg = tmean,
                                                       start.method = "StdMeteo",
                                                       end.method = options.b90$leaffall.method))
        options.b90$leaffalldoy <- budburst_leaffall$end[which(budburst_leaffall$year %in% simyears)]
      }
    }
  }


  # ---- Make Stand --------------------------------------------------------------------
  if (write.climate.in == TRUE) {
    if (tolower(options.b90$standprop.input) == "table") {
      if (verbose == T) {print("Creating long term stand dynamics from table 'standprop.table'...")}
      if (is.null(param.b90$standprop.table) & tolower(options.b90$standprop.input) == "table") {
        stop("Missing parameter: 'standprop.table', required if options.b90$standprop.input = 'table'")}

      standprop_yearly <- data.table(param.b90$standprop.table)
      # interpolate lai with rule = 2 to extend lai from table to simyears
      param.b90$maxlai <- with(param.b90$standprop.table,
                               approx(x = as.Date(paste0(year,"-01-01")),
                                      y = maxlai,
                                      xout = as.Date(paste0(simyears,"-01-01")),
                                      method = 'constant', rule = 2))$y
    } else { # standproperties from parameters
      if (verbose == T) {print("Creating constant stand properties from parameters...")}

      # yearly variation of parameters
      if (any(with(param.b90, length(sai) > 1, length(densef) > 1, length(height) > 1))) {
        standprop_yearly <- data.frame(year = c(simyears, max(simyears) + 1),
                                       age = seq(from = param.b90$age.ini, by = 1,
                                                 length.out = length(simyears) + 1),
                                       height = c(param.b90$height, param.b90$height.end),
                                       sai = c(param.b90$sai, param.b90$sai.end),
                                       densef = c(param.b90$densef, param.b90$densef.end))
        if (length(param.b90$height) == 1) { standprop_yearly$height <- param.b90$height}
        if (length(param.b90$sai) == 1) {standprop_yearly$sai <- param.b90$sai}
        if (length(param.b90$densef) == 1) {standprop_yearly$densef <- param.b90$densef}

      } else { # same properties every year
        standprop_yearly <- data.frame(year = simyears,
                                       age = seq(from = param.b90$age.ini, by = 1,
                                                 length.out = length(simyears)),
                                       height = param.b90$height,
                                       sai = param.b90$sai,
                                       densef = param.b90$densef)
      }
    }

    standprop_daily <- with(standprop_yearly,
                            data.table(MakeStand(stand.years = year,
                                                 sai = sai, height = height, densef = densef,
                                                 age = age,
                                                 years.out = simyears,
                                                 approx.method = options.b90$standprop.interp)))
    # constrain to simulation period
    standprop_daily <- standprop_daily[which(dates >= options.b90$startdate
                                             & dates <= options.b90$enddate),]

    # daily leaf area index: make data.table to create leaf area index by year.

    laidaily <- data.table(lai = MakeSeasLAI(simyears,
                                             method = options.b90$lai.method,
                                             maxlai = param.b90$maxlai,
                                             winlaifrac = param.b90$winlaifrac,
                                             budburst.doy = param.b90$budburstdoy,
                                             leaffall.doy = param.b90$leaffalldoy,
                                             emerge.dur = param.b90$emergedur,
                                             leaffall.dur = param.b90$leaffalldur,
                                             shape.budburst = param.b90$shape.budburst,
                                             shape.leaffall = param.b90$shape.leaffall,
                                             shape.optdoy = param.b90$shape.optdoy,
                                             lai.doy = param.b90$lai.doy,
                                             lai.frac = param.b90$lai.frac))

    # constrain to simulation period
    laidaily[,dates := seq.Date(as.Date(paste0(min(simyears),"-01-01")),
                                as.Date(paste0(max(simyears),"-12-31")), by = "day")]
    laidaily <- laidaily[which(dates >= options.b90$startdate
                               & dates <= options.b90$enddate),]

    if (verbose == T) {
      print("Standproperties created succesfully")
    }
  }
  #write StandProperties & Climate ----------------------------------------------------
  if (write.climate.in) {
    if (tolower(options.b90$fornetrad) == "globrad") {
      with(climate[which(climate$dates >= options.b90$startdate & climate$dates <= options.b90$enddate), ],
           writeClimate.in(dates = dates, tmax = tmax, tmin = tmin, vappres = vappres,
                           wind = wind, prec = prec, globrad = globrad, sunhours = NULL,
                           densef = standprop_daily$densef, height = standprop_daily$height,
                           lai = laidaily$lai, sai = standprop_daily$sai, age = standprop_daily$age,
                           latitude = param.b90$coords_y, use.sunhours = F,
                           filename = file.path(getwd(),"in/Climate.in"),
                           richter.prec.corr = options.b90$richter.prec.corr, prec.int = options.b90$prec.interval,
                           snow.ini = param.b90$snowini, gwat.ini = param.b90$gwatini)
      )
      if (verbose == T) {print("'Climate.in' created succesfully using global radiation")}
    } else {
      with(climate[which(climate$dates >= options.b90$startdate & climate$dates <= options.b90$enddate), ],
           writeClimate.in(dates = dates, tmax = tmax, tmin = tmin, vappres = vappres,
                           wind = wind, prec = prec, sunhours = sunhours, globrad = NULL,
                           densef = standprop_daily$densef, height = standprop_daily$height,
                           lai = laidaily$lai, sai = standprop_daily$sai, age = standprop_daily$age,
                           latitude = param.b90$coords_y, use.sunhours = T,
                           filename = file.path(getwd(),"in/Climate.in"),
                           richter.prec.corr = options.b90$richter.prec.corr, prec.int = options.b90$prec.interval,
                           snow.ini = param.b90$snowini, gwat.ini = param.b90$gwatini)
      )
      if (verbose == T) { print("'Climate.in' created succesfully using sunshine duration hours")}
    }
  }


  # Make Roots ----------------------------------------------------------------------
  if (options.b90$root.method != "soilvar") {
    soil$rootden <- MakeRelRootDens(soil$lower * (-100),
                                          param.b90$maxrootdepth * (-100),
                                          method = options.b90$rootmodel,
                                          beta = param.b90$betaroot,
                                          humusroots = options.b90$humusroots
    )}

  #  Make soil ----------------------------------------------------------------------
  #  Create materials for writeparam.in from soil
  ## TODO: materials and layers as list-elements in param.b90?

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
  soil$psiini <- param.b90$psiini

  # write Param.in ------------------------------------------------------------------
  writeParam.in(b90opts = options.b90,
                parameters = param.b90,
                materials = materials,
                soil = soil,
                outmat = outputmat,
                filename = file.path(in.dir, "Param.in"))

  if (verbose == TRUE) {
    print("'Param.in' created succesfully!")
  }

  # Execute b90.exe  ----------------------------------------------------------------
  if (run.model == T) {

    if (verbose == T) {
      print("Running model..." )
    }
    start <- Sys.time()

    cmdline <- tryCatch( {
      if (tolower(Sys.info()[["sysname"]]) == "windows") {
        system2(command = path_b90.exe,
                stdout = output.log,
                invisible = TRUE,
                wait = TRUE
        )
      } else {
        system2(command = path_b90.exe,
                stdout = output.log,
                wait = TRUE)
      }
    }, warning = function(wrn){
      return(wrn)
    },
    error = function(err){
      return(err)
    })

    simtime <- Sys.time() - start
    units(simtime) <- "secs"

    # Check for Errors --------------------------------------------------------------

    # Unix uses shell for system2 and which not returning errors, so we create one in case simres != 0
    if (is.integer(cmdline)) {
      if (cmdline != 0) {
        stop(paste0("Execution of b90.exe via shell gave an error: ", cmdline, ", check input and 'output.log'!"))
      }
    }

    # check for warnings (system2 does not return errors if command fails)
    if (inherits(cmdline, "warning")) {
      print("Simulation Error, check input and log file!")
      stop(print(cmdline))

    } else {

      if (inherits(cmdline, "error")) {
        stop(print(cmdline))
      }

      if (verbose == T) {
        print(paste("Simulation successful! Duration:", round(simtime,2), "seconds"))
        print("Reading output...")
      }

      # Read output files -----------------------------------------------------------
      simres <- readOutput.B90(options.b90$out.dir)

      # append input parameters
      if (output.param.options == TRUE) {
        simres$plant.devt <- data.table(standprop_daily)
        simres$plant.devt$lai <- laidaily$lai
        simres$param.b90 <- param.b90
        simres$options.b90 <- options.b90
        simres$soil <- soil
      }

      #remove output
      if (!keep.outputfiles) { try(file.remove(list.files(options.b90$out.dir, pattern = ".ASC", full.names = T))) }
      #remove log-file (only if simulation had no errors)
      if (output.log != "" & output.log != FALSE & keep.log.on.success == F) {
        try(file.remove(output.log))
      }

      if (verbose == T) {
        print("Finished!")
      }
      simres$finishing.time <- Sys.time()
      simres$sim.time <- simtime
      return(simres)
    }
  }
}

