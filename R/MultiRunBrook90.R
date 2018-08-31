#' Make a multirun simulation using a set of variable input parameters.
#'
#' @param nRuns Number of runs
#' @param param_fixed Named list of constant model input parameters to use in all single runs.
#' @param param_var Named list or data.frame of variable input parameters with realisations for each single run.
#' @param multirun.dir The directory where to write temporary files of single runs
#' @param singlerun_names character vector with the names of the single runs.
#' @param cores Number of cores to use for parallel processing
#' @param ... Arguments passed to \code{\link[Run.B90]{Run.B90}}.
#' At least inicontrol, climate, and soil have to be provided!
#'
#' @return a named list with results for the single runs. Simulation errors are passed on.
#' @export
#'
#' @examples
MultiRun.B90 <- function(nRuns,
                         param_fixed,
                         param_var,
                         multirun.dir = "MultiRuns",
                         keep.outfiles = FALSE,
                         singlerun_names = paste0("RunNo",1:nRuns),
                         cores = 3,
                         showProgress = TRUE,
                         ...){

  names(param_var) <- tolower(names(param_var))

  if (!all(c("inicontrol", "climate", "soil") %in% names(list(...)))) {
    stop("Please provide at least the inicontrol, climate, and soil arguments required by Run.B90!")
  }

  if (!all(names(param_var) %in% names(param_fixed))) {
    warning("Not all names of 'param_var' were found in 'param_fixed'!")
  }

  if (nrow(param_var) > nRuns) {
    warning("Number of rows in 'param_var' is greater than 'nRuns'.
            Only the first 'nRuns' rows of param_var will be used in the Multirun!")
  }
  if (nrow(param_var) < nRuns) {
    stop("The number of paramter sets is lower than 'nRun'.
         Please reduce 'nRuns' to the number of rows in 'param_var'!")
  }

  multirun.dir <- normalizePath(multirun.dir, mustWork = F)
  if (!dir.exists(multirun.dir)) {
    dir.create(multirun.dir)
  }

  oldwd <- getwd()
  setwd(multirun.dir)
  on.exit(setwd(oldwd))

  #TODO make some manipulation on the vary parms: should work as dataframe and also as list,
  # to be able to input parameters with length > 1!


  #set up Cluster and progressbar
  progress <- function(n) setTxtProgressBar(pb, nRuns)

  if (showProgress) {
    opts <- list(progress = progress)
  } else {
    opts <- list(progress = NULL)
  }

  cl <- makeSOCKcluster(cores)
  registerDoSNOW(cl)
  clusterEvalQ(cl, library("brook90r"))

  on.exit(stopCluster(cl), add = T)


  result <- foreach(i = seq_along(singlerun_names),
                    .final = function(x) setNames(x, singlerun_names),
                    .errorhandling = "pass",
                    .options.snow = ifelse(showProgress, opts, NULL)) %dopar% {

                      param_fixed[match(names(param_var),names(param_fixed))] <- param_var[i,]

                      res <- Run.B90(
                        directory = file.path(singlerun_names[i]),
                        param = param_fixed,
                        ...)

                      if (!keep.outfiles) {
                        unlink(file.path(multirun.dir, singlerun_names[i]), recursive = TRUE)
                      }

                      return(res)

                    }

  return(result)
}
