

#' Read LWF-Brook90 output files
#'
#' Reads all files in LWF-Brook90 output directory and returns them as a list of data.frames
#'
#' @param outpath directory with LWF-Brook90 outputfiles
#'
#' @return a named list containing the data from outputfiles. Each list item  corresponds
#' to one file.
#' @export
readOutput.B90 <- function(outpath){

  outfilenames <- c(list.files(outpath,pattern = ".ASC" ),
                    list.files(outpath,pattern = ".PRE" ))


  for (i in 1:length(outfilenames)) {

    # 'normal' reading
    if ( !grepl("SWAT",outfilenames[i]) &  !grepl("PSIT",outfilenames[i])
        & !outfilenames[i] %in% c("MISCMON.ASC", "BELO.PRE")
        & file.exists(file.path(outpath,outfilenames[i]))) {
      assign(tolower(outfilenames[i]),
             data.table::fread(file.path(outpath,outfilenames[i]),sep = ";",
                               header = T, showProgress = F)
      )

    }

    # 'special' reading: SWAT
    if (grepl("SWAT",outfilenames[i]) &
        file.exists(file.path(outpath,outfilenames[i]))) {
      assign(tolower(outfilenames[i]),
             data.table::fread(file.path(outpath,outfilenames[i]),sep = ";",
                               header = F, skip = 1,showProgress = F)
      )
      if (outfilenames[i] == "SWATDAY.ASC")
        data.table::setnames(get(tolower(outfilenames[i])),
                 paste("V",1:10,sep = ""),
                 c("YR",  "MO","DA","DOY","NL","SWATI", "THETA","WETNES","PSIMI","PSITI"))
      if (outfilenames[i] == "SWATMON.ASC")
        data.table::setnames(get(tolower(outfilenames[i])),
                 paste("V",1:8,sep = ""),
                 c("YR",  "MO","NL","SWATI", "THETA","WETNES","PSIMI","PSITI"))
      if (outfilenames[i] == "SWATANN.ASC")
        data.table::setnames(get(tolower(outfilenames[i])),
                 paste("V",1:7,sep = ""),
                 c("YR",  "NL","SWATI", "THETA","WETNES","PSIMI","PSITI"))
    }

    # 'special' reading: PSIT
    if (grepl("PSIT",outfilenames[i]) &
        file.exists(file.path(outpath,outfilenames[i]))) {
      assign(tolower(outfilenames[i]),
             data.table::fread(file.path(outpath,outfilenames[i]),sep = " ",
                               header = F, skip = 2,showProgress = F)
      )
      columN. <- ncol(get(tolower(outfilenames[i])))

      if (outfilenames[i] == "PSITDAY.ASC") {
        data.table::setnames(get(tolower(outfilenames[i])),
                 paste("V",1:columN.,sep = ""),
                 c("YR",  "MO","DA","DOY", paste0("PSIT",1:(columN. - 4))))}
      if (outfilenames[i] == "PSITMON.ASC") {
        data.table::setnames(get(tolower(outfilenames[i])),
                 paste("V",1:columN.,sep = ""),
                 c("YR",  "MO", paste0("PSIT",1:(columN. - 2))))}
      if (outfilenames[i] == "PSITANN.ASC") {
        data.table::setnames(get(tolower(outfilenames[i])),
                 paste("V",1:columN.,sep = ""),
                 c("YR", paste0("PSIT",1:(columN. - 1))))}

      if (outfilenames[i] == "PSIT.PRE") {
        data.table::setnames(get(tolower(outfilenames[i])),
                 paste("V",1:columN.,sep = ""),
                 c("YR","MO","DA","DOY","NP", paste0("PSIT",1:(columN. - 5))))}
      rm(columN.)
    }

    # 'special' reading: MISC, BELo
    if (outfilenames[i] == "MISCMON.ASC" &
        file.exists(file.path(outpath,"MISCMON.ASC"))) {
      assign(tolower(outfilenames[i]),
             data.table::data.table(read.table(file.path(outpath, "MISCMON.ASC"),
                                               skip = 1)))
      data.table::setnames(get(tolower(outfilenames[i])),
               paste("V",1:9,sep = ""),
               c("YR", "MO","VRFLN","SAFRAC", "STRES", "V6","V7","NITS","BALERR"))

    }

    if (outfilenames[i] == "BELO.PRE") {
      assign(tolower(outfilenames[i]),
             data.table::data.table(read.table(file.path(outpath, "BELO.PRE"),
                                               header = T))
      )

    }

    # everything lower case
    data.table::setnames(get(tolower(outfilenames[i])),
                         names(get(tolower(outfilenames[i]))),
                         tolower(names(get(tolower(outfilenames[i]))))
    )

  }


  mget( ls()[order(ls())] [which(ls() %in% tolower(outfilenames))] )

}

