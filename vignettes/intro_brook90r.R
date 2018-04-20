## ---- eval = F-----------------------------------------------------------
#  install.packages("vegperiod", repos="https://www.nw-fva.de/r-pkgs")
#  install.packages("data.table", repos="https://cran.rstudio.com/")

## ---- eval = F-----------------------------------------------------------
#  devtools::install_github("pschmidtwalter/brook90r")

## ------------------------------------------------------------------------
library(brook90r)

## ---- results='hide', message=FALSE, warning=FALSE-----------------------
options.b90 <- MakeIniControl.B90()
param.b90 <- MakeParam.B90()

## ------------------------------------------------------------------------
soil <- cbind(soil_slb1, hydpar_puh2(clay = soil_slb1$clay,
                                     silt = soil_slb1$silt,
                                     sand = soil_slb1$sand,
                                     bd = soil_slb1$bd,
                                     oc.pct = soil_slb1$c_org))

## ------------------------------------------------------------------------
meteo_slb1$globrad <- meteo_slb1$globrad*0.0864

## ------------------------------------------------------------------------
output <- choose_output.B90()

## ---- results = 'hide'---------------------------------------------------
b90.results.slb1 <- Run.B90(directory = "example_run_b90/",
                            param = param.b90,
                            inicontrol = options.b90,
                            soil = soil,
                            climate = meteo_slb1,
                            outputmat = output,
                            output_log = FALSE,
                            path_b90.exe = "H:/R-Packages/brook90r/b90.exe")

## ------------------------------------------------------------------------
str(b90.results.slb1$evapday.asc)

