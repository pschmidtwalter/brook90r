## ---- eval = F-----------------------------------------------------------
#  install.packages("data.table", repos="https://cran.rstudio.com/")

## ---- eval = F-----------------------------------------------------------
#  install.packages("vegperiod", repos="https://www.nw-fva.de/r-pkgs")
#  install.packages("sirad", repos="https://cran.rstudio.com/")
#  install.packages("foreach", repos="https://cran.rstudio.com/")
#  install.packages("doSNOW", repos="https://cran.rstudio.com/")

## ---- eval = F-----------------------------------------------------------
#  devtools::install_github(repo = "pschmidtwalter/brook90r")

## ---- message=FALSE, warning=FALSE, results='hide'-----------------------
library(brook90r)

## ------------------------------------------------------------------------
options.b90 <- MakeOptions.B90()
param.b90 <- MakeParam.B90()

## ------------------------------------------------------------------------
soil <- cbind(soil_slb1, hydpar_wessolek_mvg(tex.KA5 = soil_slb1$texture))

## ------------------------------------------------------------------------
output <- choose_output.B90()

