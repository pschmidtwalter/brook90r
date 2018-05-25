## ---- eval = F-----------------------------------------------------------
#  install.packages("vegperiod", repos="https://www.nw-fva.de/r-pkgs")
#  install.packages("data.table", repos="https://cran.rstudio.com/")

## ---- eval = F-----------------------------------------------------------
#  devtools::install_github("pschmidtwalter/brook90r")

## ---- message=FALSE, warning=FALSE, results='hide'-----------------------
library(brook90r)

## ---- results='hide', message=FALSE, warning=FALSE-----------------------
options.b90 <- MakeIniControl.B90()
param.b90 <- MakeParam.B90()

## ------------------------------------------------------------------------
soil <- cbind(soil_slb1, hydpar_wessolek_mvg(tex.KA5 = soil_slb1$texture))

## ------------------------------------------------------------------------
output <- choose_output.B90()

## ---- eval = T-----------------------------------------------------------
b90.results.slb1 <- Run.B90(directory = "/example_run_b90/",
                            param = param.b90,
                            inicontrol = options.b90,
                            soil = soil,
                            climate = meteo_slb1,
                            outputmat = output,
                            output_log = "b90.log",
                            path_b90.exe = "H:/R-Packages/brook90r/b90.exe")

## ------------------------------------------------------------------------
str(b90.results.slb1$evapday.asc)

## ------------------------------------------------------------------------
b90.results.slb1$evapday.asc[, dates := as.Date(paste(yr, mo, da, sep = "-"))]

## ------------------------------------------------------------------------
b90.results.slb1$swatday.asc[, dates := as.Date(paste(yr, mo, da, sep = "-"))]
swat100cm <- b90.results.slb1$swatday.asc[nl <= 14, 
                                          list(swat100_mm = sum(swati)), 
                                          by = dates ]

## ---- fig.height=3, fig.width=6------------------------------------------
par(mar=c(4.1,4.1,1.1,4.1))
plot(b90.results.slb1$evapday.asc$dates, 
     b90.results.slb1$evapday.asc$tran, type ='l',
     col = "green", ylab = "tran [mm]", xlab = "")
par(new =T)
plot(swat100cm$dates,
     swat100cm$swat100_mm, 
     ylim=c(150, 350), type ='l', col = "blue",
     xaxt = "n", yaxt ="n", xlab= "", ylab = "")
axis(4,pretty(c(150,350)))
mtext("swat_100cm [mm]", side = 4, line =3)
legend("bottom",inset = -0.4,
       legend = c("tran", "swat100cm"),
       col = c("green", "blue"),  lty = 1, 
      bty = "n", xpd =T,  horiz = T,  text.width = 100)

