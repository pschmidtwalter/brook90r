meteo_slb1 <- read.csv("data-raw/golden_ds_slf1nn_climate.csv", stringsAsFactors = F)
meteo_slb1$Dates <- as.Date(meteo_slb1$Dates, "%m/%d/%Y")
names(meteo_slb1)[c(1,8)] <- c("dates", "wind")
str(meteo_slb1)
meteo_slb1$esat <- with(meteo_slb1, sirad::es(Tmax = tmax,Tmin = tmin))
meteo_slb1$vappres <- with(meteo_slb1, esat*rh/100)
meteo_slb1 <- meteo_slb1[,-which(names(meteo_slb1) == "esat")]
setDT(meteo_slb1)

soil_slb1 <- read.csv("data-raw/soil_slb1.csv", stringsAsFactors = F)
names(soil_slb1)[c(2,3)] <- c("upper", "lower")
soil_slb1 <- soil_slb1[,-which(names(soil_slb1) =="rel_rootlength_density")]
soil_slb1$gravel <- soil_slb1$gravel/100
setDT(soil_slb1)
soil_slb1 <- soil_slb1[-c(1:3,20),]

devtools::use_data(meteo_slb1, soil_slb1,overwrite = T)
