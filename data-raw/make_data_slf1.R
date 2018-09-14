meteo_slb1 <- read.csv("data-raw/slf1nn_climate.csv", stringsAsFactors = F)
meteo_slb1$Dates <- as.Date(meteo_slb1$Dates, "%m/%d/%Y")
names(meteo_slb1)[c(1,8)] <- c("dates", "wind")
meteo_slb1 <- meteo_slb1[order(meteo_slb1$dates),]
str(meteo_slb1)
setDT(meteo_slb1)
devtools::use_data(meteo_slb1, overwrite = T)

soil_slb1 <- read.csv("data-raw/soil_slb1.csv", stringsAsFactors = F)
names(soil_slb1)[c(2,3)] <- c("upper", "lower")
soil_slb1 <- soil_slb1[,-which(names(soil_slb1) =="rel_rootlength_density")]
soil_slb1$gravel <- soil_slb1$gravel/100
setDT(soil_slb1)
soil_slb1 <- soil_slb1[-c(1:3,20),]
devtools::use_data(soil_slb1, overwrite = T)

standprop_slb1 <- read.csv("data-raw/LongTermVegDev_slb1.csv", stringsAsFactors = F)
standprop_slb1 <- standprop_slb1[,-c(1,2,4)]
names(standprop_slb1)[which(names(standprop_slb1) == "density")] <- "densef"
setDT(standprop_slb1)
devtools::use_data(standprop_slb1, overwrite = T)
