getwd()

library("brook90r")
options(stringsAsFactors = F)



# longtermdev <- read.csv("C:/Users/pschmidtwalter/Dropbox/Arbeit/RFunktionen_Package/LSBUNN/longtermdevLSBU.csv", sep = ",")
# inicontrol <- read.csv("C:/Users/pschmidtwalter/Dropbox/Arbeit/RFunktionen_Package/LSBUNN/inicontrolLSBU.csv", sep = ";")
# names(inicontrol) <- tolower(names(inicontrol))

climate <- read.csv("C:/Users/pschmidtwalter/Dropbox/Arbeit/RFunktionen_Package/LSBUNN/Meteo_interpoliert_LS_2010_2015.csv")
climate$dates <- as.Date(climate$dates, "%m/%d/%Y") 
# param <- read.csv("C:/Users/pschmidtwalter/Dropbox/Arbeit/RFunktionen_Package/LSBUNN/paramLSBU.csv", sep = ";")
# names(param) <- tolower(names(param))
soil <- read.csv("C:/Users/pschmidtwalter/Dropbox/Arbeit/RFunktionen_Package/LSBUNN/soillayersLSBU.csv")
names(soil) <- tolower(names(soil)) # Namen müssen exakt so lauten !
soil 


inicontrol <- MakeIniControl.B90() #inicontrol-ausgangsobjekt erstellen
param <- MakeParam.B90() # parameter-Objekt erstrellen 

outmat <-  choose_output.B90() #Output auswählen

inicontrol$startdate <- as.Date("2010-01-01") #control options anpassen
inicontrol$enddate <- as.Date("2010-12-31")
inicontrol$fornetrad <- "sunhour"

param$glmax <- 0.005 # Parameter anpassen
param$maxlai <- 6

#B90 ausführen
test <- Run.B90(directory = file.path(getwd(), "tests"), #wird erstellt
                inicontrol = inicontrol,
                param = param,
                climate = data.frame(climate),
                soil = soil,
                outputmat = outmat,
                path_b90.exe = "C:/Users/pschmidtwalter/Dropbox/Arbeit/RFunktionen_Package/RFunctionen/b90.exe"
                )
str(test)
swatprof <- Aggregate.SWAT.ASC(test$swatday.asc, soil) # Beispiel für Aggregation der Schichtwassergehalte