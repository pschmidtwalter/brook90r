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
names(soil) <- tolower(names(soil))


inicontrol <- MakeIniControl.B90()
param <- MakeParam.B90()


#Clapp-HOrnberger Parameters: loamy sand
# soil <- data.frame(upper = seq(0,-0.9,by =-0.1), lower = seq(-0.1,-1,by =-0.1),
#                    ths = rep(0.41,10), thr = rep(0.203,10), alpha = rep(-3.8,10),
#                    npar = rep(4.38,10), ksat = rep(3.5,10), tort = rep(0.92, 10), gravel = rep(0,10))

outmat <-  choose_output.B90()


inicontrol$startdate <- as.Date("2010-01-01")
inicontrol$enddate <- as.Date("2010-12-31")
inicontrol$fornetrad <- "sunhour"

# precinterval <- read.csv("tests/precinterval.csv",stringsAsFactors = F, sep = ";")
# setDT(precinterval)
# str(precinterval)
# precinterval <- precinterval[!is.na(Prec),]
# precinterval[,Date := as.Date(Date)]
# precinterval[,PINT := 1:.N, by = Date]
# precinterval[,max(PINT), by = Date][,min(V1)] # Check: all 1:24?
# precinterval[, c("yr", "mo", "da") := list(as.integer(format(Date, "%Y")),
#                                            as.integer(format(Date, "%m")),
#                                            as.integer(format(Date, "%d")))]
# write.table(precinterval[,.(yr,mo,da, numpreint = PINT, Prec, msfl = rep(0,.N))],
#             file = "tests/in/PRFILE.DAT", row.names=F, col.names = F)

#inicontrol$prec.interval <- 24

soil




param$glmax <- 0.005
param$maxlai <- 6

testclim21 <- Run.B90(directory = file.path(getwd(), "tests"),
                inicontrol = inicontrol,
                param = param,
                climate = data.frame(climate),
                soil = soil,
                outputmat = outmat,
                write.climate.in = TRUE,
                write.param.in = TRUE,
                keepoutputfiles = TRUE,
                path_b90.exe = "C:/Users/pschmidtwalter/Dropbox/Arbeit/RFunktionen_Package/RFunctionen/b90.exe"
                )

swatprof <- Aggregate.SWAT.ASC(testclim21$swatday.asc, soil)
swatprof[,dates := as.Date(paste(yr,mo,da, sep="-"))]
testclim21$swatday.asc[,dates := as.Date(paste(yr,mo,da, sep="-"))]
testclim21$beloday.asc[, dates := as.Date(paste(YR,MO,DA, sep="-"))]

swatprof[,plot(dates,SWAT_we, type="l")]



# Simulationsergebnisse: Wassergehalt der Schichten  -------------------------------------------------------
library(reshape)

lay <- soil[,c("upper","lower")]
lay$nl <- 1:nrow(lay)
setDT(lay)
setkey(lay, nl)


setkey(testclim21$swatday.asc,nl)
testclim21$swatday.asc <- testclim21$swatday.asc[lay]

testclim21$swatday.asc[,Datum := as.Date(paste(yr,mo,da, sep="-"))]

SWATI_wide <- testclim21$swatday.asc[!nl %in% 1:5,list(Datum, upper, lower,theta, wetnes)]
SWATI_wide <- cast(SWATI_wide, Datum~lower, value="theta")

colors <- colorRampPalette(c("#7F0000","red", "#FF7F00","yellow",
                             "#7FFF7F","cyan","#007FFF","blue","#00007F"))

#Subset
test <- SWATI_wide


#z: data.matrix. Wenn um eins k?rzer als y, dann wird y als schichtgrenzen interpretiert!
y <- c(as.numeric(names(test)[2:length(names(test))]),0)
z <- as.matrix(test[,c(2:length(names(test)))])


breaks <- seq(min(z), max(z),length.out=100)


mat <- matrix(c(1,2),nrow=1, byrow=TRUE)
layout(mat,width=c(0.9,0.1))
par(mar=c(4,4,2,1), oma=c(0,0,2,0))
image( x=test$Datum, y=y, z=z,
       col=colors(length(breaks)-1), breaks=breaks,   ylab="Tiefe", xlab="")

#axis(2,at= seq(-2,0, by=0.2), las=2 )
par(mar=c(5,2,4,1))

image.scale(z, col=colors(length(breaks)-1), breaks=breaks,horiz = F,
            xlab="", ylab="")
title("Vol%",line = 1)
title("Wassergehalte der Berechnungsknoten" ,outer = T, line=0)
soil


#This function creates a color scale for use with e.g. the image()
#function. Input parameters should be consistent with those
#used in the corresponding image plot. The "horiz" argument
#defines whether the scale is horizonal(=TRUE) or vertical(=FALSE).
#Depending on the orientation, x- or y-limits may be defined that
#are different from the z-limits and will reduce the range of
#colors displayed.

image.scale <- function(z, zlim, col = heat.colors(12),
                        breaks, horiz=TRUE, ylim=NULL, xlim=NULL, ...){
  if(!missing(breaks)){
    if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
  }
  if(missing(breaks) & !missing(zlim)){
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  if(missing(breaks) & missing(zlim)){
    zlim <- range(z, na.rm=TRUE)
    zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
    zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  poly <- vector(mode="list", length(col))
  for(i in seq(poly)){
    poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
  }
  xaxt <- ifelse(horiz, "s", "n")
  yaxt <- ifelse(horiz, "n", "s")
  if(horiz){YLIM<-c(0,1); XLIM<-range(breaks)}
  if(!horiz){YLIM<-range(breaks); XLIM<-c(0,1)}
  if(missing(xlim)) xlim=XLIM
  if(missing(ylim)) ylim=YLIM
  plot(1,1,t="n",ylim=ylim, xlim=xlim, xaxt=xaxt, yaxt=yaxt, xaxs="i", yaxs="i",...)
  for(i in seq(poly)){
    if(horiz){
      polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
    }
    if(!horiz){
      polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
    }
  }
}






setkey(testclim22$swatday.asc,nl)
testclim22$swatday.asc <- testclim22$swatday.asc[lay]
testclim22$swatday.asc[,max(upper[wetnes>0.95]), by = list(yr,doy)][,plot(V1, type="l")]
testclim22$swatday.asc[,max(upper[wetnes>0.95]), by = list(yr,doy)][,sum(V1 > -0.6)]

testclim22$beloday.asc[,dates := as.Date(paste(YR,MO,DA,sep="-" ))]
testclim22$beloday.asc[NL==24,list(NTFL=sum(NTFL),DSFL=sum(DSFL), VRFL=sum(VRFL)), by=list(YR,NL)  ]
testclim22$beloday.asc[NL==24,list(NTFL=sum(NTFL),DSFL=sum(DSFL), VRFL=sum(VRFL)), by=list(YR,NL)  ]
testclim22$beloday.asc[NL==25,plot(dates,VRFL, type="l")]
testclim22$beloday.asc[NL==25,plot(dates,DSFL, type="l")]
testclim22$beloday.asc[NL==25,plot(dates,cumsum(NTFL), type="l")]



testclim2$swatday.asc[,max(upper[wetnes>0.95]), by = list(yr,doy)][,plot(V1, type="l")]

testclim21$beloday.asc[NL == 25,sum(DSFL), by = list(YR,NL)]


View(test$beloday.asc[,as.list(range(DSFL)), by=list(YR,NL) ])

test$swatprofileday <- Aggregate.SWAT.ASC(test$swatday.asc, soil)


