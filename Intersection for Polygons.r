#Raghu Sidharthan - 6/18/2015

PrjDr <- "C:\\Users\\sidharthanr\\Dropbox\\Resource\\R\\GIS\\Seattle\\"

library(data.table)
library(rgdal)
library(sp)
library(rgeos)
library(shapefiles)

curDir <- "C:/Temp/RSpace";setwd(curDir);

Neighborhoods <- readOGR(paste0(PrjDr,"Neighborhoods\\WGS84"), "Neighborhoods")
unzip(paste0(PrjDr,"tracts10_shp.zip"))
tracts10 <- readOGR(paste0(curDir,"/tracts10"), "tracts10")
tracts10 <-spTransform(tracts10,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
#1. Generating a smaller neighborhod area
areaDesc <- as.character(Neighborhoods$L_HOOD)
areaDesc[is.na(areaDesc)] <- "NoName"
nhood <- Neighborhoods[areaDesc=="BALLARD",]
nhood <- nhood[nhood$S_HOOD=="West Woodland",] #West Woodland gives isection with 2 trt
extd <- as.vector(nhood@bbox)
plot(nhood)
#Getting the centroid of the polygons
trtCtrd = gCentroid(tracts10,byid=TRUE)
trtCtrddt <- data.table(trtCtrd@coords)
trtCtrddt[,isInside := (x >= extd[1] & x <= extd[3] & y >= extd[2] & y <= extd[4])]
trtsel <- tracts10[trtCtrddt$isInside,]
plot(trtsel)
plot(nhood,add=T)
check <- gIntersection(nhood,trtsel) 
plot(check)

check.poly <- check@polyobj
check.line <- check@lineobj
checkFnl <- gUnion(check.poly,check.line)


# These two function just fgives the polygon id of the other intersecting polygon
over(nhood,trtsel)
over(trtsel,nhood)


