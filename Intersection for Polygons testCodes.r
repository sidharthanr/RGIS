#Raghu Sidharthan - 6/22/2015
#Call the intersection code and do checks to ensure consitent results
source("C:\\Users\\sidharthanr\\Dropbox\\RGIS\\Intersection for Polygons.r")


PrjDr <- "C:\\Users\\sidharthanr\\Dropbox\\Resource\\R\\GIS\\Seattle\\"

library(data.table)
library(rgdal)
library(sp)
library(rgeos)

curDir <- "C:/Temp/RSpace";setwd(curDir);

# Neighborhoods is the first shapefile for which we want to get the intersection - so final output will have as many rows as this shapefile.
unzip(paste0(PrjDr,"Neighborhoods.zip"))
Neighborhoods <- readOGR(paste0(curDir,"/Neighborhoods/WGS84"), "Neighborhoods")

# Neighborhoods is the second shapefile on which the above is overlaid.
unzip(paste0(PrjDr,"tracts10_shp.zip"))
tracts10 <- readOGR(paste0(curDir,"/tracts10"), "tracts10")
  
#Transforming to a plannar CRS to get the area
tracts10 <-spTransform(tracts10,CRS("+proj=lcc +lat_1=34.03333333333333 +lat_2=35.46666666666667 +lat_0=33.5 +lon_0=-118 +x_0=2000000
+y_0=500000.0000000001 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"))
Neighborhoods <-spTransform(Neighborhoods,CRS("+proj=lcc +lat_1=34.03333333333333 +lat_2=35.46666666666667 +lat_0=33.5 +lon_0=-118 +x_0=2000000
+y_0=500000.0000000001 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"))


system.time(  getMapping <- getAreaIntersection(Neighborhoods,tracts10))
sum(getMapping$combArea)/gArea(Neighborhoods)

# debug help below
# sumstat  <- {}
# for(i in 1:nrow(Neighborhoods))
# {sumstat <- rbind(sumstat,gArea(Neighborhoods[i,]))}
# check <- getMapping[,j=list(fArea=sum(combArea)),by=fPID]
# check[,othWay:=sumstat]
# docsv(check)


