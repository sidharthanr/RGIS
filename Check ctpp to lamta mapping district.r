# Raghu Sidharthan - 6/22/2015

# Checking the SCAG CTPP to LAMTA TAZ mapping

source("C:\\Users\\sidharthanr\\Dropbox\\RGIS\\Intersection for Polygons.r")


PrjDr <- "U:\\Projects\\SCAG_28050A\\Tasks\\Data"

library(data.table)
library(rgdal)
library(sp)
library(rgeos)

curDir <- "C:/Temp/RSpace";setwd(curDir);


ctpp <- readOGR(paste0(PrjDr,"\\SCAG_CTPP"), "SCAG_CTPP_Basic")
lamta <- readOGR(paste0(PrjDr,"\\LACMTA_TAZ"), "Metro_Dist20")

#Transforming to a plannar CRS to get the area
ctpp <-spTransform(ctpp,CRS("+proj=lcc +lat_1=34.03333333333333 +lat_2=35.46666666666667 +lat_0=33.5 +lon_0=-118 +x_0=2000000
+y_0=500000.0000000001 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"))
lamta <-spTransform(lamta,CRS("+proj=lcc +lat_1=34.03333333333333 +lat_2=35.46666666666667 +lat_0=33.5 +lon_0=-118 +x_0=2000000
+y_0=500000.0000000001 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"))


system.time(  getMapping <- getAreaIntersection(ctpp,lamta,c("TAZ"),c("taz_Renumb")))
sum(getMapping$combArea)/gArea(ctpp)

getMapping$TAZCTPP  <- substr(getMapping$TAZ,6,13)

SCAG_CTPP_TAZ_SEQ <- fread("u:\\Projects\\SCAG_28050A\\Tasks\\CTPP\\Outputs\\SCAG_CTPP_TAZ_SEQ.csv")
setkey(SCAG_CTPP_TAZ_SEQ,TAZCTPP)
setkey(getMapping,TAZCTPP)

nrow(getMapping)
getMapping <- merge(getMapping,SCAG_CTPP_TAZ_SEQ)

setkeyv(getMapping,c("TAZSEQ","taz_Renumb"))

