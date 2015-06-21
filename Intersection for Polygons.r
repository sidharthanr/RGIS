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
  

tracts10 <-spTransform(tracts10,CRS("+proj=lcc +lat_1=34.03333333333333 +lat_2=35.46666666666667 +lat_0=33.5 +lon_0=-118 +x_0=2000000
+y_0=500000.0000000001 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"))
Neighborhoods <-spTransform(Neighborhoods,CRS("+proj=lcc +lat_1=34.03333333333333 +lat_2=35.46666666666667 +lat_0=33.5 +lon_0=-118 +x_0=2000000
+y_0=500000.0000000001 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"))


#1. Generating a smaller neighborhod area
areaDesc <- as.character(Neighborhoods$L_HOOD)
areaDesc[is.na(areaDesc)] <- "NoName"
nhood <- Neighborhoods#[areaDesc=="BALLARD",]
# nhood <- nhood[nhood$S_HOOD=="Whittier Heights",] #West Woodland gives isection with 2 trt
extd <- as.vector(nhood@bbox)
plot(nhood)
#Getting the centroid of the polygons
trtCtrd = gCentroid(tracts10,byid=TRUE)
trtCtrddt <- data.table(trtCtrd@coords)
trtCtrddt[,isInside := (x >= extd[1] & x <= extd[3] & y >= extd[2] & y <= extd[4])]
trtsel <- tracts10[trtCtrddt$isInside,]


allCombs <- over(nhood,trtsel,returnList = T)


allRslt <- {}
for (iloop in (1:length(allCombs))){
  print(iloop)
  soloShp <- nhood[iloop,]
  intList <- unlist(allCombs[iloop])
  for (jloop in (1:length(intList))){
    trtShp <- trtsel[intList[jloop],]
    check1 <- (gDifference(soloShp,trtShp))
#     plot(soloShp)
    plot(check1)
    plot(soloShp,add=T)
    print("After check1")
    check2 <- gArea(gDifference(soloShp,trtShp))
    print("After check2")
    temp <- (cbind(row.names(soloShp@data),row.names(trtShp@data),gArea(gDifference(soloShp,trtShp))))
    allRslt <- rbind(allRslt,temp)
  }  
}

allRsltDT <- data.table(allRslt)
allRsltDT <- allRsltDT[,lapply(.SD,as.numeric)]
setnames(allRsltDT,c("fPID","spID","combArea"))

allRsltDT[,totArea := sum(combArea),by="fPID"]
allRsltDT[,fracArea := combArea/totArea]

