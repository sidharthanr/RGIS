#Raghu Sidharthan - 6/18/2015

PrjDr <- "C:\\Users\\sidharthanr\\Dropbox\\Resource\\R\\GIS\\Seattle\\"

library(data.table)
library(rgdal)
library(sp)
library(rgeos)

curDir <- "C:/Temp/RSpace";setwd(curDir);

# Neighborhoods is the first shapefile for which we want to get the intersection - so final output will have as many rows as this shapefile.
# unzip(paste0(PrjDr,"Neighborhoods.zip"))
# Neighborhoods <- readOGR(paste0(curDir,"/Neighborhoods/WGS84"), "Neighborhoods")
# 
# # Neighborhoods is the second shapefile on which the above is overlaid.
# unzip(paste0(PrjDr,"tracts10_shp.zip"))
# tracts10 <- readOGR(paste0(curDir,"/tracts10"), "tracts10")
#   
# #Transforming to a plannar CRS to get the area
# tracts10 <-spTransform(tracts10,CRS("+proj=lcc +lat_1=34.03333333333333 +lat_2=35.46666666666667 +lat_0=33.5 +lon_0=-118 +x_0=2000000
# +y_0=500000.0000000001 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"))
# Neighborhoods <-spTransform(Neighborhoods,CRS("+proj=lcc +lat_1=34.03333333333333 +lat_2=35.46666666666667 +lat_0=33.5 +lon_0=-118 +x_0=2000000
# +y_0=500000.0000000001 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"))


#1. Generating a smaller neighborhod area
getAreaIntersection <- function(spdf1,spdf2)
{
  allCombs <- over(spdf1,spdf2,returnList = T)  
  allRslt <- {}
  for (iloop in (1:length(allCombs))){
    s1 <- spdf1[iloop,]
    s1Area = gArea(s1)
    intList <- unlist(allCombs[iloop])
    if(length(intList)>0)
    {
      for (jloop in (1:length(intList))){
        s2 <- spdf2[intList[jloop],]
        shapeDiff <- (gDifference(s1,s2))
        areaContribution <- s1Area
        if(!is.null(shapeDiff))
        { areaContribution <- s1Area - gArea(shapeDiff) }
        temp <- (cbind(row.names(s1@data),row.names(s2@data),areaContribution))
        allRslt <- rbind(allRslt,temp)
      }
      unAccountedShape <- gDifference(s1,gUnaryUnion(spdf2[intList,]))
      if(!is.null(unAccountedShape)){      unAccountedArea <- gArea(unAccountedShape) 
      if(unAccountedArea/s1Area>0.000001){
        allRslt <- rbind(allRslt,(cbind(row.names(s1@data),NA, unAccountedArea)))
      }
      }
    }
    else{
      allRslt <- rbind(allRslt,(cbind(row.names(s1@data),NA, gArea(s1))))
    }
  }
  allRsltDT <- data.table(allRslt)
  allRsltDT <- allRsltDT[,lapply(.SD,as.numeric)]
  setnames(allRsltDT,c("fPID","spID","combArea"))
  allRsltDT[,totArea := sum(combArea),by="fPID"]
  allRsltDT[,fracArea := combArea/totArea]
  return(allRsltDT)
}


system.time(  getMapping <- getAreaIntersection(Neighborhoods,tracts10))
sum(getMapping$combArea)/gArea(Neighborhoods)

# debug help below
# sumstat  <- {}
# for(i in 1:nrow(Neighborhoods))
# {sumstat <- rbind(sumstat,gArea(Neighborhoods[i,]))}
# check <- getMapping[,j=list(fArea=sum(combArea)),by=fPID]
# check[,othWay:=sumstat]
# docsv(check)


