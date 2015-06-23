#Raghu Sidharthan - 6/18/2015

library(data.table)
library(rgdal)
library(sp)
library(rgeos)
# This function takes two SpatialPolygonDataFrames and returns the intersection 
#area for the polygons of the first object

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



