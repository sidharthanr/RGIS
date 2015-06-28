#Raghu Sidharthan - 6/18/2015

library(data.table)
library(rgdal)
library(sp)
library(rgeos)
library(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)

# This function takes two SpatialPolygonDataFrames and returns the intersection 
#area for the polygons of the first object

getAreaIntersectionPrl <- function(spdf1,spdf2,varLst1,varLst2)
{

  allCombs <- over(spdf1,spdf2,returnList = T)  
  allRslt <- {}
  allRslt <- foreach(iloop = (1:length(allCombs)),.packages=c('rgdal','rgeos'),.combine=rbind) %dopar% {  
# for (iloop in (1:length(allCombs))){
    pllallRslt <- {}
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
        temp <- (cbind(iloop,row.names(s1@data),row.names(s2@data),areaContribution))
        pllallRslt <- rbind(pllallRslt,temp)
      }
      unAccountedShape <- gDifference(s1,gUnaryUnion(spdf2[intList,]))
      if(!is.null(unAccountedShape)){      unAccountedArea <- gArea(unAccountedShape) 
                                           if(unAccountedArea/s1Area>0.000001){
                                             pllallRslt <- rbind(pllallRslt,(cbind(iloop,row.names(s1@data),NA, unAccountedArea)))
                                           }
      }
    }
    else{
      pllallRslt <- rbind(pllallRslt,(cbind(iloop,row.names(s1@data),NA, gArea(s1))))
    }
    pllallRslt
  }
  allRsltDT <- data.table(allRslt)
  setnames(allRsltDT,c("iloop","fPID","sPID","combArea"))  
  allRsltDT <- allRsltDT[,combArea:=as.numeric(combArea)]  
  allRsltDT[,totArea := sum(combArea),by="fPID"]
  allRsltDT[,fracArea := combArea/totArea]
  
  # merging other variables from the data frame using variables varLst1
  if(!missing(varLst1)){
    dt1 <- data.table(fPID=row.names(spdf1),data.table(spdf1@data)[,varLst1,with=FALSE]) 
    setkey(dt1,fPID)
    setkey(allRsltDT,fPID)
    allRsltDT <- dt1[allRsltDT,]#merge(allRsltDT,dt1,by="fPID")
  }
  if(!missing(varLst2)){
    dt2 <- data.table(sPID=row.names(spdf2),data.table(spdf2@data)[,varLst2,with=FALSE])
    setkey(dt2,sPID)
    setkey(allRsltDT,sPID)    
    allRsltDT <- dt2[allRsltDT,] #merge(allRsltDT,dt2,by="sPID")
  }
  return(allRsltDT)
}


