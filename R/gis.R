#' GIS: imports shapefiles & dbf 
#' 
#' @param shp.path  full path to file (with or without file extension ".shp")
#' @return Imported GIS shapefile as R data.frame

importShapefile <- function (shp.path)
{
  shape.file <- removeFileExtension(shp.path)
  
  selShape <- shapefiles::read.shapefile(shape.name =  shape.file)
  
  features <- nrow(selShape$dbf$dbf)
  
  x <- data.frame()
  ### type ESRI Shape type 1=point, 3=polyLine, 5=polygon
  lookupTable <- data.frame(shape.type = c(1,3,5),
                            shape.type.name = c("Point", "Polyline", "Polygon")
  )
  
  if(selShape$shx$header$shape.type == 1) {
    
    x <- cbind(selShape$shp$shp[,-1], selShape$dbf$dbf)
    
  } else if(selShape$shx$header$shape.type %in% c(3,5)) {
    
    for(i in 1:features){
      tmp <- cbind(selShape$shp$shp[[i]]$points,
                   shape.type = selShape$shp$shp[[i]]$shape.type,
                   id = selShape$dbf$dbf$id[i])
      names(tmp) <- tolower(names(tmp))
      x <- plyr::rbind.fill(x, tmp)
    }
    x <- merge(x, selShape$dbf$dbf)
    
  }
  x <- cbind(shape.name=rep(basename(shape.file), nrow(x)),
             x)
  return(x)
}

#' GIS: imports shapefiles & dbf 
#' 
#' @param shp.files vector with full paths to shapefiles "Boundary", "Ponds", 
#' "Observation wells" to be imported (with or without file extension ".shp")
#' @return Imported GIS shapefiles in an R data.frame
#' @examples
#' shp.dir <- system.file("extdata", "qgis", package="kwb.demeau")
#' shp.files <- dir(path = shp.dir, pattern = ".shp", full.names = TRUE)
#' ### Store GIS data in R data.frame "gisData"
#' gisData <- importShapefiles(shp.files)
#' ### Plot imported GIS data:
#  xyplot(y ~ x, groups=Name, data = gisData, pch=16, auto.key=TRUE)

importShapefiles <- function (shp.files = dir(path = system.file("extdata", 
                                                                 "qgis", 
                                                                 package="kwb.demeau"), 
                                              pattern = ".shp", 
                                              full.names = TRUE))
{
  x <- data.frame()
  for(i in 1:length(shp.files))
  {
    
    shp.file <- importShapefile(shp.path = shp.files[i])
    x <- plyr::rbind.fill(x, shp.file)
  }
  
  x <- setFeatureIDs(x)
  return(x)
}

#' Helper function: check whether left model boundary lies within polygon
#' @param gisData data.frame as retrieved by importShapefiles() 
#' @param leftBoundaryRow row number index of "gisData" which contains the left 
leftBoundaryInPolygon <- function (gisData,
                                   leftBoundaryRow = 1)
{   
  polygons <- gisData$shape.type==5
  polygonIDs <- unique(gisData[polygons,"id"])
  gisData$leftBoundaryInPolygon <- 0
  for (id in polygonIDs )
  {
    polygonIDs <- unique(gisData[polygons,"id"])
    condition <- polygons & gisData$id == id
    selPoly <- gisData[condition,]
    
    inPolygon <- sp::point.in.polygon(point.x=gisData$x[leftBoundaryRow], 
                                      point.y=gisData$y[leftBoundaryRow], 
                                      pol.x=selPoly$x, 
                                      pol.y=selPoly$y, 
                                      mode.checked=FALSE)
    gisData$leftBoundaryInPolygon[condition] <- inPolygon
  }
  return(gisData)
  
}


#' Helper function: add horizontal distances from left boundary coordinates  
#' 
#' @param gisData data.frame as retrieved by importShapefiles() 
#' @param leftBoundaryRow row number index of "gisData" which contains the left 
#' model boundary (Default: 1) 
#' @return Model coordinates instead of "real" world coordinates
#' @examples
#' shp.dir <- system.file("extdata", "qgis", package="kwb.demeau")
#' shp.files <- dir(path = shp.dir, pattern = ".shp", full.names = TRUE)
#' ### Store GIS data in R data.frame "gisData"
#' gisData <- importShapefiles(shp.files)
#' ### Add horizontal distances from left boundary and ignore all features that 
#' ### have larger distances than "right" boundary
#' gisData <- addHorizontalDistances(gisData)
#' ### Plot horizontal model coordinates
#' maxVertical <- -abs(max(gisData$fcBottom, na.rm=TRUE))
#  plot(rep(0,nrow(gisData)) ~ xModel,
#      data = gisData, 
#      pch=16, 
#      ylim=rev(c(0,maxVertical)), las=1, 
#      xlab="x", 
#      ylab="y")

addHorizontalDistances <- function(gisData,
                                   leftBoundaryRow = 1) 
{
  gisData <- leftBoundaryInPolygon(gisData = gisData, leftBoundaryRow = leftBoundaryRow)
  
  ### 1) Step calculates horizontal distance from "left boundary"
  leftBoundary <- gisData[leftBoundaryRow , c("x", "y")]
  dx <- gisData$x - leftBoundary$x
  dy <- gisData$y - leftBoundary$y
  
  gisData$xModel <- sqrt(dx*dx + dy*dy)
  
  polygons <- gisData$shape.type==5
  tmp <- gisData[polygons, ]
  
  minDist <- stats::aggregate(x = tmp$xModel, by = list(shape.name=tmp$shape.name, id=tmp$id),FUN = min)
  maxDist <- stats::aggregate(x = tmp$xModel, by = list(shape.name=tmp$shape.name, id=tmp$id),FUN = max)
  
  polygonMinMaxIndexes <- gisData$xModel %in% c(minDist$x, maxDist$x)
  
  condition <- !polygons | polygonMinMaxIndexes
  x <- gisData[condition,]
  x <- x[order(x$xModel),]
  bndInd <- which(x$shape.name=="Boundary")
  if (bndInd[2] != nrow(x))
  {
    removedFeatures <- (bndInd[2]+1):nrow(x)
    msg <- sprintf("Feature: %-s of %s.shp removed (horizontal distance %5.1f > right boundary distance %5.1f\n", 
                   x$Name[removedFeatures], 
                   x$shape.name[removedFeatures], 
                   x$xModel[removedFeatures],
                   x$xModel[bndInd[2]])
    message(msg)
  }
  x <- x[bndInd[1]:bndInd[2],]
  
  ### Checking whether "left boundary" lies within a polygon (in this case the 
  ### minDist should be 0! (assumption that "right boundary" always lies outside of 
  ### polygon) -->>> could be later integrated!
  
  condBndInPoly <- tmp$leftBoundaryInPolygon > 0
  
  leftBoundaryInPolygons <- any(condBndInPoly )
  if (leftBoundaryInPolygons) {
    indices <- which(condBndInPoly)
    leftBoundaryInPolyID <- unique(tmp[indices,"id"])
    
    condition1 <- which(x$shape.type==5 & x$id %in% leftBoundaryInPolyID)[1]
    
    x$xModel[condition1] <- 0
    condition2 <- which(gisData$x > leftBoundary$x & 
                          gisData$shape.type==5 & 
                          gisData$id %in% leftBoundaryInPolyID)
    maxDistRecalc <- gisData[condition2, ]
    
    dx <- maxDistRecalc$x - leftBoundary$x
    dy <- maxDistRecalc$y - leftBoundary$y
    
    condition3 <- which(x$shape.type==5 & x$id %in% leftBoundaryInPolyID)[2]
    x$xModel[condition3] <- max(sqrt(dx*dx + dy*dy), na.rm=TRUE)
    
    msg1 <- sprintf("Feature: %s of %s.shp  (Left boundary lies inside. Minimum distance set to 0)\n", 
                    x$Name[condition1], 
                    x$shape.name[condition1])
    msg2 <- sprintf("Feature: %s of %s.shp  (Left boundary lies inside. Maximum distance set to %4.1f)\n", 
                    x$Name[condition3], 
                    x$shape.name[condition3],
                    x$xModel[condition3])
    message(msg1,msg2)
    x <- x[order(x$xModel),]
  } 
  
  
  return(x)
  
}
