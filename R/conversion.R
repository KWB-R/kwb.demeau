#' Conversion: feature parameterisation 
#' 
#' @param x absolute horizontal min/max coordinates of model feature. L, Default: c(100,220)
#' @param y absolute vertical min/max coordinates of model feature. L, Default: c(0,0)
#' @param dx dx
#' @param dy dy 
#' @param pondDepth Depth of pond below ground level (Default: verticalCmToMeters(cm = 1.2), 
#' i.e. 3.4m, derived from Figure 8, p.11 ENSAT report)
#' @param steepness of pond bank (in degree) only used for ponds if "pondDepth" 
#' is defined (Default: 45 degree)
#' @return Parameterisation of model features

modelCoordinatesToFeature <- function  (x = c(100,220),
                                        y = 0, 
                                        dx = 0.5,
                                        dy = 0.5,
                                        pondDepth = verticalCmToMeters(cm = 1.2),### optional parameter for ponds: 
                                        steepness = 45 ### optional parameter for pond degree
) 
{
  out <- data.frame()
  
  #### Is Pond? 
  if (!is.na(pondDepth) & length(y)==1)
  {
    pondDepth <- abs(pondDepth)
    
    if (!is.na(steepness) & pondDepth > 0) {
      if (steepness > 0 & steepness < 90) {
        alpha <- pi * steepness / 180  ### in radians
        
        xOffset <- pondDepth / sin(alpha)
        
        y <- stats::median(y)
        yOffset <- y - pondDepth
        
        xOffsetRounded <- ceiling(xOffset/dx)*dx 
        yOffsetRounded <-  ceiling(yOffset/dy)*dy 
        
        bank <- stats::approx(
          x = c(0, xOffsetRounded), 
          y = c(y, yOffsetRounded),
          xout = seq(0, xOffsetRounded, by = dx)
        )
        
        leftBank <- data.frame(
          x = seq(from = x[1], to = x[1] + xOffsetRounded, by = dx),
          y = y + bank$y
        )
        
        rightBank <- data.frame(
          x = seq(from = x[2]  - xOffsetRounded, to = x[2], by = dx),
          y = y + bank$y[order(bank$y,decreasing = FALSE)]
        )
        
        out<- leftBank
        out<- rbind(out, rightBank)
        
        xCoord <- seq(from = x[1] + xOffsetRounded, to = x[2] - xOffsetRounded, by = dx)
        
        bottom <- data.frame(x = xCoord,
                             y = rep(yOffsetRounded, length(xCoord)))
        
        out <- rbind(out, bottom)
      }} else if (pondDepth == 0) {
        xCoord <- seq(from = x[1], 
                      to = x[2],
                      by = dx)
        
        out <- data.frame(x = xCoord,
                          y = rep(y, length(xCoord)))
        
      } else {
        stop("No valid daty input for pond. Specify column 'steepness' and 'pondDepth' in 
             shapefile 'Ponds.shp'!")
      } 
    
  } else if (length(x)==1 & length(y)==2) {   #### Is Observation well or boundary? 
    y <- abs(y)
    yCoord <- seq(from = y[1], 
                  to = y[2],
                  by = dy)
    
    out <- data.frame(x = rep(x,length(yCoord)),
                      y = -yCoord)
    
  }  else {
    stop("Check input data types")
  }
  
  out <- out[order(out$x), ]
  
  return(out)
}

#' Conversion: "real" coordinates to model coordinates  
#' 
#' @param gisData data.frame() as retrieved by importShapefiles() 
#' @param dx horizontal model grid spacing (Default: 1)
#' @param dy vertical model grid spacing (Default: 1)
#' @param y vertical model extent (Default: NULL, i.e. maximum filter screen depth below ground level)
#' @examples
#' shp.dir <- system.file("extdata", "qgis", package="kwb.demeau")
#' shp.files <- dir(path = shp.dir, pattern = ".shp", full.names = TRUE)
#' gisData <- importShapefiles(shp.files)
#' ### Optionally remove some features 
#' # gisData <- removeFeatures(gisData = gisData, ignoreFeatureIDs = c(3,20))
#' modelCoords <- convRealToModelCoordinates(gisData, dx=1, dy=1)
#' xyplot(y ~ x, groups = Name, data=modelCoords, pch=16, auto.key=TRUE)

convRealToModelCoordinates  <- function(gisData, 
                                        dx = 1, 
                                        dy = 1, 
                                        y = NULL) 
{
  
  gisData <- addHorizontalDistances(gisData)
  
  metaInfo <- getFeatures(gisData=gisData)
  
  if (is.null(y)) y <- max(gisData$fcBottom,na.rm=TRUE)
  x <- data.frame()
  
  for (id in metaInfo$fID)
  {
    condition <- which(gisData$fID==id)
    selFeature <- gisData[condition, ]
    out <- data.frame()
    if (all(selFeature$shape.type == 5)) { ### Is Polygon ,i.e. Pond? 
      out <- modelCoordinatesToFeature(
        x = selFeature$xModel,
        y = 0,
        dx = dx, 
        dy = dy, 
        pondDepth = stats::median(selFeature$pondDepth, na.rm = TRUE), 
        steepness = stats::median(selFeature$steepness, na.rm = TRUE)
      )
      
    } else if (selFeature$shape.type == 1 &&  #### Is Boundary
                 is.na(selFeature$fcTop) &&
                 is.na(selFeature$fcBottom)) { 
      out <-  modelCoordinatesToFeature(x = selFeature$xModel,
                                        y = c(0,y),
                                        dx = dx, 
                                        dy = dy)
    } else if (selFeature$shape.type == 1 &&  #### Is Observation well?
                 !is.na(selFeature$fcTop) &&
                 !is.na(selFeature$fcBottom)) {
      out <-  modelCoordinatesToFeature(x = selFeature$xModel,
                                        y = c(selFeature$fcTop,
                                              selFeature$fcBottom),
                                        dx = dx, 
                                        dy = dy)
      
    } else {
      stop("Check data input type!")
    }
    out <- data.frame(fID=id, out)
    x <- plyr::rbind.fill(x, out)
  }
  
  x <- merge(x, metaInfo)
  return(x)
}


#' Conversion: model coordinates to nodes  
#' 
#' @param modelCoords as retrieved by convRealToModelCoordinates() 
#' @param dx horizontal model grid spacing (Default: 1)
#' @param dy vertical model grid spacing (Default: 1)
#' @return Model nodes corresponding to model coordinates 
#' @examples
#' shp.dir <- system.file("extdata", "qgis", package="kwb.demeau")
#' shp.files <- dir(path = shp.dir, pattern = ".shp", full.names = TRUE)
#' gisData <- importShapefiles(shp.files)
#' ### Optionally remove some features 
#' # gisData <- removeFeatures(gisData = gisData, ignoreFeatureIDs = c(3,20))
#' modelCoords <- convRealToModelCoordinates(gisData, dx=1, dy=1)
#' modelNodes <- convModelCoordinatesToNodes(modelCoords=modelCoords)

convModelCoordinatesToNodes <- function(modelCoords, 
                                        dx = 1, 
                                        dy = 1) 
{
  nodes <- data.frame(xNode = round(abs(modelCoords$x/dx),0),
                      yNode = round(abs(modelCoords$y/dy),0))
  
  xNodeZero <- nodes$xNode==0 
  yNodeZero <- nodes$yNode==0 
  
  if (any(xNodeZero)) {
    nodes$xNode[xNodeZero] <- 1
  }
  if (any(yNodeZero)) {
    nodes$yNode[yNodeZero] <- 1
  }
  
  #### 1) Required because "real" model start in second column/row
  nodes <- nodes + 1
  
  features <- cbind(modelCoords, nodes)
  features$XR_m <-  features$xNode*dx + (dx/2)
  features$Z_m <-  features$yNode*dy + (dy/2)
  grid <- list(nxr = max(nodes$xNode) + 1, ### +1,  "real" model ends in second column
               nly = max(nodes$yNode) + 1, ### +1,"real" model ends in second row
               dx = dx, 
               dy = dy)
  
  x <- list(grid = grid, 
            features = features)
  return(x)
}

#' Conversion: "real" coordinates to model nodes
#' @description By calling functions convRealToModelCoordinates() and  convModelCoordinatesToNodes()
#' @param gisData data.frame() as retrieved by importShapefiles() 
#' @param dx horizontal model grid spacing (Default: 1)
#' @param dy vertical model grid spacing (Default: 1)
#' @param y vertical model extent (Default: NULL, i.e. maximum filter screen depth below ground level)
#' @return model nodes

convRealCoordinatesToNodes <- function(gisData, 
                                       dx = 1, 
                                       dy = 1,
                                       y = NULL) {
  
  modelCoords <- convRealToModelCoordinates(gisData = gisData, 
                                            dx = dx, 
                                            dy = dy, 
                                            y = y)
  
  modelNodes <- convModelCoordinatesToNodes(modelCoords = modelCoords,
                                            dx = dx,
                                            dy = dy)
  
  return(modelNodes)
  
}