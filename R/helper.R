#' Helper function: converting vertical scale into meters (Figure. 08, page 11, ENSAT report) 
#' 
#' @param cm measured centimeters in Figure 8
#' @return vertical length in meters

verticalCmToMeters <- function (cm) { 
  round(10 * cm / 3.5, 1)
}

#' Helper function: converting node ID to row/column
#' 
#' @param nodeID vector of node ids
#' @param  numberOfRows number of rows (here: nly)
#' @return row (yNode) and column of node ID in grid
nodeIdToRowColumn <- function
(
  nodeID, 
  numberOfRows
)
{
  data.frame(
    xNode = (nodeID - 1) %/% numberOfRows + 1,
    yNode = (nodeID -1) %% numberOfRows + 1
  )
}

#' Helper function: removes file extensions (copied from "kwb.quantum" package)
#' @param x file.path
#' @return file path without extension (i.e. without ".*")
removeFileExtension <- function(x) 
{
  sub("\\.[^.]+$", "", x)
}

#' Helper function: set soil ITEX to zero
#' 
#' @param nodes as retrieved by convModelCoordinatesToNodes() or
#' convRealCoordinatesToNodes()
#' @param soilGrid soil grid (as retrieved by kwb.vs2dh::vs2dh.ConfigureSoilGrid())
#' @param shape.name name of polygon shape files with ponds (Default: "Ponds)
#' @return soil grid (with possible ITEX values set to zero)
#'   
setSoilZero <- function (nodes,
                         soilGrid,
                         shape.name="Ponds"
) {
  
  condition <- nodes$shape.name==shape.name & nodes$yNode > 2
  zero <- nodes[condition,]
  
  if (nrow(zero) > 0) {
    for (ind in 1:nrow(zero))
    {
      soilGrid$jtex[2:(zero$yNode[ind]-1),zero$xNode[ind]] <- 0
      
    }
  }
  return(soilGrid)
  
}

#' Helper function: Defines feature ids for gis objectes 
#' @description Aggregates gis features by shape.name & id add adds new column fID
#' @param x data.frame with gis features as retrieved by importShapefile()
#' @return input data.frame and new column feature id (fID)
setFeatureIDs <- function(x) 
{
  ### Add feature identifer id (column fID)
  identifier <- stats::aggregate(x=x$id, by = list(shape.name=x$shape.name, id=x$id),FUN = min)
  identifier <- identifier[order(identifier$shape.name, identifier$id),]
  identifier$x <- 1:nrow(identifier)
  names(identifier)[names(identifier)=="x"] <- "fID"
  x <- merge(identifier, x)
  x <- x[order(x$shape.name, x$id),]
}

#' Helper function: Gets feature information
#' @description Aggregates gis features by shape.name & id add adds new column fID and 
#' returns additional metainformation (parameter )
#' @param gisData data.frame with gis features as retrieved by importShapefile(s)()
#' @param addColNames vector with additional colnames for output data.frame (Default: NULL),
#' for valid inputs check: colNames(gisData)
#' @return Return the features with attributes feature id (fID), shapefile name 
#' (shape.name) and feature name (Name)
getFeatures <- function (gisData, 
                         addColNames=NULL)
{
  colNames <- c("fID", "shape.name", "Name")
  
  condition <- !(addColNames %in% colNames) & addColNames %in% names(gisData)
  
  if (any(condition)) {
    colNames <- c(colNames, addColNames[condition])
  }
  
  #### Group by fId for adding meta info to x
  features <- gisData[,colNames]
  form <- stats::as.formula(paste(colNames[1], " ~ ", paste(colNames[2:length(colNames)], collapse = " + ")))
  features <- stats::aggregate(formula = form, data = features, FUN = min)
  
  return(features)
}

#' Helper function: remove features which should be ignored
#' @param gisData data.frame with gis features as retrieved by importShapefile(s)()
#' @param ignoreFeatureIDs should be a valid feature id column gisData$fID, 
#' for possible values check: unique(gisData$fID)
#' @return input gisData (possibly removed by ignoredFeatureIDs)
removeFeatures <- function (gisData, 
                            ignoreFeatureIDs = NULL)
{
  metaInfo <- getFeatures(gisData)
  
  #### If features should be ignored
  featuresToIgnore <- which(metaInfo$fID %in% ignoreFeatureIDs)
  condition <- !is.null(ignoreFeatureIDs) & length(featuresToIgnore) > 0
  if (condition)
  {
    msg <- sprintf("Feature: %-s of %s.shp ignored by user!\n", metaInfo$Name[featuresToIgnore],
                   metaInfo$shape.name[featuresToIgnore])
    message(msg)
    
    gisData <- gisData[!(gisData$fID %in% ignoreFeatureIDs),]
    
  } 
  return(gisData)
  
}

#' Helper function: convert head boundary
#' @param leftHead if TRUE left head, if FALSE right head (Default: TRUE) 
#' @param modelStructure modelStructure as retrieved by 
#' @param depthToWaterTable depthToWaterTable 
#' @param hydraulicGradient hydraulicGradient 
#' @return head boundary meta info
#' 
convHeadBoundary <- function (leftHead = TRUE,
                              modelStructure, 
                              depthToWaterTable,  
                              hydraulicGradient ) {
  
  
  offset <- hydraulicGradient * modelStructure$grid$nxr / 2
  
  if (leftHead) {
    headVal <- depthToWaterTable - offset
    headLoc <- "left"
    xNode <- 2
  } else {
    headVal <- depthToWaterTable + offset
    headLoc <- "right"
    xNode <- modelStructure$grid$nxr - 1
  }
  
  dy2 <- modelStructure$grid$dy / 2
  yVals <- seq(from = 1 - dy2 , 
               to = modelStructure$grid$nly - dy2, 
               by=modelStructure$grid$dy)
  
  diffAbs <- abs(headVal-yVals)
  ### selects the yNode closest to the surface 
  yNode <- which(diffAbs==min(diffAbs))[1] 
  
  x <- data.frame(location=headLoc, val=-headVal, xNode=xNode, yNode=yNode)
  return(x)
}

#' Helper function: define head boundary
#' @param head as retrieved by convHeadBoundary()
#' @param nly number of vertical nodes
#' @param temp temperature at boundary
#' @param ntx vector with node type identifier for boundary conditions. 0 (for 
#' no specified boundary (needed for resetting some nodes after initial recharge 
#' period); 1 (for specified pressure head); 2 (for specified flux per unit 
#' horizontal surface area in units of L/T); 3 (for possible seepage face);
#' 4 (for specified total head); 5 (for evaporation, Note: is not implemented
#' yet!); 6 (for specified volumetric flow in units of L3/T). 7 (for gravity 
#' drain). (The gravity drain boundary condition allows gravity driven vertical
#' flow out of the domain assuming a unit vertical hydraulic gradient. Flow 
#' into the domain cannot occur.)"
#' @param ntc vector with node type identifier for transport boundary conditions. 
#' 0 (for no specified boundary); 1 (for specified temperatures), (Default: 1)
#' @return boundary head parameterisation
#' 
defineHeadBoundary <- function (head,
                                nly, 
                                temp,
                                ntx = 4, 
                                ntc = 1 ) {
  yNodes <- head$yNode:(nly-1)
  n <- length(yNodes)
  x <- kwb.vs2dh::vs2dh.ConfigureBoundaryCondition(jj = yNodes, 
                                                   nn = head$xNode, 
                                                   ntx = rep(ntx,n),
                                                   pfdum = rep(head$val,n),
                                                   ntc = rep(ntc,n),
                                                   cf = rep(temp,n))
  return(x$boundary)
}

