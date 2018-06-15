#' Helper function for main label of comparision plot
#' @param model object as retrieved by runHeatModel()
#' @return main label 
mainLabel <- function(model) {
  sprintf("ratioKzKh = %1.3f, satKh = %4.1f, neff = %1.2f, rmc = %1.2f, hydrGrad = %1.4f, alphaL = %1.2f, alphaT = %1.2f",
          model$prepared$paras$flow_ratioKzKh,
          model$prepared$paras$flow_satKh, 
          model$prepared$paras$flow_neff, 
          model$prepared$paras$flow_rmc, 
          model$prepared$paras$init_hydrGrad, 
          model$prepared$paras$heat_alphaL, 
          model$prepared$paras$heat_alphaT)
}


#' Compare measured & modelled results
#' @param modDf modDf
#' @param parName (Default: "TEMP")
#' @param func (Default: \code{\link[stats]{median}})
#' @return Plot of water level, water level change & temperature of measured vs.
#' modelled data 
#' 
selectModelled <- function(modDf, parName = "TEMP", func = stats::median) {
  expr <- stats::as.formula(sprintf("%s ~ Name + TIME_day", parName))
  
  x <- stats::aggregate(expr, data = modDf, FUN = func)
  names(x)[which(names(x) == parName)] <- "modelled"
  return(x)
}

#' Helper function to order data frame by two columns (col1, col2)
#' @param df data.frame 
#' @param col1 first column to be used for ordering (Default: Name)
#' @param col2 second column to be used for ordering (Default: TIME_day)
#' @param ... further arguments passed to function order(), e.g. decreasing = TRUE
#' @return ordered (default: accending) data.frame according to col1 & col2
orderedDataFrame <- function(df, 
                             col1 = "Name", 
                             col2 = "TIME_day" ) {
  df[order(df[, col1], df[, col2]),]
}


#' Compare measured & modelled resutls
#' @param heatModel object as retrieved by runHeatModel() 
#' @param moniDat as retrieved by processingData()
#' @param toPlot If TRUE results are plotted (Default: TRUE)
#' @return Plot/List of water level, water level change & temperature of measured vs.
#' modelled data 

compareModelledMeasured <- function(heatModel, 
                                    moniDat, 
                                    toPlot = TRUE) {
  
  infPeriod <- filterMoniData(minDate = "2009-03-03", 
                              maxDate = "2009-04-02", 
                              df = moniDat$agg$dailyMedian)
  
  infPeriod$TIME_day <- as.numeric(difftime(infPeriod$myDate,min(infPeriod$myDate) - 0.5,
                                            units = "days"))
  
 
  rowColumn <- nodeIdToRowColumn(heatModel$res$obsPoints$NODE,
                                 heatModel$prepared$conf$basic$grid$matrix$nly)
  
  obsPoints <- cbind(heatModel$res$obsPoints, rowColumn)
  
  condition <- heatModel$prepared$modelStructure$features$shape.name == "ObservationWells"
  
  modelled <- merge(heatModel$prepared$modelStructure$features[condition,],obsPoints,
                    by.x = c("xNode","yNode"), 
                    by.y = c("xNode","yNode"))
  #+ P_m + THETA + SAT + TEMP + VX + VZ H_m
  
  
  ### 1) Depth to water table 
  depthToWaterTableModelled <- selectModelled(modDf = modelled, 
                                              parName = "H_m", 
                                              func = stats::median)
  

  ## 2) Water level change 
  condition <- depthToWaterTableModelled$TIME_day == min(depthToWaterTableModelled$TIME_day)
  initialDepthToWaterTable <- depthToWaterTableModelled[condition,c("Name", "modelled")]
  names(initialDepthToWaterTable)[names(initialDepthToWaterTable) == "modelled"] <- "initial"
  
  depthToWaterTableModelled <- merge(depthToWaterTableModelled, initialDepthToWaterTable)
  

  waterLevelChangeModelled <- depthToWaterTableModelled[,c("Name", "TIME_day")] 
  
  ### Water level change in centimeter
  waterLevelChangeModelled$modelled <- 100 * (abs(depthToWaterTableModelled$initial) - abs(depthToWaterTableModelled$modelled))
  waterLevelChangeMeasured <- infPeriod[infPeriod$moniParName == "WaterLevelChange_cm",c("moniLocation", "parVal", "TIME_day")]
  names(waterLevelChangeMeasured)[names(waterLevelChangeMeasured) == "parVal"] <- "measured"
  
  waterLevelChangeModMeas <- merge(
    waterLevelChangeModelled, 
    waterLevelChangeMeasured, 
    by.x = c("Name", "TIME_day"), 
    by.y = c("moniLocation", "TIME_day"),
    all.x = TRUE
  )
  
  ### 3) Temperature 
  tempModelled <- selectModelled(
    modDf = modelled, parName = "TEMP", func = stats::median
  )
  
  tempMeasured <- infPeriod[infPeriod$moniParName == "Temp_C",c("moniLocation", "parVal", "TIME_day")]
  names(tempMeasured)[names(tempMeasured) == "parVal"] <- "measured"
  
  tempModMeas <- merge(tempModelled, tempMeasured, 
                       by.x = c("Name", "TIME_day"), 
                       by.y = c("moniLocation", "TIME_day"),
                       all.x = TRUE)
  
  if (toPlot) {
    xlabel <- "Time (in days) since start of infiltration" 
    mLabel <- mainLabel(heatModel)
    
    ### 1) Depth to water table 
    print(lattice::xyplot(
      stats::as.formula("modelled  ~ TIME_day | Name"), 
      auto.key =  list(columns = 2),
      type = "l", pch = 16, 
      data = orderedDataFrame(df = depthToWaterTableModelled), 
      ylab = "Water level (in meters) below surface", 
      xlab = xlabel,
      main = mLabel
    ))
    

#     key = list(text = list("modelled", "measured"), 
#                points = list(pch = c(16,16), col = 12:14)), pch = 16, col = c("red", "blue"))
  
    ## 2) Water level change 
    print(lattice::xyplot(
      stats::as.formula("modelled + measured ~ TIME_day | Name"),  
      auto.key =  list(columns = 2),
      type = "b", 
      pch = 16,
      data = orderedDataFrame(df = waterLevelChangeModMeas), 
      ylab = "Water level change (in centimeters)", 
      xlab = xlabel,
      main = mLabel
    ))
    ### New plotting style test
#     print(lattice::xyplot(stats::as.formula("modelled + measured ~ TIME_day | Name"),  
#                           key = list(text = list(c("modelled", "measured")), 
#                                      type = list(c("l","l")),
#                                      points = list(pch = c(16,1), 
#                                      lines = list(lty = c(1, 1)),
#                                       col = c("blue", "red")),
#                                       columns = 2), 
#                           type = c("l", "p"),
#                           pch = c(NA, 16),
#                           col = c("blue", "red"),
#                           data = orderedDataFrame(df = waterLevelChangeModMeas), 
#                           ylab = "Water level change (in centimeters)", 
#                           xlab = xlabel,
#                           main = mLabel))
    
    
    ### 3) Temperature 
    print(lattice::xyplot(
      stats::as.formula("modelled + measured ~ TIME_day | Name"), 
      auto.key = list(columns = 2),
      type = "p", pch = 16,
      data = orderedDataFrame(df = tempModMeas), 
      ylab = "Temperature (in \u00B0\ C)", 
      xlab = xlabel,
      main = mLabel
    ))
  }
  
  fitness <- list()
  fitness$waterLevelChange <- kwb.demeau::modelFitness(modelledMeasured = waterLevelChangeModMeas)
  fitness$temp <- kwb.demeau::modelFitness(modelledMeasured = tempModMeas)
  
  x <- list(depthToWaterTable = depthToWaterTableModelled,
       waterLevelChange = waterLevelChangeModMeas, 
       temp = tempModMeas, 
       fitness = fitness)
  
  return(x)
}

#' Compare measured & modelled results
#' 
#' @param heatModel object as retrieved by runHeatModel()
#' @param moniDat as retrieved by processingData()
#' @param objState model state variable to be optimised either
#'   "waterLevelChange" or "temp" (for temperature) (Default:
#'   "waterLevelChange")
#' @param objCrit vector with performance parameters produced by function
#'   hydroGOF:gof(), Default: "RMSE" (valid parameters: "ME", MAE", "MSE",
#'   "RMSE", "NRMSE", "PBIAS", "RSR", "rSD", "NSE", "mNSE", "rNSE", "d", "md",
#'   "rd", "cp", "r", "R2", "bR2", "KGE", "VE")
#' @param plotIt if TRUE lattice plot will be produced (Default: TRUE)
#' @param plot.type plot type
#' @param cex.label character expansion factor for labels
#' @param main plot title
#' @param performance.in.label if \code{TRUE}, the performance indicator appears
#'   as label
#' @param \dots additional parameters passe to plot function lattice:xyplot()
#' @return Plot of water level, water level change & temperature of measured vs.
#'   modelled data
fitnessWithLabel <- function(
  heatModel,
  moniDat,
  objState = "waterLevelChange",
  objCrit = c("RMSE", "R2"), 
  plotIt = TRUE,
  plot.type = "b",
  cex.label = 1,
  main = NULL,
  performance.in.label = TRUE,
  ...
) 
{
  compared <- compareModelledMeasured(heatModel = heatModel,
                                      moniDat = moniDat,
                                      toPlot = FALSE)
  
  fitness <- compared$fitness[[objState]][,c("Name", objCrit)]
  performance <- vector()
  for (myCol in 2:ncol(fitness)) {
    labelTmp <-
      sprintf("%s: %3.2f", names(fitness)[myCol], fitness[,myCol])
    
    if (myCol == 2) {
      performance <- labelTmp
    } else {
      performance <- sprintf("%s, %s", performance, labelTmp)
    }
  }
  
  if (performance.in.label) {
    label <- sprintf("%s (%s)", fitness$Name, performance)
  }
  else {
    label <- fitness$Name
  }
  
  fitness$Label <- label
  
  x <- merge(x = compared[[objState]], y = fitness, all = FALSE)
  x <- x[order(x$Name, x$TIME_day),]

  if (plotIt == TRUE) {
    xlabel <- "Time (in days) since start of infiltration" 
    
    if (is.null(main)) {
      main <- mainLabel(heatModel)      
    }
    
    ## 2) Water level change 
    if (objState == "waterLevelChange") ylabel <- "Water level change (in centimeters)"
    if (objState == "temp") ylabel <- "Temperature (in C)"
    
    ylabel <- list(label = ylabel, cex = cex.label)
    xlabel <- list(label = xlabel, cex = cex.label)
    
    latticeObject <- lattice::xyplot(
      stats::as.formula("modelled + measured ~ TIME_day | Label"),  
      auto.key =  list(columns = 2, cex = cex.label),
      type = plot.type, 
      pch = 16,
      data = orderedDataFrame(df = x), 
      ylab = ylabel, 
      xlab = xlabel,
      main = main,
      scales = list(
        x = list(cex = cex.label),
        y = list(cex = cex.label)
      ),
      ...
    )
    
    print(latticeObject)
    
  }
  
  return(x)
}

#' Compare measured & modelled resutls
#' @param soluteModel object as retrieved by runSoluteModel()
#' @param offset offset (Default: 0.01) used for filtering soluteModel results
#' (i.e. maxConc/2 +- offset)
#' @param toPlot If TRUE results are plotted (Default: TRUE)
#' @param ... additional parameters passe to plot function lattice:xyplot()
#' @return Plot/List of water level, water level change & temperature of measured vs.
#' modelled data 

soluteModelled <- function(soluteModel, 
                           offset = 0.01,
                           toPlot = TRUE,
                           ...) {
  
  
  
  rowColumn <- nodeIdToRowColumn(soluteModel$res$obsPoints$NODE,
                                 soluteModel$prepared$conf$basic$grid$matrix$nly)
  
  obsPoints <- cbind(soluteModel$res$obsPoints, rowColumn)
  
  condition <- soluteModel$prepared$modelStructure$features$shape.name == "ObservationWells"
  
  modelled <- merge(soluteModel$prepared$modelStructure$features[condition,],obsPoints,
                    by.x = c("xNode","yNode"), 
                    by.y = c("xNode","yNode"))
  #+ P_m + THETA + SAT + TEMP + VX + VZ H_m
  
  
  ### 1) Concentration
  concModelled <- selectModelled(modDf = modelled, 
                                 parName = "CONC", 
                                 func = stats::median)
  
  domTimess <- dominantTravelTimes(concModelled = concModelled,
                                 offset = offset)
  domTime <- domTimess$agg
  names(domTime)[names(domTime) == "TIME_day"] <- "t.dom_days"
  
  concModelled <- merge(concModelled, domTime[,-3])
  #concModelled <- concModelled[order(concModelled$Name, concModelled$TIME_day),]
  if (toPlot) {
    xlabel <- "Time (in days) since start of infiltration" 
    mLabel <- mainLabel(soluteModel)
    
    ### 1) Depth to water table 
    print(lattice::xyplot(
      stats::as.formula("modelled  ~ TIME_day | Label"), 
      auto.key =  list(columns = 2),
      type = "p", pch = 16, 
      data = concModelled, 
      ylab = "Solute concentration", 
      xlab = xlabel,
      main = mLabel,
      ...
    ))
  }
  
  x <- list(concModelled = concModelled,
            domeTimes = domTimess)
  
  return(x)
}
