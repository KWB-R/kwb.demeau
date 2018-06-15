#' Calibration: helper function "modelFitness" (called by function modelFitnessAggregated())
#' @param modelledMeasured as retrieved by compareModelledMeasured() either $temp or 
#' $waterLevelChange
#' @param obsPoints  regular expression of observation points/wells to be 
#' included for goodness of fit calculation (Default: *, i.e. all); if only 
#' BSV-6_3, then: "BSV-6-3"
#' @param ... further arguments passed to hydroGOF::gof()
#' @return matrix with goodness of fit criteria

modelFitness <- function(modelledMeasured,  
                          obsPoints = "*",
                          ...) {
  
  modelledMeasured <- modelledMeasured[!is.na(modelledMeasured$measured),]
  subResult <- modelledMeasured[grep(pattern = obsPoints, modelledMeasured$Name),]
  df <- data.frame()
  for (obsPoint in unique(subResult$Name)) {

   tmp <- subResult[subResult$Name == obsPoint,]
    
  fitness <- t(hydroGOF::gof(sim =  tmp$modelled, 
                    obs =  tmp$measured, digits = 3, na.rm = TRUE, 
                    ...))
  
  colnames(fitness) <- sub(" %", "", colnames(fitness))
  
  dfTmp <- data.frame(Name = obsPoint, as.data.frame(fitness))
  if (nrow(df) == 0) {
    df <- dfTmp } else {
   df <- rbind(df, dfTmp)}
  }
  return(df)
  }

#' Calibration: helper function "modelFitnessAggregated" (called by function 
#' fitnessAdaptedModelConfiguration()
#' @param modelledMeasured as retrieved by compareModelledMeasured() either $temp or 
#' $waterLevelChange
#' @param obsPoints  regular expression of observation points/wells to be 
#' included for goodness of fit calculation (Default: *, i.e. all); if only 
#' BSV-6_3, then: "BSV-6-3"
#' @param objCrit vector with performance parameters produced by function 
#' hydroGOF:gof(), Default: "RMSE" (valid parameters: "ME", MAE", "MSE", "RMSE", 
#' "NRMSE", "PBIAS", "RSR", "rSD", "NSE", "mNSE", "rNSE", "d", "md", "rd", "cp", 
#' "r", "R2", "bR2", "KGE", "VE")
#' @param ... further arguments passed to hydroGOF::gof()
#' @return matrix with goodness of fit criteria
modelFitnessAggregated <- function(modelledMeasured, 
                                   obsPoints = "*",
                                   objCrit = "RMSE",
                                   ...) {
  
  fitness <- modelFitness(modelledMeasured, obsPoints, ...)
  
  x <- stats::median(fitness[, objCrit])
  
  cat(sprintf("RSME: %3.2f\n", x))
  
  x
}


#' Calibration: helper function "fitnessAdaptedModelConfiguration" ( called by 
#' function calibrateModel()
#' @param parameterValue parameterValue
#' @param parameterName parameterName
#' @param preparedHeatModel as retrieved by prepareHeatModel()
#' @param objState model state variable to be optimised either "waterLevelChange"  
#' or "temp" (for temperature) (Default: "waterLevelChange") 
#' @param objCrit vector with performance parameters produced by function 
#' hydroGOF:gof(), Default: "RMSE" (valid parameters: "ME", MAE", "MSE", "RMSE", 
#' "NRMSE", "PBIAS", "RSR", "rSD", "NSE", "mNSE", "rNSE", "d", "md", "rd", "cp", 
#' "r", "R2", "bR2", "KGE", "VE")
#' @param moniDat moniDat
#' @param obsPoints  regular expression of observation points/wells to be 
#' included for goodness of fit calculation (Default: *, i.e. all); if only 
#' BSV-6_3, then: "BSV-6-3"
#' @param ... further arguments passed to hydroGOF::gof()
#' @return fitness of model configuration
fitnessAdaptedModelConfiguration <- function(parameterValue, 
                                             parameterName, 
                                             preparedHeatModel,
                                             objState = "waterLevelChange",
                                             objCrit = "RMSE",
                                             moniDat,
                                             obsPoints,
                                             ...) {
  
  #### 1) Change parameter
  preparedHeatModel$paras[sprintf("flow_%s", parameterName)] <- parameterValue
  preparedHeatModel$conf$soils$props$itex1$hk[parameterName] <- parameterValue
  
  ### 2) Run 
  heatModel <- runHeatModel(preparedHeatModel = preparedHeatModel)
  
  ### 3) Compare 
#   heatModelCompare <- compareModelledMeasured(heatModel = heatModel, 
#                                               moniDat = moniDat)
  
  heatModelCompare <- fitnessWithLabel(heatModel = heatModel, 
                   moniDat = moniDat,
                   objState = objState, 
                   objCrit = objCrit )
  
  ### 4) Return goodness of fit (for water level change) 
  modelFitnessAggregated(modelledMeasured = heatModelCompare, 
                         obsPoints = obsPoints, 
                         objCrit = objCrit, 
                         ...)

}

#' Calibration: single parameter 
#' @param preparedHeatModel as retrieved by prepareHeatModel()
#' @param moniDat moniDat
#' @param obsPoints  regular expression of observation points/wells to be 
#' included for goodness of fit calculation (Default: *, i.e. all); if only 
#' BSV-6_3, then: "BSV-6-3"
#' @param parameterName parameterName (Default: "ratioKzKh")
#' @param parameterRange parameterRange (Default: c(0.01, 0.1))
#' @param objState model state variable to be optimised either "waterLevelChange"  
#' or "temp" (for temperature) (Default: "waterLevelChange") 
#' @param objCrit vector with performance parameters produced by function 
#' hydroGOF:gof(), Default: "RMSE" (valid parameters: "ME", MAE", "MSE", "RMSE", 
#' "NRMSE", "PBIAS", "RSR", "rSD", "NSE", "mNSE", "rNSE", "d", "md", "rd", "cp", 
#' "r", "R2", "bR2", "KGE", "VE"), ATTENTION: currently optimising is implemented 
#' to MINIMISE the value of only ONE selected objCrit parameter. Thus please make
#' sure that the best model fit results of the MINIMUM of the selected parameter 
#' @param ... further arguments passed to hydroGOF::gof()
#' @return calibration results
calibrateModel <- function(
  ### VS2dh parameterisation, e.g. as retrieved by prepareHeatModel()
  preparedHeatModel,
  moniDat,
  ### regular expression or name of well(s) to be used for calibration: e.g. "BSV-5"
  obsPoints="*",
  ### name of ONE hydraulic VS2dh parameter to be calibrated: e.g. "ratioKzKh",
  ### "satKh",  "ss",  "porosity",  "alpha", "rmc", "beta"   
  parameterName="ratioKzKh",
  ### min/max range of possible calibration parameter values 
  parameterRange = c(0.01, 0.1),
  objState = "waterLevelChange",
  objCrit = "RMSE",
  ...) 
{
  optResults <- stats::optimise(
    f = fitnessAdaptedModelConfiguration, 
    interval = parameterRange,
    parameterName = parameterName, 
    preparedHeatModel = preparedHeatModel, 
    moniDat = moniDat,
    obsPoints = obsPoints,
    objState = objState,
    objCrit = objCrit,
    ...
  )
  
  ### Save calibrated WTAQ configuration: 
  vs2dhConfigCalibrated <- list(
    preparedHeatModel = preparedHeatModel, 
    parameterName = parameterName, 
    parameterValue = optResults$minimum)
  
  ### Save optimisation results in list
  list(parameterName = parameterName, 
       obsPoints = obsPoints,
       optimalParameterValue = optResults$minimum, 
       minimalPerformanceValue = optResults$objective,
       vs2dhConfig = vs2dhConfigCalibrated)
  
}


# #' Calibration: multiple parameters  
# #' @param preparedHeatModel as retrieved by prepareHeatModel()
# #' @param moniDat moniDat
# #' @param obsPoints  regular expression of observation points/wells to be 
# #' included for goodness of fit calculation (Default: *, i.e. all); if only 
# #' BSV-6_3, then: "BSV-6-3"
# #' @param parameterName parameterName (Default: "ratioKzKh")
# #' @param parameterRange parameterRange (Default: c(0.01, 0.1))
# #' @param objState model state variable to be optimised either "waterLevelChange"  
# #' or "temp" (for temperature) (Default: "waterLevelChange") 
# #' @param objCrit vector with performance parameters produced by function 
# #' hydroGOF:gof(), Default: "RMSE" (valid parameters: "ME", MAE", "MSE", "RMSE", 
# #' "NRMSE", "PBIAS", "RSR", "rSD", "NSE", "mNSE", "rNSE", "d", "md", "rd", "cp", 
# #' "r", "R2", "bR2", "KGE", "VE"), ATTENTION: currently optimising is implemented 
# #' to MINIMISE the value of only ONE selected objCrit parameter. Thus please make
# #' sure that the best model fit results of the MINIMUM of the selected parameter 
# #' @param ... further arguments passed to hydroGOF::gof()
# #' @return calibration results
#   multiCalibrateModel <- function(
#   ### VS2dh parameterisation, e.g. as retrieved by prepareHeatModel()
#   preparedHeatModel,
#   moniDat,
#   ### regular expression or name of well(s) to be used for calibration: e.g. "BSV-5"
#   obsPoints="*",
#   ### name of ONE hydraulic VS2dh parameter to be calibrated: e.g. "ratioKzKh",
#   ### "satKh",  "ss",  "porosity",  "alpha", "rmc", "beta"   
#   paras = list("ratioKzKh" = list(limits = c(0.01, 0.1), 
#                initial = min(limits) + diff(limits)/2),
#                "satKh" = ),
#   ### min/max range of possible calibration parameter values 
#   parameterRange = c(0.01, 0.1),
#   objState = "waterLevelChange",
#   objCrit = "RMSE",
#   ...) 
# {
#   optResults <- optim(
#     f = fitnessAdaptedModelConfiguration, 
#     interval = parameterRange,
#     parameterName = parameterName, 
#     preparedHeatModel = preparedHeatModel, 
#     moniDat = moniDat,
#     obsPoints = obsPoints,
#     objState = objState,
#     objCrit = objCrit,
#     ...)
#   
#   ### Save calibrated WTAQ configuration: 
#   vs2dhConfigCalibrated <- list(
#     preparedHeatModel = preparedHeatModel, 
#     parameterName = parameterName, 
#     parameterValue = optResults$minimum)
#   
#   ### Save optimisation results in list
#   list(parameterName = parameterName, 
#        obsPoints = obsPoints,
#        optimalParameterValue = optResults$minimum, 
#        minimalPerformanceValue = optResults$objective,
#        vs2dhConfig = vs2dhConfigCalibrated)
#   
# }
# 
# defineCaliPara <- function(parName = "ratioKzKh", 
#                            limits = c(0.01, 0.1), 
#                             initial = min(limits) + diff(limits)/2) {
# 
#   x <- list()
#   x <-  list(list(initial = initial, limits = limits)
# 
# }
