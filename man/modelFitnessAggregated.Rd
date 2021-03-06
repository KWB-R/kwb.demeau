% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calibrate.R
\name{modelFitnessAggregated}
\alias{modelFitnessAggregated}
\title{Calibration: helper function "modelFitnessAggregated" (called by function 
fitnessAdaptedModelConfiguration()}
\usage{
modelFitnessAggregated(modelledMeasured, obsPoints = "*",
  objCrit = "RMSE", ...)
}
\arguments{
\item{modelledMeasured}{as retrieved by compareModelledMeasured() either $temp or 
$waterLevelChange}

\item{obsPoints}{regular expression of observation points/wells to be 
included for goodness of fit calculation (Default: *, i.e. all); if only 
BSV-6_3, then: "BSV-6-3"}

\item{objCrit}{vector with performance parameters produced by function 
hydroGOF:gof(), Default: "RMSE" (valid parameters: "ME", MAE", "MSE", "RMSE", 
"NRMSE", "PBIAS", "RSR", "rSD", "NSE", "mNSE", "rNSE", "d", "md", "rd", "cp", 
"r", "R2", "bR2", "KGE", "VE")}

\item{...}{further arguments passed to hydroGOF::gof()}
}
\value{
matrix with goodness of fit criteria
}
\description{
Calibration: helper function "modelFitnessAggregated" (called by function 
fitnessAdaptedModelConfiguration()
}
