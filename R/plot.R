#' Plot model Structure
#' @param df data.frame as retrieved by convRealToModelCoordinates() or  
#' convModelCoordinatesToNodes()
#' @param xColName name of x column to plot
#' @param yColName name of y column to plot  

### Plot the model: 
plotModelStructure <- function(df, xColName="x", yColName="y")
{
  ncols <- length(unique(df$fID))/4
  
  formula <- as.formula(sprintf("%s ~ %s", 
                                yColName, 
                                xColName))
  
  print(lattice::xyplot(formula, groups = df$Name, 
                        data=df, 
                        pch=16, 
                        auto.key=list(columns = ncols) ))
}

#' Monitoring: plot data with two y axex
#' 
#' @param parY1 name of parameter for Y1 axis (see: colnames(listToMatrixForm(df)),
#' (Default: "WaterLevelChange_cm")
#' @param parY2 name of parameter for Y2 axis (see: colnames(listToMatrixForm(df)),
#' (Default: "Temp_C")
#' @param labelParY1 user defined label for Y1 for legend (Default: 
#' "Water level change (cm)")
#' @param labelParY2 user defined label for Y2 for legend (Default: 
#' "Temperature (degree C)")
#' @param df data.frame with structure like moniDat$agg$dailyMedian
#' @param ... further parameters passed to xyplot()


plotMonitoringWithTwoYAxes <- function (parY1 = "WaterLevelChange_cm", 
                                        parY2 = "Temp_C", 
                                        labelParY1 = "Water level change (cm)",
                                        labelParY2 = "Temperature (C)",
                                        df,
                                        ...
)
{
  ### Transform aggregated monitoring data in wide format 
  df <- listToMatrixForm(df)
  
  dates <- df$myDate
  
  ## construct separate plots for each series
  obj1 <- lattice::xyplot(as.formula(sprintf("%s ~ myDate | moniLocation", parY1)), 
                          data=df, 
                          panel=function(...) {
                            panel.xyplot(...)
                            panel.abline(v=dates, col = "lightgrey",lty = 2, lwd=0.5) 
                          },
                          type = "l", 
                          xlab="", 
                          ylab="", 
                          auto.key=TRUE,
                          ...)
  obj2 <- lattice::xyplot(as.formula(sprintf("%s ~ myDate | moniLocation", parY2)), 
                          data=df,
                          panel=function(...) {
                            panel.xyplot(...)
                            panel.abline(v=dates, col = "lightgrey", lty = 2, lwd=0.5) 
                          },
                          type = "l", 
                          xlab="", 
                          ylab="",
                          auto.key=TRUE, 
                          ...)
  
  ## ...or with a key
  latticeExtra::doubleYScale(obj1, obj2, auto.key = list(columns=2,
                                                         points=F,
                                                         lines = T, 
                                                         text = c(labelParY1, 
                                                                  labelParY2)
  )
  )
}

#' Monitoring: plot data
#' 
#' @param moniParName (Default: "WaterLevel_cm")
#' @param moniLocationPattern (Default: "BSV|Tuberia")
#' @param minDate (Default: "2000-01-01")
#' @param maxDate (Default: "2015-01-01")
#' @param groups (Default: FALSE)
#' @param df data frame
plotMonitoringData <- function (moniParName="WaterLevel_cm", 
                                moniLocationPattern="BSV|Tuberia",
                                minDate = "2000-01-01", 
                                maxDate = "2015-01-01",
                                groups=FALSE,
                                df) {
  dateCol <- "myDate"
  
  if("myDateTime" %in% colnames(df))
  {
    dateCol <- "myDateTime"
  } 
  sel <- df[grepl(pattern = moniLocationPattern, df$moniLocation) & 
              df[,dateCol] > minDate & 
              df[,dateCol] < maxDate & 
              df$parVal != 0 &
              df$moniParName==moniParName,]
  if (groups==FALSE)
  {
    print(lattice::xyplot(as.formula(sprintf("parVal ~ %s | moniLocation", dateCol)), 
                          ylab=moniParName, 
                          xlab="Date",
                          data=sel, 
                          as.table=TRUE))
  } else {
    print(lattice::xyplot(as.formula(sprintf("parVal ~ %s", dateCol)),
                          groups=df$moniLocation,
                          ylab=moniParName, 
                          xlab="Date",
                          data=sel, 
                          auto.key=TRUE, #list(columns=unique(df$moniLocation)),
                          as.table=TRUE))
  }
  
}

