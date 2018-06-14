#' Processing: data preprocessing
#'  
#' @param rawData raw data imported with importData()

processingData <- function (rawData) {
  ### Add Date column in raw data
  rawData$raw$timeSeries$myDate <- as.Date(rawData$raw$timeSeries$myDateTime, tz="CET")
  rawData$raw$comments$myDate <- as.Date(rawData$raw$comments$myDateTime, tz="CET")
  
  ### 1.1) Aggregate to daily median values 
  tsDailyMedian <- aggregate(parVal ~ myDate + moniLocation + moniParName,
                             data = rawData$raw$timeSeries, 
                             FUN = median)
  #tsDailyMedian <- tsDailyMedian[tsDailyMedian$parVal != 0,]
  
  commentsDailyMedian <- aggregate(parVal ~ myDate + moniLocation,
                                   data = rawData$raw$comments, 
                                   FUN = median)
  commentsDailyMedian <- commentsDailyMedian[commentsDailyMedian$parVal != 0,]
  commentsDailyMedian <- kwb.utils::hsRenameColumns(dframe = commentsDailyMedian, 
                                                    renames = list(parVal="commentID"))
  
  rawData$agg$dailyMedian <- merge(tsDailyMedian, commentsDailyMedian, all.x=TRUE)
  rawData$agg$dailyMedian$Type <- "measured"
  
  ### 1.2) Correct water levels with barometric pressure (using daily median data, 
  ###      not cleaned by "commentID")
  baro <- rawData$agg$dailyMedian 
  baro <- baro[baro$moniLocation == "BSV-2_BARO65699" & 
                 baro$moniParName == "WaterLevel_cm",c("myDate", 
                                                       "parVal")]
  
  baro <- kwb.utils::hsRenameColumns(dframe = baro, 
                                     renames = list(parVal="BaroPressure_cm"))
  
  waterLevels <- rawData$agg$dailyMedian
  waterLevels <- waterLevels[waterLevels$moniParName == "WaterLevel_cm", ]
  waterLevels <- kwb.utils::hsRenameColumns(dframe = waterLevels, 
                                            renames = list(parVal="WaterLevel_cm"))
  
  baroCor <- merge(waterLevels, baro)
  baroCor$moniParName <- "DiverBaro_cm"
  baroCor$parVal <- baroCor$WaterLevel_cm - baroCor$BaroPressure_cm
  baroCor <- baroCor[baroCor$moniLocation != "BSV-2_BARO65699",]
  baroCor$Type <- "calc."
  
  
  rawData$agg$dailyMedian <- rbind.fill(rawData$agg$dailyMedian, baroCor[,c("myDate",
                                                                            "moniLocation", 
                                                                            "moniParName", 
                                                                            "parVal", 
                                                                            "Type")]
  )
  
  rawData$agg$dailyMedian <- calcWaterLevelChange(refDate="2009-03-02",
                                                  df = rawData$agg$dailyMedian)
  
  return(rawData)
}