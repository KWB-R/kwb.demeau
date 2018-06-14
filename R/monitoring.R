
#' Monitoring: convert to list & add monitoring columns
#'  
#' @param df dataframe containing the data to be transformed
#' @param  keyFields keyFields to be used as keyFields in kwb.hsMatrixToListForm 
#' (Default: "myDateTime")

convertToListAndAddMoniColumns <- function (df,
                                            keyFields="myDateTime"){
  ### Convert to list form
  tmp_List <- kwb.utils::hsMatrixToListForm(df = df, 
                                            keyFields = keyFields) 
  
  ### Remove rows where parVal == NA 
  tmp_List <- tmp_List[!is.na(tmp_List$parVal) ,]
  
  ### Extract moniLocation & moniParName from column "parName"
  splitDat <- strsplit(x = tmp_List$parName,split = "@")
  tmp_List$moniLocation <- sapply(splitDat, "[", 1)
  tmp_List$moniParName <- sapply(splitDat, "[", 2)
  
  return(tmp_List[,c(keyFields, 
                     #"parName", 
                     "moniLocation", 
                     "moniParName", 
                     "parVal")]) 
}

#' Monitoring: excel data import
#' 
#' @param xlsPath full path to excel file http://192.168.22.12/svn/kwb/DEMEAU/Work 
#' Areas/WA1 MAR/TracerSVH/datosTOT_StVicen+_jun2008-abr2009OK.xls", if FALSE 
#' already imported data object moniDat is loaded (Default: NULL) 
#' @param  metaTables  vector with names of tables with meta information 
#' (Default: "metadataTable", "commentTable")
#' @param moniTables vector with names of tables with monitoring data to be 
#' imported (Default: "bsv4", "bsvAll", "tuberia", "inflow")
#' @param ignoredParNames vector of parNames that are ignored for storing 
#' (Default:Temp_graphics_C, DiverBaro_graphFictive_cm,  DiverBaro_cm: syntetical 
#' or calculated parameters!)
#' @examples
#' ### xlsDir needs to be set correctly !!!!!
#' xlsDir <- "C:/Users/mrustl/Documents/WC_Server/DEMEAU/Work Areas/WA1 MAR/TracerSVH"
#' xlsFile <- "datosTOT_StVicen_jun2008-abr2009OK.xls"
#' xlsPath <- file.path(xlsDir, xlsFile)
#' ##importData(xlsPath=xlsPath)
#' 
#' #### Loading with stored moniDat.RData object
#' #importData()

importData <- function(xlsPath = NULL,
                       metaTables=c("metadataTable", 
                                    "commentTable"),
                       moniTables=c("bsv4",
                                    "bsvAll",
                                    "tuberia",
                                    "inflow"),
                       ignoredParNames=c("Temp_graphics_C",
                                         "DiverBaro_graphFictive_cm",
                                         "DiverBaro_cm")) {
  if (is.null(xlsPath)) {
    x <- load(system.file("extdata/monitoring/moniDat.RData", 
                     package = "kwb.demeau"))
    return(get(x))
  } else {
  tablesToImport <- c(metaTables, moniTables)
  tmpDat <- data.frame()
  for (table in tablesToImport)
  {
    cat(sprintf("Import table: %s ...", table))
    tbl <- kwb.db::hsGetTable(mdb=xlsPath, tbl = table)
    ### Ignore columns where moniParName in "ignoredParNames"
    ignoredParNames <- paste(ignoredParNames, collapse="|") 
    cat(sprintf("Import ignores the following columns: %s\n", ignoredParNames)) 
    columnsToImport <- colnames(tbl)[!grepl(ignoredParNames, colnames(tbl))]
    cat(sprintf("Imported columns: %s\n", paste(columnsToImport, collapse=","))) 
    tbl <- tbl[,columnsToImport]
    
    ### 2) Monitoring data: Convert to list form and add columns for monitoring
    if (!(table %in% metaTables))
    {
      cat("Convert to list & add moni columns....")
      tbl_Lst <- convertToListAndAddMoniColumns(df=tbl)
      tmpDat <- rbind(tmpDat, tbl_Lst)
    }
    cat("Done!\n\n")
  }
  cat("Creating list with monitoring data and comments...")
  resList <- list(timeSeries=tmpDat[tmpDat$moniParName!="CommentID",],
                  comments=tmpDat[tmpDat$moniParName=="CommentID", c("myDateTime",
                                                                     "moniLocation",
                                                                     "parVal"
                  )
                  ]
  )
  cat("Finished!\n\n")
  return(list(raw=resList))
  }
}

#' Monitoring: download meteo data
#' 
#' @param startYear 1.1.startYear (Default: 1.1.2008)
#' @param endYear 31.12.endYear (Default: 31.12.2014)

downloadMeteoData <- function (startYear = 2008,
                               endYear = 2014) {
  
  cat(sprintf("Request meteo data for years %d - %d\n\n", startYear, endYear))
  meteoData <- data.frame()
  for (year in startYear:endYear)
  {
    myUrl <- sprintf("http://www.wunderground.com/history/airport/LEBL/%s/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=%s&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1", year, year)
    cat(sprintf("Downloading data for year %d ....", year))
    x <- RCurl::getURL(myUrl)
    cat("Done!\n")
    
    # Remove "<br />" at the end of each line
    x <- gsub("<br />", "", x)
    
    meteoData <- rbind(meteoData, read.csv(textConnection(x)))
  }
  meteoData$CET <- as.Date(meteoData$CET, tz="CET")
  meteoData <- kwb.utils::hsRenameColumns(dframe = meteoData, 
                                          renames = list(CET="myDate"))
  return(meteoData)
}


#' Monitoring: convert data to matrix forma
#' 
#' @param df data frame with structure like: moniDat$agg$dailyMedian

listToMatrixForm <- function (df) {
  wide <- reshape(data = df,
                  timevar = "moniParName", 
                  idvar=c("myDate", "moniLocation"), 
                  drop=c("commentID", "Type" ), 
                  direction = "wide")
  colnames(wide) <- sub(pattern = "parVal.", 
                        replacement = "", 
                        x = colnames(wide)) 
  return(wide)
}

#' Helper function: rename values
#' 
#' @param df  data.frame to be modified
#' @param colName olName containing the values to be changed
#' @param oldVal old value
#' @param newVal new value

renameValues <- function(df,
                         ### data.frame to be modified
                         colName, 
                         ### colName containing the values to be changed, 
                         oldVal,
                         ### old value
                         newVal
                         ### new value
)
{
  df[df[,colName]==oldVal, colName] <- newVal
  return(df)
}

#' Helper function: filter monitoring data
#' 
#' @param locations (Default: NULL) 
#' @param locationsCol (Default: "moniLocation")
#' @param paras (Default: NULL)
#' @param paraCol (Default: "moniParName")
#' @param minDate (Default: "2009-03-02")
#' @param maxDate (Default: "2009-04-06")
#' @param df data.frame structure like: moniDat$agg$dailyMedian

filterMoniData <- function (locations=NULL,
                            locationsCol="moniLocation",
                            paras=NULL,
                            paraCol="moniParName",
                            minDate = "2009-03-02", 
                            maxDate = "2009-04-06",
                            df) {
  if (is.null(paras)) 
  {
    paras <- unique(df[,paraCol])  
  } 
  
  if (is.null(locations)) 
  {
    locations <- unique(df[,locationsCol])  
  } 
  
  condition <- df[,paraCol] %in% paras & df[,locationsCol] %in% locations & df$myDate >= minDate & df$myDate <= maxDate 
  
  return(df[condition, ])
}

#' Helper function: calculate water level change
#' 
#' @param refDate reference date for start (Default: "2009-03-02")
#' @param df data frame with structure like moniDat$agg$dailyMedian

calcWaterLevelChange <- function (refDate = "2009-03-02", 
                                  df) {
  undisturbed <- filterMoniData(paras = "DiverBaro_cm", 
                                df =  df,
                                minDate = refDate, 
                                maxDate = refDate)[,c(2,4)]
  undisturbed <- kwb.utils::hsRenameColumns(dframe = undisturbed, 
                                 renames = list(parVal="startVal"))
  
  allDiverBaro <- filterMoniData(paras = "DiverBaro_cm", df =  df)
  
  waterLevelChange <- merge(allDiverBaro, undisturbed, all.x=TRUE)
  waterLevelChange$DiverBaro_cm <- waterLevelChange$parVal 
  
  
  waterLevelChange <- renameValues(df =waterLevelChange,
                                   colName = "moniParName",
                                   oldVal = "DiverBaro_cm", 
                                   newVal="WaterLevelChange_cm"
  ) 
  
  waterLevelChange <- renameValues(df = waterLevelChange,
                                   colName = "Type",
                                   oldVal = "calc.", 
                                   newVal=sprintf("calc. (ref.date: %s)",
                                                  refDate)
  ) 
  
  waterLevelChange$parVal <- waterLevelChange$DiverBaro_cm - waterLevelChange$startVal
  
  return(rbind(df, waterLevelChange[, colnames(df)]))
}