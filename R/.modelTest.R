library(kwb.demeau)

################################################################################
# 1) Step: Import Data
## From Excel file (takes a while)
# xlsDir <- system.file("extdata", 
#                       "monitoring", 
#                       package = "kwb.demeau")
# 
# xlsPath <- file.path(xlsDir, "moniDat.xls")
#rawData <- importData(xlsPath = xlsPath)
#save(rawData,file = "C:/Users/mrustl/Documents/WC_Server/R_Development/trunk/RPackages/kwb.demeau/inst/extdata/monitoring/moniDat.RData")
# From R object (if Excel was already imported)
rawData <- importData()

################################################################################
# 2) Step: Data processing (aggregate to median daily values)
moniDat <- processingData(rawData = rawData)

# Infiltration pond area (m2)
shp.dir <- system.file("extdata", "qgis", package = "kwb.demeau")
shp.files <- dir(path = shp.dir, pattern = ".shp", full.names = TRUE)
gisData <- importShapefiles(shp.files)
ponds <- getFeatures(gisData,addColNames = "Area")
infiltrationPond_Area <- ponds$Area[ponds$Name == "Infiltration pond"] 


infPeriod <- filterMoniData(minDate = "2009-03-03", 
                            maxDate = "2009-04-02", 
                            df = moniDat$agg$dailyMedian)

infPeriod$TIME_day <- as.numeric(difftime(infPeriod$myDate,min(infPeriod$myDate) - 0.5,
                                          units = "days"))

iniAmbientTemp <- filterMoniData(paras = "Temp_C",
                                 minDate = "2009-02-28", 
                                 maxDate = "2009-03-02", 
                                 df = moniDat$agg$dailyMedian)

### Median GW temperature before infiltration 
#### (important note: ignored BSV-1 has 12 C !!! -> permanent infiltration????)
iniAmbientTempMedian <- stats::aggregate(
  stats::as.formula("parVal ~ moniLocation"), 
  data = iniAmbientTemp,
  FUN = stats::median
)

condition <- !iniAmbientTempMedian$moniLocation %in% c("Tuberia", "BSV-2_BARO65699", "BSV-1")
iniAmbientGwTempMedian <- stats::median(iniAmbientTempMedian[condition,]$parVal)

print(sprintf("Initial GW temperature before infiltration: %2.3f C", iniAmbientGwTempMedian))

### Median pond temperature at Tuberia during infiltration period
pondTemp <- infPeriod[infPeriod$moniParName == "Temp_C" & infPeriod$moniLocation == "Tuberia",]
pondTempMedian <- stats::median(pondTemp$parVal)

print(sprintf("Median pond temperature during infiltration: %2.3f C", pondTempMedian))

# Median daily inflow (m3/d) into infiltration pond between 2009-03-03 and 
# 2009-04-02
inflow_cbmPerHour <- infPeriod$parVal[infPeriod$moniParName == "Inflow_cbmPerH"]

medianInflow_cbmPerDay <- stats::median(inflow_cbmPerHour)*24

print(sprintf("Median inflow to infiltration pond: %2.1f m3/h (%4.1f m3/day)",
              stats::median(inflow_cbmPerHour), 
              medianInflow_cbmPerDay))

### Infiltration rate per unit area (m/d)
infRate_perUnitArea <- medianInflow_cbmPerDay/infiltrationPond_Area
print(sprintf("Infiltration rate per unit pond area: %1.3f m/d",infRate_perUnitArea))

################################################################################
# 3) Modelling
################################################################################

### Importing GIS features
shp.dir <- system.file("extdata", "qgis", package = "kwb.demeau")
shp.files <- dir(path = shp.dir, pattern = ".shp", full.names = TRUE)
gisData <- importShapefiles(shp.files)
### Optionally remove some features 
#getFeatures(gisData)
gisData <- removeFeatures(gisData = gisData, 
                          ignoreFeatureIDs = c(20, ## id 20: settling basin
                                                3))  ## id 3: BSV-1

### Heat model calibration 
#### 1) Prepare
preparedHeatModel <- prepareModel(gisData = gisData,
                                      type = "heat",
                                      init_hydrGrad = 0.006,
                                      rech_pondInfRate = infRate_perUnitArea,
                                      rech_pondTemp = pondTempMedian, 
                                      init_gwTemp = iniAmbientGwTempMedian,
                                      flow_satKh = 440.4667,
                                      flow_ratioKzKh = 0.01)

### 2) Calibrate 

# caliTemp <- calibrateModel(preparedHeatModel = preparedHeatModel, 
#                            moniDat = moniDat,
#                            obsPoints = "BSV-5|BSV-6_2|BSV-6_3|BSV-3",
#                            #obsPoints = "*",  # all obsPoints
#                            parameterName = "ratioKzKh",
#                            parameterRange = c(0.005, 0.05),
#                            objState = "temp", 
#                            objCrit = "RMSE")

#caliTemp$optimalParameterValue
#caliTemp$minimalPerformanceValue

### 3) Run best fit 

calibratedHeatModel <- prepareModel(gisData = gisData,
                                  type = "heat",
                                  init_hydrGrad = 0.011,
                                  rech_pondInfRate = infRate_perUnitArea,
                                  rech_pondTemp = pondTempMedian, 
                                  init_gwTemp = iniAmbientGwTempMedian,
                                  flow_satKh = 440.4667,
                                  flow_ratioKzKh = 0.01)

heatModel <- runHeatModel(preparedHeatModel = calibratedHeatModel)

heatModelParas <- data.frame(unlist(heatModel$prepared$paras[3:length(heatModel$prepared$paras)]))
names(heatModelParas)[1] <- "parVal"

### 3.1) Compare 
heatModelCompare <- compareModelledMeasured(heatModel = heatModel, 
                                            moniDat = moniDat)

### 3.2) Plot goodness of fit (for water level change & temperature)
objCrit <- c("RMSE", "R2", "PBIAS")

fitnessWaterLevelChange <- fitnessWithLabel(heatModel = heatModel, 
                 moniDat = moniDat,
                 objState = "waterLevelChange", 
                 objCrit = objCrit,
                 plot.type=c("l"),
                 cex.label = 2,
                 main = "",
                 performance.in.label = FALSE
                 )

fitnessTemp <- fitnessWithLabel(heatModel = heatModel, 
                 moniDat = moniDat,
                 objState = "temp", 
                 objCrit = objCrit,
                 cex.label = 2,
                 main = "",
                 performance.in.label = FALSE)


### Addon: Optional additional analyse & Check results
if (FALSE) {
  ##Model features
  plotModelStructure(df = heatModel$prepared$modelStructure$features)
  
  ####Spatio-temporal pressure head distribution
  kwb.vs2dh::vs2dh.plotVariables(para = "PressureHead",
                                 data = heatModel$res$variables) 
  
  ####Spatio-temporal temperature distribution
  kwb.vs2dh::vs2dh.plotVariables(para = "Temp", 
                                 data = heatModel$res$variables)  
  
  ####Spatio-temporal temperature distribution at observation points
  kwb.vs2dh::vs2dh.plotObservationPoints(paras = "TEMP", 
                                         paraLabel = "Temperature", 
                                         data = heatModel$res$obsPoints)
  
  ####Spatio-temporal flow velocity distribution at observation points
  kwb.vs2dh::vs2dh.plotObservationPoints(paras = "V", 
                                         paraLabel = "Velocity (m / d)", 
                                         data = heatModel$res$obsPoints)
  
  ### 1) Flow velocity (meter / day) in observation points/wells 
  # pdf(file = "flow.pdf", width = 29.7/2.54, height = 21/2.54)
  # for (obsWell in unique(modelled$Name)) {
  #   print(lattice::xyplot(V ~ TIME_day | as.factor(sprintf("%2.1f m", Z_m.x)), 
  #                         type = "p", 
  #                         pch = 16, 
  #                         auto.key = TRUE, 
  #                         data = modelled[modelled$Name == obsWell, ], 
  #                         ylab = "Flow velocity (meter / day)", 
  #                         main = sprintf("Observation well %s", obsWell)))
  # }
  # dev.off()
  # 
  # modVelocity <- selectModelled(modDf = modelled, parName = "V")
  # lattice::bwplot(modelled ~ Name, 
  #                 data = modVelocity, 
  #                 ylab = "Flow velocity (meter / day)")
  # lattice::xyplot(modelled ~ TIME_day | Name, 
  #                 data = modVelocity, 
  #                 ylab = "Flow velocity (meter / day)", 
  #                 xlab = "Time")
  
}

### Solute transport model  
#### 1) Prepare
preparedSoluteModel <- prepareModel(gisData = gisData,
                                       type = "solu",
                                       rech_pondInfRate = infRate_perUnitArea,
                                       rech_pondTemp = pondTempMedian, 
                                       init_gwTemp = iniAmbientGwTempMedian,
                                       time_outputTimeStep = 0.1,
                                       time_minSimTime = 0.1, 
                                       time_maxSimTime = 30.5,
                                       flow_satKh = 440.4667,
                                       flow_ratioKzKh = 0.01)


## 2) Run 
soluteModel <- runSoluteModel(preparedSoluteModel = preparedSoluteModel)

soluteModelParas <- data.frame(unlist(soluteModel$prepared$paras[3:length(soluteModel$prepared$paras)]))
names(soluteModelParas)[1] <- "parVal"

### 3) Plot
solute <- soluteModelled(soluteModel = soluteModel,offset = 0.01)
solute$domeTimes$agg

domTravelTimes <- solute$domeTimes$agg
domTravelTimes$maxConc <- round(domTravelTimes$maxConc * 100, 0)
domTravelTimes$Name <- as.character(domTravelTimes$Name)

domTravelTimes <- domTravelTimes[,c("Name","TIME_day", "maxConc" )] %>%  plyr::rename(c(Name = 'MoniWellId', 
                                   TIME_day = "Dominant travel time (days)",
                                   maxConc = "Share of infiltrate (%)"))

exportCSV <- function(objects =  c("fitnessTemp", 
                                    "fitnessWaterLevelChange", 
                                    "domTravelTimes", 
                                    "heatModelParas",
                                    "soluteModelParas"),
                       tDir = tempdir(),
                      openDir = TRUE) {
for (object in objects) {
  write.csv(x = get(object),file = file.path(tDir, paste0(object,".csv")))
if (openDir) kwb.utils::hsOpenWindowsExplorer(tDir)
}
}
exportCSV()
save.image(file = "demeau.RData")
#save(fitnessTemp, fitnessWaterLevelChange, domTravelTimes, file = "tab1.Rdata")
ylabel <- "Dominant infiltration pond share (in %)"
xlabel <- "Dominant travel time (in days)"
par(oma = c(0,2,0,0))
barplot(height = solute$domeTimes$agg$TIME_day,
        names.arg = solute$domeTimes$agg$Label, horiz = TRUE, las = 1, 
        xlab = xlabel)
# plot(modelled*100 ~ TIME_day, data = domTimes$agg, 
#      pch = 16, 
#      col = "blue", 
#      las = 1,
#      xlim = c(0,14),
#      ylim = c(44,52),
#      ylab = ylabel,
#      xlab = xlabel)
# text(x = domTimes$agg$TIME_day - 0.5, 
#      y=domTimes$agg$modelled*100 + 0.2, 
#      labels = domTimes$agg$Name)
# 
# lattice::xyplot(modelled*100 ~ TIME_day, groups=as.character(Name), data = domTimes$agg, 
#      pch = 16, 
#      auto.key = TRUE,
#      las = 1,
#      ylab = ylabel,
#      xlab = xlabel)

####Spatio-temporal temperature distribution at observation points
kwb.vs2dh::vs2dh.plotObservationPoints(paras = "CONC", 
                                       paraLabel = "Concentration", 
                                       data = soluteModel$res$obsPoints)

kwb.vs2dh::vs2dh.plotVariables(para = "Temp", 
                               data = soluteModel$res$variables, 
                               main = "Share of infiltration")  
