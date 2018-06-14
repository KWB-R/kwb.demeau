#' Model: prepare model configuration
#' 
#' @param modelStructure  as retrieved by convRealCoordinatesToNodes()
#' @param pondTemp constant pond temperature (default: 20)
#' @param gwTempIni initial groundwater temperature (default: 12)
#' @param infRate infiltration rate per unit area (Default: 0.9514151 m/d)
#' @param depthToWaterTable water table below ground level (default: 6 m)
#' @param hydraulicGradient hydraulic gradient between left & right model boundary 
#' (Default: 0), if positive flow is from left to right, if negative from right 
#' to left
#' @param bnd list of structure list(temp=VALUE, ntx=VALUE, ntx=VALUE) passed to 
#' function defineHeadBoundary (i.e. boundary with seepage face), if bnd=NULL 
#' left/right boundaries are no-flow boundaries
#' @param hk hydraulic properties of soil as retrieved by kwb.vs2dh::vs2dh.ConfigureGenuchten()
#' @param ht transport properties of soil as retrieved by kwb.vs2dh::vs2dh.ConfigureTrans()
#' @param iniOutputTime automatically output results after 1 second of simulation 
#' @param minSimTime minimum simulation time in days (default: 0.5)
#' @param maxSimTime maximum simulation time in days (default: 31)
#' @param outputTimeStep  at which timestep are the results printed (default: 1 ), i.e. each day
#' @param solver general solver (Default: kwb.vs2dh::vs2dh.ConfigureBasicSolver())
#' @param rSolver recharge period solver (Default: kwb.vs2dh::vs2dh.ConfigureRechargePeriodSolver())
#' @seealso \code{defineHeadBoundary} for valid additional arguments
#' @return SVH model configuration
#' @examples
#' ### Importing GIS features
#' shp.dir <- system.file("extdata", "qgis", package="kwb.demeau")
#' shp.files <- dir(path = shp.dir, pattern = ".shp", full.names = TRUE)
#' gisData <- importShapefiles(shp.files)
#' ### Optionally remove some features 
#' gisData <- removeFeatures(gisData = gisData, ignoreFeatureIDs = c(3,20))
#' modelStructure <- convRealCoordinatesToNodes(gisData = gisData)

#' ### Model config
#' conf <- modelConfiguration(modelStructure = modelStructure)
#'                
#'### Running the configuration in VS2DH
#'res <- kwb.vs2dh::vs2di.runConfig(conf = conf,
#'                                  openTargetDir = TRUE)
#' 
#'### Plotting results
#' kwb.vs2dh::vs2dh.plotObservationPoints(paras = "TEMP", 
#'                                    paraLabel = "Temperature", 
#'                                    data=res$obsPoints)
#' kwb.vs2dh::vs2dh.plotVariables(para = "Temp", 
#'                                data = res$variables)  

modelConfiguration <- function(modelStructure,
                               pondTemp = 20,
                               gwTempIni = 12, 
                               infRate = 0.03,
                               depthToWaterTable = 6, 
                               hydraulicGradient = 0.001,
                               bnd = list(tmp = gwTempIni, ## default: equal to aquifer
                                          ntx = 4, ### default: specific head
                                          ntc = 1  ### default: transport boundary = TRUE
                               ),
                               hk = kwb.vs2dh::vs2dh.ConfigureGenuchten(ratioKzKh = 1,
                                                                        ss = 0,
                                                                        satKh = 750, ### m/d, reference: PREPARED report
                                                                        porosity = 0.2,
                                                                        alpha = 2.3,
                                                                        rmc = 0,
                                                                        beta = 5.8),
                               ht = kwb.vs2dh::vs2dh.ConfigureTrans(),
                               iniOutputTime = 1/(3600*24),
                               minSimTime = 0.5,
                               maxSimTime = 31, 
                               outputTimeStep = 1,
                               solver = kwb.vs2dh::vs2dh.ConfigureBasicSolver(),
                               rSolver = kwb.vs2dh::vs2dh.ConfigureRechargePeriodSolver()
){
  
  
  outputTimes <- c(iniOutputTime, seq(from = minSimTime, 
                                      to = maxSimTime, 
                                      by = outputTimeStep))
  
  ################################################################################
  #### 1) Basic configuration
  ################################################################################
  units <- kwb.vs2dh::vs2dh.ConfigureBasicUnits() ### default: m, day and "J" 
  time <- kwb.vs2dh::vs2dh.ConfigureBasicTime(tmax = maxSimTime)
  
  
  times <- kwb.vs2dh::vs2dh.ConfigureBasicOutputTimes(pltim = outputTimes)
  main <- kwb.vs2dh::vs2dh.ConfigureBasicOutputMain(f6p = FALSE, 
                                                    thpt = FALSE, 
                                                    spnt = FALSE, 
                                                    ppnt = FALSE, 
                                                    hpnt = FALSE, 
                                                    vpnt = FALSE)
  
  #### Define coordinates for observation points/wells, boundary fluxes 
  #### (derived form absolute x/y coordinates!): here some effort is required!
  
  is.obsWells <- modelStructure$features$shape.name == "ObservationWells"
  
  obsWells <- modelStructure$features[is.obsWells,]
  
  obsPoints <- kwb.vs2dh::vs2dh.ConfigureObsPoints(obs_n = obsWells$xNode,
                                                   obs_j = obsWells$yNode, 
                                                   outputEachTimeStep = FALSE)
  
  
  ##### Default should be for all boundaries set with kwb.vs2dh::vs2dh.ConfigureBoundaryCondition()
  ##### idbf should be corresponding to feature, e.g. infiltration pond, etc.
  boundaryFluxes <- kwb.vs2dh::vs2dh.ConfigureBoundaryFluxes()
  
  output <-  kwb.vs2dh::vs2dh.ConfigureBasicOutput(times = times, 
                                                   main = main,
                                                   obsPoints = obsPoints, 
                                                   boundaryFluxes = boundaryFluxes)
  
  
  grid <- kwb.vs2dh::vs2dh.ConfigureBasicGrid(nxr = modelStructure$grid$nxr, 
                                              nly = modelStructure$grid$nly,
                                              dx = modelStructure$grid$dx, 
                                              dy = modelStructure$grid$dy)
  
  basic <- kwb.vs2dh::vs2dh.ConfigureBasic(titl = "Heat tracer case study SVH",
                                           units = units,
                                           time = time, 
                                           output = output, 
                                           solver = solver, 
                                           grid = grid)
  
  
  ################################################################################
  #### 2) Soils configuration (only one soil!)
  ################################################################################
  
  soil1 <- kwb.vs2dh::vs2dh.ConfigureSoil(hk = hk, 
                                          ht = ht)
  
  
  soilGrid <-  setSoilZero(nodes = modelStructure$features, 
                           soilGrid =  kwb.vs2dh::vs2dh.ConfigureSoilGrid(ntex = 1,
                                                                          grid =  grid))
  
  soils <- kwb.vs2dh::vs2dh.ConfigureSoils(props = list(soil1),
                                           grid = soilGrid
  )
  
  ################################################################################
  #### 3) Initial conditions
  ################################################################################
  flow <- kwb.vs2dh::vs2dh.ConfigureInitialFlow(dwtx = abs(depthToWaterTable),
                                                hmin = -abs(depthToWaterTable))
  temp <- kwb.vs2dh::vs2dh.ConfigureInitialTemp(values = gwTempIni)
  
  initial <- kwb.vs2dh::vs2dh.ConfigureInitial(flow = flow, temp = temp)
  
  ################################################################################
  #### 4) Recharge periods 
  ################################################################################
  
  #### 1) set infiltration pond as boundary s"pecified flux" boundary
  is.infiltrationPond <- modelStructure$features$Name == "Infiltration pond"  
  
  pond <- modelStructure$features[is.infiltrationPond ,]
  
  rBoundary <-  kwb.vs2dh::vs2dh.ConfigureBoundaryCondition(jj = pond$yNode, 
                                                            nn = pond$xNode, 
                                                            ntx = rep(2,nrow(pond)),
                                                            pfdum = rep(infRate, nrow(pond)),
                                                            ntc = rep(1,nrow(pond)),
                                                            cf = rep(pondTemp,nrow(pond)))
  #### 2) define left & right fixed head boundaries
  if( !is.null(bnd)) {
    leftHead <- convHeadBoundary(leftHead = TRUE, 
                                 modelStructure,
                                 depthToWaterTable,
                                 hydraulicGradient)
    
    rightHead <- convHeadBoundary(leftHead = FALSE, 
                                  modelStructure,
                                  depthToWaterTable,
                                  hydraulicGradient)  
    
    
    leftHeadBoundary <- defineHeadBoundary(head = leftHead,
                                           temp =  bnd$tmp, 
                                           nly = modelStructure$grid$nly,
                                           ntx = bnd$ntx,
                                           ntc = bnd$ntc)
    
    rightHeadBoundary <- defineHeadBoundary(head = rightHead,
                                            temp =  bnd$tmp, 
                                            nly = modelStructure$grid$nly,
                                            ntx = bnd$ntx,
                                            ntc = bnd$ntc)
    
    headBoundaries <- rbind(leftHeadBoundary, rightHeadBoundary)
    rBoundary$boundary <- rbind(rBoundary$boundary,headBoundaries)
    
    
    #### 3) Set seepage faces
    seepLeft <- vs2dh.ConfigureSeepageFace(j = (leftHead$yNode - 1):2, 
                                           n = 2)
    seepRight <- vs2dh.ConfigureSeepageFace(j = (rightHead$yNode - 1):2, 
                                            n = modelStructure$grid$nxr - 1)
    
    seepage <- kwb.vs2dh::vs2dh.ConfigureSeepage(seepFaces = list(seepLeft,
                                                                  seepRight))
  } else {
    seepage <- kwb.vs2dh::vs2dh.ConfigureSeepage(seepFaces = list())
  }
  
  rOptions <- kwb.vs2dh::vs2dh.ConfigureRechargePeriodOptions(seepage = seepage)
  
  
  #### only one recharge periods (extension to multiple: additional effort for 
  #### modifying "tper" and "boundary" conditions)
  rechargePeriod <- kwb.vs2dh::vs2dh.ConfigureRechargePeriod(tper = maxSimTime,
                                                             options =  rOptions, 
                                                             solver = rSolver, 
                                                             boundary = rBoundary )
  
  recharge <- kwb.vs2dh::vs2dh.ConfigureRechargePeriods(periods = list(rechargePeriod))
  
  ################################################################################
  #### 5) Building the configuration
  ################################################################################
  conf <- kwb.vs2dh::vs2dh.Configure(basic = basic, 
                                     soils = soils, 
                                     initial = initial, 
                                     recharge = recharge)
  
  #cat(kwb.vs2dh::vs2dh.writeConfig(conf))
  return(conf)
}
#' Prepare model: wrapper for modelConfiguration()

#' @param gisData as retrieved by kwb.demeau::importShapefiles()
#' @param type either "heat" (for heat model VS2DH) or "solute" (for solute transport 
#' model VSDT), Default: "heat"
#' @param rech_pondInfRate recharge rate of infiltration pond during model simulation 
#' in meter / day (Default: 1 m/d)
#' @param rech_pondTemp median pond temperature during recharge period (Default: 
#' 13.7 C during whole simulation time, i.e. 30 days)
#' @param rech_pondConc median sunbstance concentration in pond during recharge 
#' period (Default: 1 during whole simulation time, i.e. 30 days)
#' @param init_gwTemp initial groundwater temperature before infiltration (Default: 
#' 19 C)
#' @param init_gwConc initial substance concentration in GW before infiltration 
#' (Default: 0)
#' @param init_depthToWaterTable initial depth to groundwater table (Default: 6 m,
#' assumption, no data)
#' @param init_hydrGrad hydraulic gradient between left & right model boundary,
#' if positive flow is from left to right, if negative from right to left (Default: 
#' 0.001)
#' @param grid_dx model grid spacing in horizontal direction (Default: 1 m), 
#' @param grid_dy model grid spacing in vertical direction (Default: 1 m), 
#' @param grid_yMax maximum vertical model extent in meters (Default: NULL, i.e. 
#' maximum filter screen depth below ground level is set as maximum vertical model 
#' extent)
#' @param flow_ratioKzKh Ratio of hydraulic conductivity in the z-coordinate 
#' direction to that in the x-coordinate direction (Default: 0.01) 
#' @param flow_satKh saturated hydraulic conductivity (Default: 450 m / day), 
#' @param flow_ss  Specific storage (Ss), L^-1. (Default: 0)
#' @param flow_neff effective porosity (Default: 0.2), sum of effective porosity and 
#' residual moisture content (rmc) are equal to parameter "porosity" used for van
#' Genuchten model 
#' @param flow_rmc residual moisture content (Default: 0.05), sum of effective 
#' porosity and residual moisture content (rmc) are equal to parameter "porosity"
#' used for van Genuchten model 
#' @param flow_alpha van Genuchten alpha. NOTE: alpha is as defined by van Genuchten 
#' (1980) and is the negative reciprocal of alpha' used in earlier versions (prior 
#' to version 3.0) of VS2DT, L. (Default: 2) 
#' @param flow_beta van Genuchten parameter, beta' in Healy (1990) and Lappala 
#' and others (1987), (Default: 5) 
#' @param heat_alphaL Longitudinal dispersion (Default: 1 m), 
#' @param heat_alphaT Transversal dispersion (Default: 0.1 m)
#' @param heat_cs Heat capacity of dry solids (Cs), Q/L3 C. (Default: 2180000.0 J/m3C)
#' @param heat_ktRmc  Thermal conductivity of water sediment at residual moisture 
#' content, Q/LTC. (Default: 129600)
#' @param heat_ktSat Thermal conductivity of water sediments at full saturation, 
#' Q/LC. (Default: 155520) 
#' @param heat_cw Heat capacity of water (Cw), which is the product of density times
#' specific heat of water, Q/L3 C. (default: 4180000.0)
#' @param solu_alphaL Longitudinal dispersion (Default: 1 m), 
#' @param solu_alphaT Transversal dispersion (Default: 0.1 m)
#' @param solu_molDiffCoeff Molecular diffusion coefficient, Dm, L2/T. (Default: 
#' ( 4.5e-05*3600*24/100 m2/day, ### http://en.wikipedia.org/wiki/Mass_diffusivity#Liquids 
#' for hydrogen - water at 25 C)
#' @param solu_decayConst Decay constant, l, T-1. (Default: 0)
#' @param solu_bulkDensity  Bulk density, Dm (set to 0 for no adsorption
#' or ion exchange) M/L3. (Default: 0) 
#' @param solu_adsorp  0 for no adsorption or ion exchange; Kd for linear 
#' adsorption isotherm; K1 for Langmuir isotherm; Kf for Freundlich isotherm; or
#' Km for ion exchange. (Default: 0)
#' @param time_firstOutput first output after x time (default: after 1 second), 
#' (-> proxy for initial setting)
#' @param time_minSimTime start of regular output (Default: after 0.5 days)
#' @param time_maxSimTime end of regular output & model simulation (Default: 30.5 
#' days)
#' @param time_outputTimeStep time interval in days for which model results are 
#' written to output file (Default: 1 day)
#' @param solv_cis If TRUE spatial discretisation is realised by centered-in-space 
#' differencing; if FALSE backward-in-space differencing is to be used for transport 
#' equation. (default: TRUE) 
#' @param solv_cit If TRUE temporal discretisation is realised by centered-in-time 
#' differencing; if FALSE backward-in-time or fully implicit differencing is to 
#' be used. (default: TRUE)
#' @param solv_numt Maximum number of time steps.(default: 5000). (NOTE: if enhanced 
#' precision in print out to file "balance.out" and file 11 "obsPoints.out", is 
#' desired set NUMT equal to a negative number. That is, multiply actual maximum 
#' number of time steps by -1)1
#' @param solv_minit Minimum number of iterations per time step. (default: 2)
#' @param solv_itmax Maximum number of iterations per time step. (default: 300)
#' @param solv_eps	 Head closure criterion for iterative solution of flow equation, L. 
#' (default: 0.001)
#' @param solv_eps1 Temperature closure criterion for iterative solution of transport 
#' equation, C. (default: 0.001)
#' @param solv_eps2 Velocity closure criterion for outer iteration loop at each time 
#' step, L/T. (default: 0.001)
#' @param solv_hmax Relaxation parameter for iterative solution. See discussion in 
#' Lappala and others (1987) for more detail. Value is generally in the range of 
#' 0.4 to 1.2. (default: 0.7)
#' @param solv_itstop If TRUE simulation is terminated after ITMAX iterations in 
#' one time step; otherwise = F. (default: FALSE) 
#' @param solr_delt Length of initial time step for this period, T. (default: 1.0E-6)
#' @param solr_tmlt Multiplier for time step length. (default: 1.2)
#' @param solr_dltmx	Maximum allowed length of time step, T. (default: 1)
#' @param solr_dltmin Minimum allowed length of time step, T. (default: 1.0E-6)
#' @param solr_tred Factor by which time-step length is reduced if convergence is
#' not obtained in ITMAX iterations. Values usually should be in the range 0.1 to 
#' 0.5. If no reduction of time-step length is desired, input a value of 0.0. 
#' (default: 0.1)
#' @param solr_dsmax	Maximum allowed change in head per time step for this period, L. 
#' (default: 10) 
#' @param solr_sterr Steady-state head criterion; when the maximum change in head 
#' between successive time steps is less than STERR, the program assumes that steady 
#' state has been reached for this period and advances to next recharge period, L. 
#' (default: 0)
#' @param solr_pond Maximum allowed height of ponded water for constant flux nodes. 
#' See Lappala and other (1987) for detailed discussion of POND, L. (default: 0)
#' @return Prepared SVH model configuration
#' @examples
#' ### Importing GIS features
#' shp.dir <- system.file("extdata", "qgis", package="kwb.demeau")
#' shp.files <- dir(path = shp.dir, pattern = ".shp", full.names = TRUE)
#' gisData <- importShapefiles(shp.files)
#' ### Optionally remove some features 
#' gisData <- removeFeatures(gisData = gisData, ignoreFeatureIDs = 20)
#' #### 1) Prepare heat 
#' preparedHeatModel  <- prepareModel(gisData = gisData, type = "heat")
#' #' #### 1) Prepare solute transport model  
#' preparedSoluteModel  <- prepareModel(gisData = gisData, type = "solu") 
prepareModel <- function(gisData, 
                         type = "heat", 
                         rech_pondInfRate = 1, #infRate_perUnitArea
                         rech_pondTemp = 13.7, ##pondTempMedian
                         rech_pondConc = 1, ##substance concentration in pond 
                         init_gwTemp = 19, # 19 C
                         init_gwConc = 0, ### substance concentration in GW
                         init_depthToWaterTable = 6, #m assumption, no data
                         init_hydrGrad = 0.001, 
                         grid_dx = 1, 
                         grid_dy = 1, 
                         grid_yMax = NULL,
                         flow_ratioKzKh = 0.01, 
                         flow_satKh = 450, 
                         flow_ss = 0, 
                         flow_neff = 0.2, 
                         flow_rmc = 0.05, 
                         flow_alpha = 2, 
                         flow_beta = 5, 
                         heat_alphaL = 1, 
                         heat_alphaT = 0.1,
                         heat_cs = 2180000,
                         heat_ktRmc = 129600, 
                         heat_ktSat = 155520, 
                         heat_cw = 4180000,
                         solu_alphaL = 1, 
                         solu_alphaT = 0.1,
                         solu_molDiffCoeff = 4.5e-05*3600*24/100, ### m2/day http://en.wikipedia.org/wiki/Mass_diffusivity#Liquids
                         solu_decayConst = 0,
                         solu_bulkDensity = 0, 
                         solu_adsorp = 0,
                         time_firstOutput = 1/(3600*24), ## first output after 1 second (proxy for initial setting)
                         time_minSimTime = 0.5, # days
                         time_maxSimTime = 30.5, # days
                         time_outputTimeStep = 1, # every 24 hours
                         solv_cis = TRUE, 
                         solv_cit = TRUE, 
                         solv_numt = 5000,
                         solv_minit = 2, 
                         solv_itmax = 300, 
                         solv_eps = 0.001, 
                         solv_eps1 = 0.001, 
                         solv_eps2 = 0.001,
                         solv_hmax = 0.7, 
                         solv_itstop = FALSE,
                         solr_delt = 1e-06,
                         solr_tmlt = 1.2, 
                         solr_dltmx = 1,
                         solr_dltmin = 1e-06, 
                         solr_tred = 0.5, 
                         solr_dsmax = 10, 
                         solr_sterr = 0, 
                         solr_pond = 0) {
  
  rech_pond_SelPara <- list()
  init_gw_selPara <- list()
  type_selPara <- list()
  
  if (type == "solu") {
    rech_pond_SelPara <- list(rech_pondConc = rech_pondConc)
    
    init_gw_selPara <- list(init_gwConc = init_gwConc)
    
    type_selPara <- list(solu_alphaL = solu_alphaL, 
                         solu_alphaT = solu_alphaT,
                         solu_molDiffCoeff = solu_molDiffCoeff,
                         solu_decayConst = solu_decayConst,
                         solu_bulkDensity = solu_bulkDensity, 
                         solu_adsorp = solu_adsorp) } 
  else if (type == "heat") {
    rech_pond_SelPara <- list(rech_pondTemp = rech_pondTemp)
    
    init_gw_selPara <- list(init_gwTemp = init_gwTemp)
    
    type_selPara <- list(heat_alphaL = heat_alphaL, 
                         heat_alphaT = heat_alphaT,
                         heat_cs = heat_cs,
                         heat_ktRmc = heat_ktRmc, 
                         heat_ktSat = heat_ktSat, 
                         heat_cw = heat_cw) } 
  else {
    stop("Please enter valid option for parameter 'type': 'heat' (for heat model) or 'solu' (for solute transport model)")
  }
  
  
  paras <- list(gisData = gisData, 
                type = type, 
                rech_pondInfRate = rech_pondInfRate,
                init_depthToWaterTable = init_depthToWaterTable, 
                init_hydrGrad = init_hydrGrad, 
                grid_dx = grid_dx, 
                grid_dy = grid_dy, 
                grid_yMax = grid_yMax, 
                flow_ratioKzKh = flow_ratioKzKh, 
                flow_satKh = flow_satKh, 
                flow_neff = flow_neff, 
                flow_rmc = flow_rmc, 
                flow_porosity = flow_neff + flow_rmc,
                flow_alpha = flow_alpha, 
                flow_beta = flow_beta,
                time_firstOutput = time_firstOutput,
                time_minSimTime = time_minSimTime,
                time_maxSimTime = time_maxSimTime, 
                time_outputTimeStep = time_outputTimeStep,
                solv_cis = solv_cis, 
                solv_cit =  solv_cit, 
                solv_numt = solv_numt,
                solv_minit = solv_minit, 
                solv_itmax = solv_itmax, 
                solv_eps = solv_eps, 
                solv_eps1 = solv_eps1, 
                solv_eps2 = solv_eps2,
                solv_hmax = solv_hmax, 
                solv_itstop = solv_itstop,
                solr_delt = solr_delt,
                solr_tmlt =  solr_tmlt, 
                solr_dltmx = solr_dltmx,
                solr_dltmin = solr_dltmin, 
                solr_tred = solr_tred, 
                solr_dsmax = solr_dsmax, 
                solr_sterr = solr_sterr, 
                solr_pond = solr_pond)
  
  paras <-  append(paras, c(rech_pond_SelPara, 
                            init_gw_selPara,  
                            type_selPara))
  
  modelStructure <- convRealCoordinatesToNodes(gisData = gisData,
                                               dx = grid_dx, #m 
                                               dy = grid_dy, #m
                                               y = grid_yMax ### maxium vertical model extent
  )
  
  conf <- modelConfiguration(modelStructure = modelStructure,                      
                             pondTemp = rech_pond_SelPara[[1]], 
                             gwTempIni = init_gw_selPara[[1]], 
                             depthToWaterTable = init_depthToWaterTable ,
                             hydraulicGradient = init_hydrGrad, # assumption, no data
                             bnd = list(tmp = init_gw_selPara[[1]], ## default: equal to gwTempIni
                                        ntx = 4, ### default: specific head
                                        ntc = 0),  ### default: transport boundary = 1
                             #                           bnd = NULL,  ### no-flow boundary
                             hk = kwb.vs2dh::vs2dh.ConfigureGenuchten(ratioKzKh = flow_ratioKzKh, 
                                                                      satKh = flow_satKh, ## from report
                                                                      ss = flow_ss, 
                                                                      porosity = paras$flow_porosity,
                                                                      alpha = flow_alpha, ##assumption, no data
                                                                      rmc = flow_rmc, 
                                                                      beta = flow_beta ##assumption, no data
                             ),
                             ht = kwb.vs2dh::vs2dh.ConfigureTrans(alphaL = type_selPara[[1]], 
                                                                  alphaT = type_selPara[[2]],
                                                                  cs = type_selPara[[3]],
                                                                  ktRmc = type_selPara[[4]], 
                                                                  ktSat = type_selPara[[5]], 
                                                                  cw = type_selPara[[6]]),
                             infRate = rech_pondInfRate, 
                             iniOutputTime = time_firstOutput, ## first output after 1 second (proxy for initial setting)
                             minSimTime = time_minSimTime, # days
                             maxSimTime = time_maxSimTime, # days
                             outputTimeStep = time_outputTimeStep, # every 24 hours
                             
                             solver = kwb.vs2dh::vs2dh.ConfigureBasicSolver(itstop = solv_itstop, 
                                                                            hmax = solv_hmax,
                                                                            itmax = solv_itmax, 
                                                                            eps = solv_eps, 
                                                                            eps1 = solv_eps1, 
                                                                            eps2 = solv_eps2, 
                                                                            numt = solv_numt),
                             rSolver = kwb.vs2dh::vs2dh.ConfigureRechargePeriodSolver(delt = solr_delt,
                                                                                      tmlt = solr_tmlt, 
                                                                                      dltmx = solr_dltmx,
                                                                                      dltmin = solr_dltmin, 
                                                                                      tred = solr_tred, 
                                                                                      dsmax = solr_dsmax, 
                                                                                      sterr = solr_sterr, 
                                                                                      pond = solr_pond))
  
  
  return(list(paras = paras,
              modelStructure = modelStructure,
              conf = conf))
}


#' Run heat model: wrapper for vs2di.runConfig()
#' @param preparedHeatModel prepared heat model as retrieved by  prepareModel(type="heat")
#' @param tDir target directory where vs2dh model input/output files should be 
#' stored. (Default: tempdir())
#' @param openTargetDir If TRUE: model directory with heat model results will be
#' opened in Windows explorer (Default: FALSE)
#' @param ... additional arguements passed to function kwb.vs2dh::vs2di.runConfig()
#' @return List with heat model results ($res) and configuration

runHeatModel <- function(preparedHeatModel,
                         tDir = tempdir(),
                         openTargetDir = FALSE,
                         ...) {
  
  ##Running the model
  res <- kwb.vs2dh::vs2di.runConfig(conf = preparedHeatModel$conf,
                         engine = "vs2dh",
                         tDir = tDir,
                         openTargetDir = openTargetDir,
                         ...)
  
  ### Calculate flow velocity 
  res$obsPoints$V <- sqrt(res$obsPoints$VX ^ 2 + res$obsPoints$VZ ^ 2)
  
  return(list(prepared = preparedHeatModel, 
              res = res))
}


#' Run solute transport model: wrapper for vs2di.runConfig()
#' @param preparedSoluteModel prepared solute transport model as retrieved by  prepareModel(type="solu")
#' @param tDir target directory where vs2dh model input/output files should be 
#' stored. (Default: tempdir())
#' @param returnOutput  Default: TRUE
#' @param openTargetDir If TRUE: model directory with heat model results will be
#' opened in Windows explorer (Default: FALSE)
#' @param ... additional arguements passed to function kwb.vs2dh::vs2di.runConfig()
#' @return List with heat model results ($res) and configuration

runSoluteModel <- function(preparedSoluteModel,
                              tDir = tempdir(),
                              returnOutput = TRUE,
                         openTargetDir = FALSE,
                         ...) {
  
  ##Running the model
  res <- kwb.vs2dh::vs2di.runConfig(conf = preparedSoluteModel$conf,
                                    engine = "vs2dt",
                                    tDir = tDir,
                                    returnOutput = returnOutput, 
                                    openTargetDir = openTargetDir,
                         ...)
  
  ### Calculate flow velocity 
  res$obsPoints$V <- sqrt(res$obsPoints$VX ^ 2 + res$obsPoints$VZ ^ 2)
  
  return(list(prepared = preparedSoluteModel, 
              res = res))
}


