% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{prepareModel}
\alias{prepareModel}
\title{Prepare model: wrapper for modelConfiguration()}
\usage{
prepareModel(gisData, type = "heat", rech_pondInfRate = 1,
  rech_pondTemp = 13.7, rech_pondConc = 1, init_gwTemp = 19,
  init_gwConc = 0, init_depthToWaterTable = 6, init_hydrGrad = 0.001,
  grid_dx = 1, grid_dy = 1, grid_yMax = NULL,
  flow_ratioKzKh = 0.01, flow_satKh = 450, flow_ss = 0,
  flow_neff = 0.2, flow_rmc = 0.05, flow_alpha = 2, flow_beta = 5,
  heat_alphaL = 1, heat_alphaT = 0.1, heat_cs = 2180000,
  heat_ktRmc = 129600, heat_ktSat = 155520, heat_cw = 4180000,
  solu_alphaL = 1, solu_alphaT = 0.1, solu_molDiffCoeff = 4.5e-05 *
  3600 * 24/100, solu_decayConst = 0, solu_bulkDensity = 0,
  solu_adsorp = 0, time_firstOutput = 1/(3600 * 24),
  time_minSimTime = 0.5, time_maxSimTime = 30.5,
  time_outputTimeStep = 1, solv_cis = TRUE, solv_cit = TRUE,
  solv_numt = 5000, solv_minit = 2, solv_itmax = 300,
  solv_eps = 0.001, solv_eps1 = 0.001, solv_eps2 = 0.001,
  solv_hmax = 0.7, solv_itstop = FALSE, solr_delt = 1e-06,
  solr_tmlt = 1.2, solr_dltmx = 1, solr_dltmin = 1e-06,
  solr_tred = 0.5, solr_dsmax = 10, solr_sterr = 0, solr_pond = 0)
}
\arguments{
\item{gisData}{as retrieved by kwb.demeau::importShapefiles()}

\item{type}{either "heat" (for heat model VS2DH) or "solute" (for solute transport 
model VSDT), Default: "heat"}

\item{rech_pondInfRate}{recharge rate of infiltration pond during model simulation 
in meter / day (Default: 1 m/d)}

\item{rech_pondTemp}{median pond temperature during recharge period (Default: 
13.7 C during whole simulation time, i.e. 30 days)}

\item{rech_pondConc}{median sunbstance concentration in pond during recharge 
period (Default: 1 during whole simulation time, i.e. 30 days)}

\item{init_gwTemp}{initial groundwater temperature before infiltration (Default: 
19 C)}

\item{init_gwConc}{initial substance concentration in GW before infiltration 
(Default: 0)}

\item{init_depthToWaterTable}{initial depth to groundwater table (Default: 6 m,
assumption, no data)}

\item{init_hydrGrad}{hydraulic gradient between left & right model boundary,
if positive flow is from left to right, if negative from right to left (Default: 
0.001)}

\item{grid_dx}{model grid spacing in horizontal direction (Default: 1 m),}

\item{grid_dy}{model grid spacing in vertical direction (Default: 1 m),}

\item{grid_yMax}{maximum vertical model extent in meters (Default: NULL, i.e. 
maximum filter screen depth below ground level is set as maximum vertical model 
extent)}

\item{flow_ratioKzKh}{Ratio of hydraulic conductivity in the z-coordinate 
direction to that in the x-coordinate direction (Default: 0.01)}

\item{flow_satKh}{saturated hydraulic conductivity (Default: 450 m / day),}

\item{flow_ss}{Specific storage (Ss), L^-1. (Default: 0)}

\item{flow_neff}{effective porosity (Default: 0.2), sum of effective porosity and 
residual moisture content (rmc) are equal to parameter "porosity" used for van
Genuchten model}

\item{flow_rmc}{residual moisture content (Default: 0.05), sum of effective 
porosity and residual moisture content (rmc) are equal to parameter "porosity"
used for van Genuchten model}

\item{flow_alpha}{van Genuchten alpha. NOTE: alpha is as defined by van Genuchten 
(1980) and is the negative reciprocal of alpha' used in earlier versions (prior 
to version 3.0) of VS2DT, L. (Default: 2)}

\item{flow_beta}{van Genuchten parameter, beta' in Healy (1990) and Lappala 
and others (1987), (Default: 5)}

\item{heat_alphaL}{Longitudinal dispersion (Default: 1 m),}

\item{heat_alphaT}{Transversal dispersion (Default: 0.1 m)}

\item{heat_cs}{Heat capacity of dry solids (Cs), Q/L3 C. (Default: 2180000.0 J/m3C)}

\item{heat_ktRmc}{Thermal conductivity of water sediment at residual moisture 
content, Q/LTC. (Default: 129600)}

\item{heat_ktSat}{Thermal conductivity of water sediments at full saturation, 
Q/LC. (Default: 155520)}

\item{heat_cw}{Heat capacity of water (Cw), which is the product of density times
specific heat of water, Q/L3 C. (default: 4180000.0)}

\item{solu_alphaL}{Longitudinal dispersion (Default: 1 m),}

\item{solu_alphaT}{Transversal dispersion (Default: 0.1 m)}

\item{solu_molDiffCoeff}{Molecular diffusion coefficient, Dm, L2/T. (Default: 
( 4.5e-05*3600*24/100 m2/day, ### http://en.wikipedia.org/wiki/Mass_diffusivity#Liquids 
for hydrogen - water at 25 C)}

\item{solu_decayConst}{Decay constant, l, T-1. (Default: 0)}

\item{solu_bulkDensity}{Bulk density, Dm (set to 0 for no adsorption
or ion exchange) M/L3. (Default: 0)}

\item{solu_adsorp}{0 for no adsorption or ion exchange; Kd for linear 
adsorption isotherm; K1 for Langmuir isotherm; Kf for Freundlich isotherm; or
Km for ion exchange. (Default: 0)}

\item{time_firstOutput}{first output after x time (default: after 1 second), 
(-> proxy for initial setting)}

\item{time_minSimTime}{start of regular output (Default: after 0.5 days)}

\item{time_maxSimTime}{end of regular output & model simulation (Default: 30.5 
days)}

\item{time_outputTimeStep}{time interval in days for which model results are 
written to output file (Default: 1 day)}

\item{solv_cis}{If TRUE spatial discretisation is realised by centered-in-space 
differencing; if FALSE backward-in-space differencing is to be used for transport 
equation. (default: TRUE)}

\item{solv_cit}{If TRUE temporal discretisation is realised by centered-in-time 
differencing; if FALSE backward-in-time or fully implicit differencing is to 
be used. (default: TRUE)}

\item{solv_numt}{Maximum number of time steps.(default: 5000). (NOTE: if enhanced 
precision in print out to file "balance.out" and file 11 "obsPoints.out", is 
desired set NUMT equal to a negative number. That is, multiply actual maximum 
number of time steps by -1)1}

\item{solv_minit}{Minimum number of iterations per time step. (default: 2)}

\item{solv_itmax}{Maximum number of iterations per time step. (default: 300)}

\item{solv_eps}{Head closure criterion for iterative solution of flow equation, L. 
(default: 0.001)}

\item{solv_eps1}{Temperature closure criterion for iterative solution of transport 
equation, C. (default: 0.001)}

\item{solv_eps2}{Velocity closure criterion for outer iteration loop at each time 
step, L/T. (default: 0.001)}

\item{solv_hmax}{Relaxation parameter for iterative solution. See discussion in 
Lappala and others (1987) for more detail. Value is generally in the range of 
0.4 to 1.2. (default: 0.7)}

\item{solv_itstop}{If TRUE simulation is terminated after ITMAX iterations in 
one time step; otherwise = F. (default: FALSE)}

\item{solr_delt}{Length of initial time step for this period, T. (default: 1.0E-6)}

\item{solr_tmlt}{Multiplier for time step length. (default: 1.2)}

\item{solr_dltmx}{Maximum allowed length of time step, T. (default: 1)}

\item{solr_dltmin}{Minimum allowed length of time step, T. (default: 1.0E-6)}

\item{solr_tred}{Factor by which time-step length is reduced if convergence is
not obtained in ITMAX iterations. Values usually should be in the range 0.1 to 
0.5. If no reduction of time-step length is desired, input a value of 0.0. 
(default: 0.1)}

\item{solr_dsmax}{Maximum allowed change in head per time step for this period, L. 
(default: 10)}

\item{solr_sterr}{Steady-state head criterion; when the maximum change in head 
between successive time steps is less than STERR, the program assumes that steady 
state has been reached for this period and advances to next recharge period, L. 
(default: 0)}

\item{solr_pond}{Maximum allowed height of ponded water for constant flux nodes. 
See Lappala and other (1987) for detailed discussion of POND, L. (default: 0)}
}
\value{
Prepared SVH model configuration
}
\description{
Prepare model: wrapper for modelConfiguration()
}
\examples{
### Importing GIS features
shp.dir <- system.file("extdata", "qgis", package="kwb.demeau")
shp.files <- dir(path = shp.dir, pattern = ".shp", full.names = TRUE)
gisData <- importShapefiles(shp.files)
### Optionally remove some features 
gisData <- removeFeatures(gisData = gisData, ignoreFeatureIDs = 20)
#### 1) Prepare heat 
preparedHeatModel  <- prepareModel(gisData = gisData, type = "heat")
#' #### 1) Prepare solute transport model  
preparedSoluteModel  <- prepareModel(gisData = gisData, type = "solu") 
}
