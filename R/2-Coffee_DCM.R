#' @title Dynamic Agroforestry Coffee Crop Model
#' @description   The DynACof process-based model computes plot-scale Net Primary Productivity, carbon allocation, growth, yield,
#'                energy, and water balance of coffee plantations according to management, while accounting for spatial effects using
#'                metamodels from the 3D process-based model \href{https://maespa.github.io/}{MAESPA}. The model also uses cohorts for
#'                the development of the coffee buds and fruits to better represent fruit carbon demand distribution along the year.
#' @param Period   Period of time to be simulated, see details. Default: \code{NULL}
#' @param WriteIt  If \code{TRUE}, write the outputs to disk using \code{\link{write.results}}, see details. Default: \code{FALSE}
#' @param ...      Further arguments to pass to \code{\link{write.results}}.
#' @param output_f Output format. If \code{output_f = ".RData"}, the output list will be saved as a unique \code{.RData} file. Any other value:
#'                 write the output list in several \code{.csv} and \code{.txt} files. Default: \code{.RData}
#' @param Inpath   Path to the input parameter list folder, Default: \code{"1-Input/Default"}
#' @param Outpath  Path pointing to the folder were the results will be writen, Default: \code{Outpath = Inpath}
#' @param Simulation_Name Character name of the simulation file name if \code{WriteIt = T}. Default: \code{"DynACof"}
#' @param FileName A list of input file names :
#' \describe{
#'   \item{Site}{Site parameters file name, see details. Default: \code{'1-Site.R'}}
#'   \item{Meteo}{Meteo parameters file name, see details. Default: \code{'2-Meteorology.txt'}}
#'   \item{Soil}{Soil parameters file name, see details. Default: \code{'3-Soil.R'}}
#'   \item{Coffee}{Coffee parameters file name, see details. Default: \code{'4-Coffee.R'}}
#'   \item{Tree}{Shade tree parameters file name, see details. Default: \code{NULL}}
#' }
#' Default input files are provided with the package as an example parameterization.
#'
#' @return Return invisibly a list containing three objects (Parameters, Meteo and Sim):
#' \itemize{
#'   \item Sim: A data.frame of the simulation outputs at daily time-step: \tabular{llll}{
#' \strong{Type} \tab \strong{Var} \tab \strong{unit} \tab \strong{Definition}\cr
#' General                      \tab Cycle                    \tab -                   \tab Plantation cycle ID                                                                \cr
#'                              \tab Plot_Age                 \tab year                \tab Plantation age (starting at 1)                                                     \cr
#'                              \tab Plot_Age_num             \tab year (numeric)      \tab Numeric age of plantation                                                          \cr
#'                              \tab LAIplot                  \tab m2 leaves m-2 soil  \tab Plot (Coffee + Shade Tree if any) Leaf Area Index                                  \cr
#' Suffixes for Coffee organs   \tab *_RE                     \tab -                   \tab Reserves                                                                           \cr
#'                              \tab *_SCR                    \tab -                   \tab Stump and Coarse roots                                                             \cr
#'                              \tab *_Fruit                  \tab -                   \tab Fruit                                                                              \cr
#'                              \tab *_RsWood                 \tab -                   \tab Resprout wood (= branches)                                                         \cr
#'                              \tab *_FRoot                  \tab -                   \tab Fine roots                                                                         \cr
#'                              \tab *_Leaf                   \tab                     \tab Leaves                                                                             \cr
#' Suffixes for Shade Tree org. \tab *_RE_Tree                \tab -                   \tab Reserves                                                                           \cr
#'                              \tab *_Stem_Tree              \tab -                   \tab Stem (= trunk)                                                                     \cr
#'                              \tab *_Branch_Tree            \tab -                   \tab Branches                                                                           \cr
#'                              \tab *_CoarseRoot_Tree        \tab -                   \tab Coarse roots                                                                       \cr
#'                              \tab *_FRoot_Tree             \tab -                   \tab Fine roots                                                                         \cr
#'                              \tab *_Leaf_Tree              \tab                     \tab Leaves                                                                             \cr
#' Energy                       \tab Rn_tot                   \tab MJ m-2 d-1          \tab System net radiation                                                               \cr
#'                              \tab Rn_Tree                  \tab MJ m-2 d-1          \tab Shade tree net radiation                                                           \cr
#'                              \tab Rn_Coffee                \tab MJ m-2 d-1          \tab Coffee net radiation                                                               \cr
#'                              \tab Rn_Soil                  \tab MJ m-2 d-1          \tab Soil net radiation                                                                 \cr
#'                              \tab Rn_Soil_SW               \tab MJ m-2 d-1          \tab Soil net radiation computed using Shuttleworth & Wallace (1985) for reference      \cr
#'                              \tab LE_*                     \tab MJ m-2 d-1          \tab System/Coffee/Tree/Soil latent heat                                                \cr
#'                              \tab H_*                      \tab MJ m-2 d-1          \tab System/Coffee/Tree/Soil sensible heat                                              \cr
#'                              \tab Q_Soil                   \tab MJ m-2 d-1          \tab Soil heat transport                                                                \cr
#'                              \tab Transmittance_Tree       \tab fraction            \tab Fraction of light transmitted by the shade trees                                   \cr
#'                              \tab PAR_Trans_Tree           \tab MJ m-2 d-1          \tab Light transmitted by the shade trees canopy                                        \cr
#'                              \tab PAR_Trans                \tab MJ m-2 d-1          \tab Light transmitted by the Coffea canopy                                             \cr
#'                              \tab K_Dir                    \tab -                   \tab Direct light extinction coefficient                                                \cr
#'                              \tab K_Dif                    \tab -                   \tab Diffuse light extinction coefficient                                               \cr
#'                              \tab APAR                     \tab MJ m-2 d-1          \tab Absorbed PAR by the plant                                                          \cr
#'                              \tab APAR_Dif                 \tab MJ m-2 d-1          \tab Absorbed diffuse PAR (Direct is APAR-APAR_Dif)                                     \cr
#'                              \tab lue                      \tab gC MJ               \tab Light use efficiency                                                               \cr
#'                              \tab Tcan_MAESPA_Coffee       \tab deg C               \tab Coffee canopy temperature computed using MAESPA metamodel                          \cr
#'                              \tab Tleaf_Coffee             \tab deg C               \tab Coffee canopy temperature computed by DynACof                                      \cr
#'                              \tab WindSpeed_*              \tab m s-1               \tab Wind speed at the center of the layer                                              \cr
#'                              \tab TairCanopy_*             \tab deg C               \tab Air tempetature at the center of the layer                                         \cr
#'                              \tab DegreeDays_Tcan          \tab deg C               \tab Growing degree days computed using Coffee Canopy Temperature                       \cr
#' Carbon                       \tab GPP                      \tab gC m-2 d-1          \tab Gross primary productivity                                                         \cr
#'                              \tab Consumption_RE           \tab gC m-2 d-1          \tab Daily reserve consumption                                                          \cr
#'                              \tab Carbon_Lack_Mortality    \tab gC m-2 d-1          \tab Mortality from a higher carbon consumption than offer                              \cr
#'                              \tab Rm                       \tab gC m-2 d-1          \tab Total Coffee maintenance respiration                                               \cr
#'                              \tab Rm_*                     \tab gC m-2 d-1          \tab Maintenance respiration at organ scale                                             \cr
#'                              \tab Rc                       \tab gC m-2 d-1          \tab Total Coffee growth respiration                                                    \cr
#'                              \tab Rc_*                     \tab gC m-2 d-1          \tab Growth respiration at organ scale                                                  \cr
#'                              \tab Ra                       \tab gC m-2 d-1          \tab Coffee layer autotrophic respiration (=Rm+Rc)                                      \cr
#'                              \tab Demand_*                 \tab gC m-2 d-1          \tab C demand at organ scale (fruit, leaf and fine root only)                           \cr
#'                              \tab Alloc_*                  \tab gC m-2 d-1          \tab C allocation to organ net of Rm (NPP+Rc)                                           \cr
#'                              \tab Offer                    \tab gC m-2 d-1          \tab C offer at the begining of the day at layer scale (GPP+Reserve consumption-Rm)     \cr
#'                              \tab Offer_*                  \tab gC m-2 d-1          \tab C offer to organ, net of Rm                                                        \cr
#'                              \tab NPP                      \tab gC m-2 d-1          \tab Net primary productivity at layer scale                                            \cr
#'                              \tab NPP_*                    \tab gC m-2 d-1          \tab Net primary productivity at organ scale                                            \cr
#'                              \tab Mnat_*                   \tab gC m-2 d-1          \tab Organ natural mortality (= due to lifespan)                                        \cr
#'                              \tab Mprun_*                  \tab gC m-2 d-1          \tab Organ mortality due to pruning                                                     \cr
#'                              \tab M_ALS                     \tab gC m-2 d-1          \tab Coffee leaf mortality from American Leaf Spot                                      \cr
#'                              \tab Mortality_*              \tab gC m-2 d-1          \tab Total organ mortality                                                              \cr
#'                              \tab LAI                      \tab m2 leaves m-2 soil  \tab Leaf Area Index                                                                    \cr
#'                              \tab CM_*                     \tab gC m-2 d-1          \tab Organ C mass                                                                       \cr
#'                              \tab DM_*                     \tab gDM m-2 d-1         \tab Organ dry mass                                                                     \cr
#' Fruit development            \tab BudInitPeriod            \tab boolean             \tab Bud initiation period                                                              \cr
#'                              \tab Budinit                  \tab Buds d-1            \tab Total Number of Buds Initiated per day                                             \cr
#'                              \tab ratioNodestoLAI          \tab Nodes LAI-1         \tab Number of fruiting nodes per LAI unit                                              \cr
#'                              \tab Temp_cor_Bud             \tab fraction            \tab Temperature correction factor for bud development                                  \cr
#'                              \tab p_budbreakperday         \tab 0-1                 \tab Daily probability of bud dormancy break                                            \cr
#'                              \tab BudBreak                 \tab Buds d-1            \tab Total number of buds breaking dormancy per day                                     \cr
#'                              \tab Sucrose_Mass             \tab g m-2 d-1           \tab Coffee Fruit Sucrose Mass                                                          \cr
#'                              \tab Sucrose_Content          \tab g Sugar gDM         \tab Coffee Fruit Sucrose Content                                                       \cr
#'                              \tab Maturation_duration      \tab days Fruit cohort-1 \tab Coffee Fruit Total Maturation Duration for each cohort                             \cr
#'                              \tab Harvest_Maturity_Pot     \tab Fraction            \tab Daily average fruit maturity (0-1)                                                 \cr
#'                              \tab Date_harvest             \tab day of year         \tab date of harvest                                                                    \cr
#'                              \tab Harvest_Fruit            \tab gC m-2              \tab Total fruit carbon mass at harvest                                                 \cr
#'                              \tab Harvest_Maturity         \tab Fraction            \tab Average fruit maturity at harvest (0-1)                                            \cr
#'                              \tab Overriped_Fruit          \tab gC m-2 d-1          \tab Overriped fruits that fall onto the ground                                         \cr
#' Water                        \tab IntercMax                \tab mm                  \tab Maximum potential rainfall interception by canopy                                  \cr
#'                              \tab CanopyHumect             \tab mm                  \tab Rainfall interception by canopy                                                    \cr
#'                              \tab Throughfall              \tab mm                  \tab Rainfall not intercepted by the canopy, coming to the soil                         \cr
#'                              \tab SuperficialRunoff        \tab mm                  \tab Water runoff from the superficial soil layer                                       \cr
#'                              \tab ExcessRunoff             \tab mm                  \tab Discharge from the superficial soil layer                                          \cr
#'                              \tab TotSuperficialRunoff     \tab mm                  \tab Sum of discharge+ExcessRunoff                                                      \cr
#'                              \tab InfilCapa                \tab mm                  \tab Superficial water infiltration capacity to first layer of soil                     \cr
#'                              \tab Infiltration             \tab mm                  \tab Superficial water infiltration to first layer of soil                              \cr
#'                              \tab Drain_[1-3]              \tab mm                  \tab Water drainage from soil layer 1, 2 or 3                                           \cr
#'                              \tab WSurfaceRes              \tab mm                  \tab Soil water content from the surface layer                                          \cr
#'                              \tab W_tot                    \tab mm                  \tab Total soil profile water content                                                   \cr
#'                              \tab W_[1-3]                  \tab mm                  \tab Soil water content from the layer 1, 2 or 3                                        \cr
#'                              \tab REW_tot                  \tab -                   \tab Relative extractable water from the soil                                           \cr
#'                              \tab REW_[1-3]                \tab -                   \tab Relative extractable water from the layer 1, 2 or 3                                \cr
#'                              \tab EW_tot                   \tab mm                  \tab Extractable water from the soil                                                    \cr
#'                              \tab EW_[1-3]                 \tab mm                  \tab Extractable water from the layer 1, 2 or 3                                         \cr
#'                              \tab SWD                      \tab mm                  \tab soil water deficit                                                                 \cr
#'                              \tab RootWaterExtract_[1-3]   \tab mm                  \tab Root water extraction for soil layer 1 to 3                                        \cr
#'                              \tab IntercRevapor            \tab mm                  \tab Evaporation by canopy                                                              \cr
#'                              \tab T_*                      \tab mm                  \tab Transpiration at system/Coffee/Tree scale                                          \cr
#'                              \tab E_Soil                   \tab mm                  \tab Soil evaporation                                                                   \cr
#'                              \tab ETR                      \tab mm                  \tab System evapotranspiration                                                          \cr
#'                              \tab SoilWaterPot             \tab MPa                 \tab Soil water potential                                                               \cr
#'                              \tab LeafWaterPotential       \tab Mpa                 \tab Coffee leaf water potential                                                        \cr
#' Special shade tree variables \tab LA_Tree                  \tab m2 leaves tree-1    \tab shade tree leaf area                                                               \cr
#'                              \tab Crown_H_Tree             \tab m                   \tab Crown height                                                                       \cr
#'                              \tab Trunk_H_Tree             \tab m                   \tab Trunk height                                                                       \cr
#'                              \tab Height_Tree              \tab m                   \tab Shade tree total height (used for boundary conductance), set to 0 if no shade trees\cr
#'                              \tab DBH_Tree                 \tab m                   \tab Diameter at breast height                                                          \cr
#'                              \tab LAD_Tree                 \tab m2 m-3              \tab Shade tree Leaf Area Density                                                       \cr
#'                              \tab CrownRad_Tree            \tab m                   \tab Crown radius                                                                       \cr
#'                              \tab CrownProj_Tree           \tab m2 crown tree-1     \tab Crown projection                                                                   \cr
#'                              \tab Stocking_Tree            \tab tree m-2            \tab Shade tree density                                                                 \cr
#'                              \tab TimetoThin_Tree          \tab boolean             \tab Days on which tree is thinned                                                      \cr
#'                              \tab MThinning_*_Tree         \tab gc m-2 d-1          \tab Mortality due to thining at organ scale
#'}
#'
#'   \item Meteo: A data.frame of the input meteorology, potentially coming from the output of \code{\link{Meteorology}}: \tabular{llll}{\strong{Var} \tab \strong{unit} \tab \strong{Definition} \tab \strong{If missing} \cr
#' Date            \tab POSIXct date\tab Date in POSIXct format                       \tab Computed from start date parameter, or set a dummy date if missing\cr
#' year            \tab year        \tab Year of the simulation                       \tab Computed from Date \cr
#' DOY             \tab day         \tab day of the year                              \tab Computed from Date \cr
#' Rain            \tab mm          \tab Rainfall                                     \tab Assume no rain \cr
#' Tair            \tab deg C       \tab Air temperature (above canopy)               \tab Computed from Tmax and Tmin \cr
#' Tmax            \tab deg C       \tab Maximum air temperature during the day       \tab Required (error) \cr
#' Tmin            \tab deg C       \tab Minimum air temperature during the day       \tab Required (error) \cr
#' RH              \tab \%          \tab Relative humidity                            \tab Not used, but prefered over VPD for Rn computation \cr
#' RAD             \tab MJ m-2 d-1  \tab Incident shortwave radiation                 \tab Computed from PAR \cr
#' Pressure        \tab hPa         \tab Atmospheric pressure                         \tab Try to compute from VPD, Tair and Elevation, or Tair and Elevation. \cr
#' WindSpeed       \tab m s-1       \tab Wind speed                                   \tab Try to set it to constant: \code{Parameters$WindSpeed} \cr
#' CO2             \tab ppm         \tab Atmospheric CO2 concentration                \tab Try to set it to constant: \code{Parameters$CO2}\cr
#' DegreeDays      \tab deg C       \tab Growing degree days                          \tab Computed using \code{\link{GDD}} \cr
#' PAR             \tab MJ m-2 d-1  \tab Incident photosynthetically active radiation \tab Computed from RAD \cr
#' FDiff           \tab Fraction    \tab Diffuse light fraction                       \tab Computed using \code{\link{Diffuse_d}} using Spitters formula \cr
#' VPD             \tab hPa         \tab Vapor pressure deficit                       \tab Computed from RH \cr
#' Rn (!=Rn_tot)    \tab MJ m-2 d-1  \tab Net radiation (will soon be depreciated)     \tab Computed using \code{\link{Rad_net}} with RH, or VPD \cr
#' DaysWithoutRain \tab day         \tab Number of consecutive days with no rainfall  \tab Computed from Rain \cr
#' Air_Density     \tab kg m-3      \tab Air density of moist air (\eqn{\rho}) above canopy \tab Computed using \code{\link[bigleaf]{air.density}}}
#'   \item Parameters: A list of the input parameters (see \code{\link{site}})
#' }
#'
#' @details The user can import a simulation using \code{\link[base]{readRDS}}.
#'          Almost all variables for coffee exist also for shade trees with the suffix
#'          \code{_Tree} after the name of the variable, e.g. : LAI = coffee LAI,
#'          LAI_Tree = shade tree LAI.
#'          Special shade tree variables (see return section) are only optional,
#'          and it may have more variables upon parameterization because variables can be added in
#'          the metamodels parameter file in \strong{\code{\link[=Light_extinction_K]{Metamodels}}} or
#'          \strong{\code{\link{Allometries}}}.
#'          Important :
#'          It is highly recommended to set the system environment timezone to the one from the meteorology file.
#'          For example the default meteorology file (\code{\link{Aquiares}}) has to be set to \code{Sys.setenv(TZ="UTC")}.
#'
#' @note All variable units are available as attributes (see example)
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  Sys.setenv(TZ="UTC")
#'  Sim= DynACof(Period= as.POSIXct(c("1979-01-01", "1980-12-31")))
#'
#'  # Get the units of the input variables:
#'  attr(Sim$Meteo,"unit")
#'
#'  # Get the units of the output variables:
#'  attr(Sim$Sim,"unit")
#'  }
#' }
#' @export
#' @seealso \code{\link{Meteorology}} \code{\link{site}}
#' @importFrom bigleaf air.density
#' @importFrom dplyr n
#' @importFrom foreach %dopar%
#' @importFrom methods is new
#' @importFrom doParallel registerDoParallel
#' @importFrom crayon red green bold underline
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
DynACof= function(Period=NULL, WriteIt= F,...,
                  output_f=".RData",Inpath=NULL,Outpath=Inpath,Simulation_Name="DynACof",
                  FileName= list(Site="1-Site.R",Meteo="2-Meteorology.txt",Soil="3-Soil.R",
                                 Coffee="4-Coffee.R",Tree=NULL)){

  Cycle= Plot_Age= cy= varnames= NULL # to avoid check notes

  # Importing the parameters ------------------------------------------------

  Parameters= Import_Parameters(path = Inpath, Names= FileName[-grep("Meteo",FileName)])

  # Importing the meteo -----------------------------------------------------

  Meteo= Meteorology(file= file.path(Inpath,FileName$Meteo),Period= Period,Parameters= Parameters)
  Parameters$files$Meteorology= file.path(Inpath,FileName$Meteo) # save the meteo file path


  # Setting the simulation --------------------------------------------------

  # Number of cycles (rotations) to do over the period (given by the Meteo file):
  NCycles= ceiling((max(Meteo$year)-min(Meteo$year))/Parameters$AgeCoffeeMax)
  if(NCycles==0){
    stop(paste("Carefull, minimum allowed simulation length is one year"))
  }

  #Day number and Years After Plantation
  ndaysYear= sapply(X= unique(Meteo$year), FUN= function(x){
    length(Meteo$year[Meteo$year==x])})

  Direction= data.frame(
    Cycle= rep.int(rep(1:NCycles, each= Parameters$AgeCoffeeMax)[1:length(unique(Meteo$year))],
                   times= ndaysYear),
    Plot_Age= rep.int(rep_len(seq(Parameters$AgeCoffeeMin,Parameters$AgeCoffeeMax),
                              length.out= length(unique(Meteo$year))),times= ndaysYear))
  Direction%<>%
    group_by(Cycle,Plot_Age)%>%
    mutate(Plot_Age_num= seq(min(Plot_Age),min(Plot_Age)+1, length.out= n()))%>%ungroup()
  # Variables are reinitialized so each cycle is independant from the others -> mandatory for
  # parallel processing

  message(paste("Starting a simulation from",crayon::red(min(Meteo$Date)),"to",
                crayon::red(max(Meteo$Date)),"over",crayon::red(NCycles),
                "plantation cycle(s)"))

  if(NCycles>1){
    # Setting the parallel computation over cycles: ---------------------------

    # Set the maximum number of cores working on the model computation
    NbCores= parallel::detectCores()-1
    cl= parallel::makeCluster(min(NbCores,NCycles))
    doParallel::registerDoParallel(cl)

    CycleList= foreach::foreach(cy= 1:NCycles,.combine=rbind,
                                .packages = c("dplyr","zoo")) %dopar% {

                                  mainfun(cy,Direction,Meteo,Parameters)

                                }
    parallel::stopCluster(cl)
  }else{
    CycleList= mainfun(NCycles,Direction,Meteo,Parameters)
  }

  Table= do.call(rbind, CycleList[c(1:NCycles)])
  UnwantedVarnames= c('.Fruit_Cohort',"Bud_available","BudBreak_cohort")
  Table= Table[,!colnames(Table)%in%UnwantedVarnames]

  attr(Table,"unit")= data.frame(varnames)

  message(paste("\n", crayon::green$bold$underline("Simulation completed successfully"),
                "\n"))
  FinalList= list(Sim= Table,Meteo= Meteo, Parameters= Parameters)
  if(WriteIt){
    write.results(FinalList,output_f,Simulation_Name,Outpath,...)
  }
  invisible(FinalList)
}



#' Main function
#'
#' @description This is the main function of the model that calls all other functions
#' (meteorology, shade tree, soil) and computes the Coffea simulation. This function
#' is called by \code{\link{DynACof}} under the hood, and users should always call
#' \code{\link{DynACof}} instead of this function because it imports the files,
#' format the simulation inputs, checks for errors, takes care automatically
#' of the computation distribution along nodes, and format the outputs.
#'
#' @param cy         The growing cycle (crop rotation)
#' @param Direction  Simulation directives as a data.frame
#' @param Meteo      Meteorology data.frame
#' @param Parameters Simulation parameters
#'
#' @details The Direction \code{data.frame} has to contain at least one column
#' named Cycle that denotes the crop rotation, Plot_Age, an integer for the plot age,
#' Plot_Age_num for the plot age as a continuous variable (see example).
#'
#' @return The simulation output as a data.frame.
#'
#' @export
mainfun= function(cy,Direction,Meteo,Parameters){

  .= NULL
  # Initializing the Simulation object:

  S= SimulationClass$new()
  S$Parameters= Parameters

  # Initializing the table:
  S$Sim= as.list(Direction[Direction$Cycle==cy,])
  S$Met_c= as.list(Meteo[Direction$Cycle==cy,])
  Init_Sim(S)

  # Compute cumulative degree-days based on previous daily DD from semi-hourly data:
  CumulDegreeDays= cumsum(S$Met_c$DegreeDays)

  ## Bud induction window computation ##
  # Bud induction can start only at F_Tffb degree-days after vegetative growth stops.
  # Source: Rodriguez et al. (2011).
  # The following module finds the vegetative growth end day, and add the F_Tffb parameter
  # (Time of first floral buds, in dd), then find the very first flowering of the year
  # and set the vector BudInitPeriod to TRUE between the two dates. So buds will appear
  # between plant F_Tffb parameter and the first flowering day only.

  # Day of vegetative growth end:
  VegetGrowthEndDay= which(S$Met_c$DOY==S$Parameters$VGS_Stop)
  # Temporary variables declaration:
  CumsumRelativeToVeget= CumsumRelativeToBudinit=
    matrix(data = NA, nrow = length(VegetGrowthEndDay), ncol = length(S$Met_c$Date))
  DateBudinit= DateFFlowering= NULL
  for (i in 1:length(VegetGrowthEndDay)){
    CumsumRelativeToVeget[i,]=
      CumulDegreeDays-CumulDegreeDays[VegetGrowthEndDay[i]-1]
    # Date of first bud initialisation:
    DateBudinit[i]= tail(which(CumsumRelativeToVeget[i,]<S$Parameters$F_Tffb),1)
    CumsumRelativeToBudinit[i,]=
      CumulDegreeDays-CumulDegreeDays[DateBudinit[i]-1]
    # Minimum date of first bud development end (i.e. without dormancy):
    BudDevelEnd= tail(which(CumsumRelativeToBudinit[i,]<S$Parameters$F_buds1),1)-1
    # Maximum date of first bud development end (i.e. with maximum dormancy):
    MaxDormancy= tail(which(CumsumRelativeToBudinit[i,]<S$Parameters$F_buds2),1)-1
    # Cumulative rainfall within the period of potential dormancy:
    CumRain= cumsum(S$Met_c$Rain[BudDevelEnd:MaxDormancy])
    # Effective (real) day of first buds breaking dormancy:
    BudDormancyBreakDay= BudDevelEnd + sum(CumRain<S$Parameters$F_rain)-1
    # Effective day of first flowers:
    DateFFlowering[i]=
      tail(which(CumsumRelativeToBudinit[i,]<
                   CumsumRelativeToBudinit[i,BudDormancyBreakDay]+S$Parameters$BudInitEnd),1)
    # Effective dates between which buds can appear
    S$Sim$BudInitPeriod[DateBudinit[i]:DateFFlowering[i]]= TRUE
  }
  S$Sim$BudInitPeriod[CumulDegreeDays<S$Parameters$VF_Flowering]= FALSE

  # Search for the species specific tree function:
  if(S$Parameters$Tree_Species=="No_Shade"){
    Treefun= No_Shade
  }else{
    Treefun= Shade.Tree
  }

  # American Leaf Spot:
  S$Sim$ALS=
    suppressMessages(ALS(Elevation= S$Parameters$Elevation, SlopeAzimut= S$Parameters$SlopeAzimut,
                         Slope= S$Parameters$Slope, RowDistance= S$Parameters$RowDistance,
                         Shade= S$Parameters$Shade, CanopyHeight.Coffee= S$Parameters$Height_Coffee,
                         Fertilization= S$Parameters$Fertilization, ShadeType= S$Parameters$ShadeType,
                         CoffeePruning= S$Parameters$CoffeePruning,
                         df_rain= data.frame(year=S$Met_c$year,DOY=S$Met_c$DOY,Rain=S$Met_c$Rain)))


  # Main Loop -----------------------------------------------------------------------------------

  pb= txtProgressBar(max= length(S$Sim$LAI), style=3)

  for (i in 1:length(S$Sim$LAI)){

    setTxtProgressBar(pb, i)
    # Shade Tree computation if any
    Treefun(S,i)
    # Should output at least APAR_Tree, LAI_Tree, T_Tree, Rn_Tree, H_Tree,
    # LE_Tree (sum of transpiration + leaf evap)

    # Coffea computation:

    # CM is in gC m-2soil, so use C content to transform in dry mass
    S$Sim$LAI[i]= S$Sim$CM_Leaf[previous_i(i,1)]*S$Parameters$SLA/1000/
      S$Parameters$CC_Leaf
    S$Sim$LAIplot[i]= S$Sim$LAIplot[i]+S$Sim$LAI[i]

    # Light interception ------------------------------------------------------

    S$Sim$K_Dif[i]= S$Parameters$k_Dif
    S$Sim$K_Dir[i]= S$Parameters$k_Dir

    #APAR coffee
    S$Sim$PAR_Trans_Tree[i]= S$Met_c$PAR[i]-S$Sim$APAR_Tree[i] # PAR above coffee layer
    S$Sim$APAR_Dif[i]=
      max(0,(S$Sim$PAR_Trans_Tree[i]*S$Met_c$FDiff[i])*
            (1-exp(-S$Sim$K_Dif[i]*S$Sim$LAI[i])))
    APAR_Dir= max(0,(S$Sim$PAR_Trans_Tree[i]*(1-S$Met_c$FDiff[i]))*
                    (1-exp(-S$Sim$K_Dir[i]*S$Sim$LAI[i])))
    # APAR_Dir is not part of S$Sim because it can be easily computed by
    # S$Met_c$PARm2d1-S$Sim$APAR_Dif
    S$Sim$APAR[i]= APAR_Dir+S$Sim$APAR_Dif[i]
    S$Sim$PAR_Trans[i]= S$Sim$PAR_Trans_Tree[i]-S$Sim$APAR[i] # PAR above soil layer

    # soil (+canopy evap) water balance ---------------------------------------

    Soilfun(S,i)

    # Metamodel Coffee leaf water potential
    S$Sim$LeafWaterPotential[i]=
      0.040730 - 0.005074*S$Met_c$VPD[i] - 0.037518*S$Sim$PAR_Trans_Tree[i] +
      2.676284*S$Sim$SoilWaterPot[i]

    # Energy balance ----------------------------------------------------------

    # Transpiration Coffee
    S$Sim$T_Cof[i]=
      -0.72080 + 0.07319*S$Met_c$VPD[i] -0.76984*(1-S$Met_c$FDiff[i]) +
      0.13646*S$Sim$LAI[i] + 0.12910*S$Sim$PAR_Trans_Tree[i]
    S$Sim$T_Cof[i][S$Sim$T_Cof[i]<0]= 0

    # Plot transpiration
    S$Sim$T_tot[i]= S$Sim$T_Tree[i]+S$Sim$T_Cof[i]

    # Evapo-Transpiration
    S$Sim$ETR[i]=
      S$Sim$T_tot[i]+S$Sim$E_Soil[i]+S$Sim$IntercRevapor[i]

    # Latent and Sensible heat fluxes
    S$Sim$LE_Plot[i]= S$Sim$ETR[i]*S$Parameters$lambda

    S$Sim$LE_Coffee[i]=
      (S$Sim$T_Cof[i]+S$Sim$IntercRevapor[i]*
         (S$Sim$LAI[i]/S$Sim$LAIplot[i]))*S$Parameters$lambda

    S$Sim$H_Coffee[i]=
      1.2560 - 0.2886*S$Met_c$VPD[i] - 3.6280*S$Met_c$FDiff[i] +
      2.6480*S$Sim$T_Cof[i] + 0.4389*S$Sim$PAR_Trans_Tree[i]

    # Coffea layer net radiation
    S$Sim$Rn_Coffee[i]=
      S$Sim$H_Coffee[i] + S$Sim$LE_Coffee[i]

    # Tree LE and Rn (can not compute them in the Tree function because we need IntercRevapor)
    S$Sim$LE_Tree[i]=
      (S$Sim$T_Tree[i]+S$Sim$IntercRevapor[i]*
         (S$Sim$LAI_Tree[i]/S$Sim$LAIplot[i]))*S$Parameters$lambda
    S$Sim$Rn_Tree[i]= S$Sim$H_Tree[i] + S$Sim$LE_Tree[i]

    # Total plot energy:
    S$Sim$H_tot[i]= S$Sim$H_Coffee[i]+S$Sim$H_Tree[i]+S$Sim$H_Soil[i]
    S$Sim$LE_tot[i]= S$Sim$LE_Coffee[i]+S$Sim$LE_Tree[i]+S$Sim$LE_Soil[i]
    S$Sim$Rn_tot[i]= S$Sim$Rn_Coffee[i]+S$Sim$Rn_Tree[i]+S$Sim$Rn_Soil[i]

    # Tcanopy Coffee : using bulk conductance if no trees, interlayer conductance if trees
    # Source: Van de Griend and Van Boxel 1989.
    if(S$Sim$Height_Tree[i]>S$Parameters$Height_Coffee){

      S$Sim$TairCanopy[i]=
        S$Sim$TairCanopy_Tree[i]+((S$Sim$H_Coffee[i]+S$Sim$H_Soil[i])*Parameters$MJ_to_W)/
        (bigleaf::air.density(S$Sim$TairCanopy_Tree[i],S$Met_c$Pressure[i]/10)*
           S$Parameters$Cp*
           G_interlay(Wind= S$Met_c$WindSpeed[i], ZHT = S$Parameters$ZHT,
                      LAI_top= S$Sim$LAI_Tree[i],
                      LAI_bot= S$Sim$LAI[i],
                      Z_top= S$Sim$Height_Tree[i],
                      extwind = S$Parameters$extwind))

      S$Sim$Tleaf_Coffee[i]=
        S$Sim$TairCanopy[i]+(S$Sim$H_Coffee[i]*Parameters$MJ_to_W)/
        (bigleaf::air.density(S$Sim$TairCanopy[i],S$Met_c$Pressure[i]/10)*
           S$Parameters$Cp*
           1/(1/G_interlay(Wind= S$Met_c$WindSpeed[i], ZHT = S$Parameters$ZHT,
                           LAI_top= S$Sim$LAI_Tree[i],
                           LAI_bot= S$Sim$LAI[i],
                           Z_top= S$Sim$Height_Tree[i],
                           extwind = S$Parameters$extwind)+
                1/Gb_h(Wind = S$Met_c$WindSpeed[i], wleaf= S$Parameters$wleaf,
                       LAI_lay=S$Sim$LAI[i],
                       LAI_abv=S$Sim$LAI_Tree[i],
                       ZHT = S$Parameters$ZHT,
                       Z_top = S$Sim$Height_Tree[i],
                       extwind= S$Parameters$extwind)))

    }else{

      S$Sim$TairCanopy[i]=
        S$Met_c$Tair[i]+((S$Sim$H_Coffee[i]+S$Sim$H_Soil[i])*Parameters$MJ_to_W)/
        (bigleaf::air.density(S$Sim$TairCanopy_Tree[i],S$Met_c$Pressure[i]/10)*
           S$Parameters$Cp*
           G_bulk(Wind = S$Met_c$WindSpeed[i], ZHT = S$Parameters$ZHT,
                  Z_top = S$Parameters$Height_Coffee,
                  LAI = S$Sim$LAI[i],
                  extwind = S$Parameters$extwind))

      S$Sim$Tleaf_Coffee[i]=
        S$Sim$TairCanopy[i]+(S$Sim$H_Coffee[i]*Parameters$MJ_to_W)/
        (bigleaf::air.density(S$Sim$TairCanopy[i],S$Met_c$Pressure[i]/10)*
           S$Parameters$Cp*
           1/(1/G_bulk(Wind = S$Met_c$WindSpeed[i], ZHT = S$Parameters$ZHT,
                       Z_top = S$Parameters$Height_Coffee,
                       LAI = S$Sim$LAI[i],
                       extwind = S$Parameters$extwind)+
                1/Gb_h(Wind= S$Met_c$WindSpeed[i], wleaf= S$Parameters$wleaf,
                       LAI_lay= S$Sim$LAI[i],
                       LAI_abv= S$Sim$LAI_Tree[i],
                       ZHT= S$Parameters$ZHT,
                       Z_top= S$Parameters$Height_Coffee,
                       extwind= S$Parameters$extwind)))
    }
    # NB: if no trees, TairCanopy_Tree= Tair


    # Metamodel for Coffea Tcanopy to compare:
    S$Sim$Tcan_MAESPA_Coffee[i]=
      -0.07741 + 0.99456*S$Met_c$Tair[i] - 0.06948*S$Met_c$VPD[i] -
      1.87975*(1-S$Met_c$FDiff[i]) + 0.19615*S$Sim$PAR_Trans_Tree[i]
    S$Sim$DegreeDays_Tcan[i]=
      GDD(Tmean = S$Sim$Tleaf_Coffee[i],MinTT = S$Parameters$MinTT,
          MaxTT = S$Parameters$MaxTT)

    # Metamodel LUE coffee:
    S$Sim$lue[i]=
      2.784288 + 0.009667*S$Met_c$Tair[i] + 0.010561*S$Met_c$VPD[i] -
      0.710361*sqrt(S$Sim$PAR_Trans_Tree[i])

    #GPP Coffee
    S$Sim$GPP[i]= S$Sim$lue[i]*S$Sim$APAR[i]

    # Maintenance respiration -------------------------------------------------

    # Rm is computed at the beginning of the day on the drymass of the previous day.
    # This is considered as the highest priority for the plant (to maintain its dry mass)

    # Resprout (branches) wood:
    S$Sim$Rm_RsWood[i]=
      after(i,2)*
      (S$Parameters$Palive_RsWood*S$Sim$DM_RsWood[previous_i(i,1)]*
         S$Parameters$NC_RsWood*S$Parameters$MRN*
         S$Parameters$Q10_RsWood^((S$Sim$TairCanopy[i]-S$Parameters$TMR)/10))

    # Stump and Coarse roots (perennial wood):
    S$Sim$Rm_SCR[i]=
      after(i,2)*
      (S$Parameters$Palive_SCR*
         S$Sim$DM_SCR[previous_i(i,1)]*
         S$Parameters$NC_SCR*S$Parameters$MRN*
         S$Parameters$Q10_SCR^(
           (S$Sim$TairCanopy[i]-S$Parameters$TMR)/10))

    # Fruits:
    S$Sim$Rm_Fruit[i]=
      after(i,2)*
      (S$Parameters$Palive_Fruit*S$Sim$DM_Fruit[previous_i(i,1)]*
         S$Parameters$NC_Fruit*S$Parameters$MRN*
         S$Parameters$Q10_Fruit^((S$Sim$TairCanopy[i]-S$Parameters$TMR)/10))

    # Leaves:
    S$Sim$Rm_Leaf[i]=
      after(i,2)*
      (S$Parameters$Palive_Leaf*S$Sim$DM_Leaf[previous_i(i,1)]*
         S$Parameters$NC_Leaf*S$Parameters$MRN*
         S$Parameters$Q10_Leaf^((S$Sim$TairCanopy[i]-S$Parameters$TMR)/10))

    # Fine roots:
    S$Sim$Rm_FRoot[i]=
      after(i,2)*
      (S$Parameters$Palive_FRoot*S$Sim$DM_FRoot[previous_i(i,1)]*
         S$Parameters$NC_FRoot*S$Parameters$MRN*
         S$Parameters$Q10_FRoot^((S$Sim$TairCanopy[i]-S$Parameters$TMR)/10))

    # Total plant maintenance respiration
    S$Sim$Rm[i]=
      S$Sim$Rm_Fruit[i]+S$Sim$Rm_Leaf[i]+
      S$Sim$Rm_RsWood[i]+S$Sim$Rm_SCR[i]+
      S$Sim$Rm_FRoot[i]



    # Coffee Allocation -------------------------------------------------------

    # Potential use of reserves:
    S$Sim$Consumption_RE[i]=
      S$Parameters$kres*S$Sim$CM_RE[previous_i(i,1)]

    # Offer function
    S$Sim$Offer[i]=
      max(S$Sim$GPP[i]-S$Sim$Rm[i]+S$Sim$Consumption_RE[i],0)

    # If the respiration is greater than the GPP + reserves use, then take this carbon
    # from mortality of each compartments' biomass equally (not for fruits or reserves):
    S$Sim$Carbon_Lack_Mortality[i]=
      -min(0,S$Sim$GPP[i]-S$Sim$Rm[i]+S$Sim$Consumption_RE[i])

    # 1-Resprout wood ---------------------------------------------------------
    # Allocation priority 1, see Charbonnier 2012.
    S$Sim$Alloc_RsWood[i]= S$Parameters$lambda_RsWood*S$Sim$Offer[i]
    S$Sim$NPP_RsWood[i]= S$Sim$Alloc_RsWood[i]/S$Parameters$epsilon_RsWood
    S$Sim$Rc_RsWood[i]= S$Sim$Alloc_RsWood[i]-S$Sim$NPP_RsWood[i]
    S$Sim$Mnat_RsWood[i]=
      S$Sim$CM_RsWood[previous_i(i,1)]/S$Parameters$lifespan_RsWood
    # Pruning
    if(S$Sim$Plot_Age[i]>=S$Parameters$MeanAgePruning&
       S$Met_c$DOY[i]==S$Parameters$date_pruning){
      S$Sim$Mprun_RsWood[i]=
        S$Sim$CM_RsWood[previous_i(i,1)]*S$Parameters$WoodPruningRate
    }
    S$Sim$Mortality_RsWood[i]=
      min((S$Sim$Mnat_RsWood[i]+S$Sim$Mprun_RsWood[i]),
          S$Sim$CM_RsWood[previous_i(i,1)])

    # 2-Stump and coarse roots (perennial wood) ------------------------------
    S$Sim$Alloc_SCR[i]= S$Parameters$lambda_SCR*S$Sim$Offer[i]
    S$Sim$NPP_SCR[i]= S$Sim$Alloc_SCR[i]/S$Parameters$epsilon_SCR
    S$Sim$Rc_SCR[i]= S$Sim$Alloc_SCR[i]-S$Sim$NPP_SCR[i]
    S$Sim$Mnat_SCR[i]=
      S$Sim$CM_SCR[previous_i(i,1)]/S$Parameters$lifespan_SCR
    S$Sim$Mortality_SCR[i]= S$Sim$Mnat_SCR[i]

    # Ratio of number of new nodes per LAI unit as affected by canopy air temperature
    # according to Drinnan & Menzel, 1995
    # Source "0 Effect T on yield and vegetative growth.xlsx", sheet
    # "Std20dComposWinterNodeperBr"
    # NB: computed at the end of the vegetatitve growth only to have Tcan of the
    # whole period already computed
    # NB2 : This is the total number of productive nodes on the coffee plant, i.e. the
    # number of green wood nodes that potentially carry flower buds. Green wood mass (and
    # so number of nodes) are related to leaf area (new leaves appear on nodes) :
    # GUTIERREZ et al. (1998)
    if(S$Met_c$DOY[i]==S$Parameters$VGS_Stop){
      S$Sim$ratioNodestoLAI[S$Met_c$year>=S$Met_c$year[i]]=
        mean(S$Sim$Tleaf_Coffee[S$Met_c$year==S$Met_c$year[i]&
                                  S$Met_c$DOY>=S$Parameters$VGS_Start&
                                  S$Met_c$DOY <= S$Parameters$VGS_Stop])%>%
                                  {S$Parameters$RNL_base*(0.0005455*.^3 - 0.0226364*.^2+0.2631364*. + 0.4194773)}
    }

    # Flower Buds + Flower + Fruits -------------------------------------------

    # (1) Buds induction
    # Buds start appearing for the very first time from 5500 dd. After that,
    # they appear every "S$Parameters$F_Tffb" degree days until flowering starts
    if(S$Sim$BudInitPeriod[i]){
      S$Sim$Budinit[i]=
        (S$Parameters$a_bud+S$Parameters$b_bud*(S$Sim$PAR_Trans_Tree[i]/S$Parameters$FPAR))*
        S$Sim$LAI[i-1]*S$Sim$ratioNodestoLAI[i-1]*S$Sim$DegreeDays_Tcan[i]
      # NB: Number of nodes= S$Sim$LAI[i-1]*S$Sim$ratioNodestoLAI[i-1]
      S$Sim$Bud_available[i]= S$Sim$Budinit[i]
    }
    # NB: number of fruits ~1200 / year / coffee tree, source : Castro-Tanzi et al. (2014)
    # S$Sim%>%group_by(Plot_Age)%>%summarise(N_Flowers= sum(BudBreak))

    # (2) Cumulative degree days experienced by each bud cohort :
    DegreeDay_i= round(cumsum(S$Sim$DegreeDays_Tcan[i:previous_i(i,1000)]),2)

    # (3) Find the window where buds are under dormancy (find the dormant cohorts)
    # Bud develops during F_buds1 (840) degree days after initiation, so they cannot
    # be dormant less than F_buds1 before i. But they can stay under dormancy until
    # F_buds2 dd maximum, so they cannot be older than F_buds2 dd before i.
    OldestDormancy= i - (max(which(DegreeDay_i<S$Parameters$F_buds2))-1)
    YoungestDormancy= i - (max(which(DegreeDay_i<S$Parameters$F_buds1))-1)
    # Idem above (reduce the days computed, F_buds2 is ~300 days and F_buds1
    # ~80-100 days)

    # (4) Test if the condition of minimum required rain for budbreak is met, and if
    # not, which cohort  first met the condition (starting from younger to older cohorts):
    CumRain= cumsum(S$Met_c$Rain[YoungestDormancy:OldestDormancy])
    # (5) Compute the period were all cohorts have encountered all conditions to break
    # dormancy :
    DormancyBreakPeriod= OldestDormancy:(YoungestDormancy-sum(CumRain<S$Parameters$F_rain))

    # (6) Temperature effect on bud phenology
    S$Sim$Temp_cor_Bud[i]=
      S$Parameters$Bud_T_correction()(S$Sim$Tleaf_Coffee[i])
    S$Sim$Temp_cor_Bud[i][S$Sim$Temp_cor_Bud[i]<0]= 0
    S$Sim$Temp_cor_Bud[i][S$Sim$Temp_cor_Bud[i]>1]= 1

    # (7) Bud dormancy break, Source, Drinnan 1992 and Rodriguez et al., 2011 eq. 13
    S$Sim$p_budbreakperday[i]= 1/(1+exp(S$Parameters$a_p+S$Parameters$b_p*
                                          S$Sim$LeafWaterPotential[i]))
    # (8) Compute the number of buds that effectively break dormancy in each cohort:
    S$Sim$BudBreak_cohort[DormancyBreakPeriod]=
      pmin(S$Sim$Bud_available[DormancyBreakPeriod],
           S$Sim$Budinit[DormancyBreakPeriod]*S$Sim$p_budbreakperday[i]*
             S$Sim$Temp_cor_Bud[DormancyBreakPeriod])
    # NB 1: cannot exceed the number of buds of each cohort
    # NB 2: using Budinit and not Bud_available because p_budbreakperday is fitted on
    # total bud cohort

    # (9) Remove buds that did break dormancy from the pool of dormant buds
    S$Sim$Bud_available[DormancyBreakPeriod]=
      S$Sim$Bud_available[DormancyBreakPeriod]-S$Sim$BudBreak_cohort[DormancyBreakPeriod]

    # (10) Sum the buds that break dormancy from each cohort to compute the total number
    # of buds that break dormancy on day i :
    S$Sim$BudBreak[i]= min(sum(S$Sim$BudBreak_cohort[DormancyBreakPeriod]),
                           S$Parameters$Max_Bud_Break)
    # Rodriguez et al. state that the maximum number of buds that may break dormancy
    # during each dormancy-terminating episode was set to 12 (see Table 1).

    # Fruits :
    FruitingPeriod= i-which(DegreeDay_i<(S$Parameters$F_over))+1
    # NB : Fruits that are older than the FruitingPeriod are overripped

    # Demand from each fruits cohort present on the coffee tree (not overriped),
    # same as Demand_Fruit but keeping each value :
    Demand_Fruit_Cohort_Period=
      S$Sim$BudBreak[FruitingPeriod]*S$Parameters$Opti_C_DemandFruit*
      logistic_deriv(DegreeDay_i[1:length(FruitingPeriod)],
                     S$Parameters$u_log,S$Parameters$s_log)
    Demand_Fruit_Cohort_Period[is.na(Demand_Fruit_Cohort_Period)]= 0
    # Total C demand of the fruits :
    S$Sim$Demand_Fruit[i]= sum(Demand_Fruit_Cohort_Period)
    # C offer to Fruits (i.e. what is left from Offer after removing the consumption
    # by previous compartments and Rm):
    S$Sim$Offer_Fruit[i]=
      S$Sim$Offer[i]-S$Sim$Alloc_RsWood[i]-
      S$Sim$Alloc_SCR[i]

    # Total C allocation to all fruits on day i :
    S$Sim$Alloc_Fruit[i]= min(S$Sim$Demand_Fruit[i],S$Sim$Offer_Fruit[i])
    # Allocation to each cohort, relative to each cohort demand :
    Rel_DE= Demand_Fruit_Cohort_Period/S$Sim$Demand_Fruit[i]
    Rel_DE[is.nan(Rel_DE)]= 0
    S$Sim$Alloc_Fruit_Cohort[FruitingPeriod]=
      S$Sim$Alloc_Fruit[i]*Rel_DE
    S$Sim$NPP_Fruit_Cohort[FruitingPeriod]=
      S$Sim$Alloc_Fruit_Cohort[FruitingPeriod]/S$Parameters$epsilon_Fruit
    S$Sim$CM_Fruit_Cohort[FruitingPeriod]= S$Sim$CM_Fruit_Cohort[FruitingPeriod]+
      S$Sim$NPP_Fruit_Cohort[FruitingPeriod]
    S$Sim$DM_Fruit_Cohort[FruitingPeriod]=
      S$Sim$CM_Fruit_Cohort[FruitingPeriod]/S$Parameters$CC_Fruit
    # Overriped fruits that fall onto the ground (= to mass of the cohort that overripe) :
    S$Sim$Overriped_Fruit[i]= S$Sim$CM_Fruit_Cohort[max(min(FruitingPeriod)-1,1)]
    # S$Sim$Overriped_Fruit[i]= S$Sim$CM_Fruit_Cohort[min(FruitingPeriod)-1]*S$Parameters$epsilon_Fruit

    # Duration of the maturation of each cohort born in the ith day (in days):
    S$Sim$Maturation_duration[FruitingPeriod]=
      seq_along(FruitingPeriod)
    # Sucrose content of each cohort:
    S$Sim$Sucrose_Content[FruitingPeriod]=
      Sucrose_cont_perc(S$Sim$Maturation_duration[FruitingPeriod],
                        a= S$Parameters$S_a, b= S$Parameters$S_b,
                        x0= S$Parameters$S_x0, y0=S$Parameters$S_y0)
    # Sucrose mass of each cohort
    S$Sim$Sucrose_Mass[FruitingPeriod]=
      S$Sim$DM_Fruit_Cohort[FruitingPeriod]*S$Sim$Sucrose_Content[FruitingPeriod]
    # Harvest maturity:
    S$Sim$Harvest_Maturity_Pot[i]=
      round(sum(S$Sim$Sucrose_Mass[FruitingPeriod])/
              sum(S$Sim$DM_Fruit_Cohort[FruitingPeriod]*
                    ((S$Parameters$S_y0 + S$Parameters$S_a)/100)),3)
    S$Sim$Harvest_Maturity_Pot[i][is.nan(S$Sim$Harvest_Maturity_Pot[i])]= 0

    # NB : here harvest maturity is computed as the average maturity of the cohorts, because
    # all cohorts present in the Coffea are within the 'FruitingPeriod' window.
    # It could be computed as the percentage of cohorts that are fully mature (Pezzopane
    # et al. 2012 say at 221 days after flowering)
    # Optimal sucrose concentration around 8.8% of the dry mass

    S$Sim$NPP_Fruit[i]= S$Sim$Alloc_Fruit[i]/S$Parameters$epsilon_Fruit
    S$Sim$Rc_Fruit[i]= S$Sim$Alloc_Fruit[i]-S$Sim$NPP_Fruit[i]

    # Harvest. Made one day only for now (TODO: make it a period of harvest)

      if(S$Parameters$harvest=="quantity"){
        is_harvest=
          S$Sim$Plot_Age[i]>=S$Parameters$ageMaturity&
          all(S$Sim$NPP_Fruit[previous_i(i,0:10)]<
                S$Sim$Overriped_Fruit[previous_i(i,0:10)])&
          S$Sim$CM_Fruit[previous_i(i,1)]>S$Parameters$Min_Fruit_CM
        # Made as soon as the fruit dry mass is decreasing for 10 consecutive days.
        # This condition is met when fruit overriping is more important than fruit NPP
        # for 10 days.
        # This option is the best one when fruit maturation is not well known or when the
        # harvest is made throughout several days or weeks with the assumption that fruits
        # are harvested when mature.
      }else{
        is_harvest=
          S$Sim$Plot_Age[i]>=S$Parameters$ageMaturity &
          mean(S$Sim$Harvest_Maturity_Pot[previous_i(i,0:9)])<
          mean(S$Sim$Harvest_Maturity_Pot[previous_i(i,10:19)])
        # Made as soon as the overall fruit maturation is optimal (all fruits are mature)
      }


    if(is_harvest){
      # Save the date of harvest:
      S$Sim$Date_harvest[i]= S$Met_c$DOY[i]
      S$Sim$Harvest_Fruit[i]=
        S$Sim$CM_Fruit[i-1]+S$Sim$NPP_Fruit[i]-S$Sim$Overriped_Fruit[i]
      S$Sim$Harvest_Maturity[i]= S$Sim$Harvest_Maturity_Pot[i]
      S$Sim$CM_Fruit[i-1]= S$Sim$NPP_Fruit[i]= S$Sim$Overriped_Fruit[i]= 0
      S$Sim$CM_Fruit_Cohort= rep(0,length(S$Sim$CM_Fruit_Cohort))
      # RV: could harvest mature fruits only (To do).
    }else{
      S$Sim$Harvest_Fruit[i]= NA_real_
    }

    # Leaves ------------------------------------------------------------------

    S$Sim$Offer_Leaf[i]=
      S$Parameters$lambda_Leaf_remain*
      (S$Sim$Offer[i]-S$Sim$Alloc_Fruit[i]-
         S$Sim$Alloc_RsWood[i]-S$Sim$Alloc_SCR[i])

    S$Sim$Alloc_Leaf[i]=
      min(S$Parameters$DELM*(S$Parameters$Stocking_Coffee/10000)*
            ((S$Parameters$LAI_max-S$Sim$LAI[i])/
               (S$Sim$LAI[i]+S$Parameters$LAI_max)),
          S$Sim$Offer_Leaf[i])


    S$Sim$NPP_Leaf[i]= S$Sim$Alloc_Leaf[i]/S$Parameters$epsilon_Leaf
    S$Sim$Rc_Leaf[i]= S$Sim$Alloc_Leaf[i]-S$Sim$NPP_Leaf[i]
    S$Sim$Mnat_Leaf[i]=S$Sim$CM_Leaf[previous_i(i,1)]/S$Parameters$lifespan_Leaf

    S$Sim$NPP_RE[i]= S$Sim$NPP_RE[i]+(S$Sim$Offer_Leaf[i]-S$Sim$Alloc_Leaf[i])

    S$Sim$M_ALS[i]=
      after(i,2)*max(0,S$Sim$CM_Leaf[previous_i(i,1)]*S$Sim$ALS[i])

    if(S$Sim$Plot_Age[i]>=
       S$Parameters$MeanAgePruning&S$Met_c$DOY[i]==S$Parameters$date_pruning){
      S$Sim$Mprun_Leaf[i]= S$Sim$CM_Leaf[previous_i(i,1)]*S$Parameters$LeafPruningRate
    }else{
      S$Sim$Mprun_Leaf[i]= 0
    }

    S$Sim$Mortality_Leaf[i]= S$Sim$Mnat_Leaf[i] + S$Sim$Mprun_Leaf[i]+S$Sim$M_ALS[i]

    # Fine Roots --------------------------------------------------------------

    S$Sim$Offer_FRoot[i]=
      S$Parameters$lambda_FRoot_remain*
      (S$Sim$Offer[i]-S$Sim$Alloc_Fruit[i]-
         S$Sim$Alloc_RsWood[i]-S$Sim$Alloc_SCR[i])

    S$Sim$Alloc_FRoot[i]=max(0,min(S$Sim$Alloc_Leaf[i],S$Sim$Offer_FRoot[i]))

    S$Sim$NPP_FRoot[i]= S$Sim$Alloc_FRoot[i]/S$Parameters$epsilon_FRoot

    S$Sim$Rc_FRoot[i]= S$Sim$Alloc_FRoot[i]-S$Sim$NPP_FRoot[i]

    S$Sim$NPP_RE[i]= S$Sim$NPP_RE[i]+(S$Sim$Offer_FRoot[i]-S$Sim$Alloc_FRoot[i])

    S$Sim$Mnat_FRoot[i]= S$Sim$CM_FRoot[previous_i(i,1)]/S$Parameters$lifespan_FRoot
    S$Sim$Mprun_FRoot[i]= S$Parameters$M_RateFRootprun*S$Sim$Mprun_Leaf[i]
    S$Sim$Mortality_FRoot[i]= S$Sim$Mnat_FRoot[i]+S$Sim$Mprun_FRoot[i]


    # Biomass -----------------------------------------------------------------

    S$Sim$CM_Leaf[i]= S$Sim$CM_Leaf[previous_i(i,1)]+
      S$Sim$NPP_Leaf[i]-S$Sim$Mortality_Leaf[i]-
      S$Sim$Carbon_Lack_Mortality[i]*0.25
    S$Sim$CM_RsWood[i]= S$Sim$CM_RsWood[previous_i(i,1)]+
      S$Sim$NPP_RsWood[i]-S$Sim$Mortality_RsWood[i]-
      S$Sim$Carbon_Lack_Mortality[i]*0.25
    S$Sim$CM_Fruit[i]=S$Sim$CM_Fruit[previous_i(i,1)]+
      S$Sim$NPP_Fruit[i]-S$Sim$Overriped_Fruit[i]
    S$Sim$CM_SCR[i]= S$Sim$CM_SCR[previous_i(i,1)]+
      S$Sim$NPP_SCR[i]-S$Sim$Mortality_SCR[i]-
      S$Sim$Carbon_Lack_Mortality[i]*0.25
    S$Sim$CM_FRoot[i]= S$Sim$CM_FRoot[previous_i(i,1)]+
      S$Sim$NPP_FRoot[i]-S$Sim$Mortality_FRoot[i]-
      S$Sim$Carbon_Lack_Mortality[i]*0.25
    S$Sim$CM_RE[i]=S$Sim$CM_RE[previous_i(i,1)]+S$Sim$NPP_RE[i]-
      S$Sim$Consumption_RE[i]

    S$Sim$DM_Leaf[i]= S$Sim$CM_Leaf[i]/S$Parameters$CC_Leaf
    S$Sim$DM_RsWood[i]= S$Sim$CM_RsWood[i]/S$Parameters$CC_RsWood
    S$Sim$DM_Fruit[i]=S$Sim$CM_Fruit[i]/S$Parameters$CC_Fruit
    S$Sim$DM_SCR[i]= S$Sim$CM_SCR[i]/
      S$Parameters$CC_SCR
    S$Sim$DM_FRoot[i]= S$Sim$CM_FRoot[i]/S$Parameters$CC_FRoots
    S$Sim$DM_RE[i]=S$Sim$CM_RE[i]/S$Parameters$CC_SCR


    # Total Respiration and NPP -----------------------------------------------

    S$Sim$Rc[i]= S$Sim$Rc_Fruit[i]+S$Sim$Rc_Leaf[i]+
      S$Sim$Rc_RsWood[i]+S$Sim$Rc_SCR[i]+
      S$Sim$Rc_FRoot[i]
    S$Sim$Ra[i]=S$Sim$Rm[i]+S$Sim$Rc[i]
    S$Sim$NPP[i]=S$Sim$NPP_RsWood[i]+S$Sim$NPP_SCR[i]+
      S$Sim$NPP_Fruit[i]+S$Sim$NPP_Leaf[i]+S$Sim$NPP_FRoot[i]

  }
  CycleList=list(Sim= S$Sim%>%as.data.frame)
  CycleList
}
