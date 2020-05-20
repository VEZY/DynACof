#' @title Dynamic Agroforestry Coffee Crop Model
#' @description   The DynACof process-based model computes plot-scale Net Primary Productivity, carbon allocation, growth, yield,
#'                energy, and water balance of coffee plantations according to management, while accounting for spatial effects using
#'                metamodels from the 3D process-based model \href{https://maespa.github.io/}{MAESPA}. The model also uses cohorts for
#'                the development of the coffee buds and fruits to better represent fruit carbon demand distribution along the year.
#' @param Period   Period of time to be simulated, see details. Default: `NULL`
#' @param WriteIt  If `TRUE`, write the outputs to disk using [write.results()], see details. Default: `FALSE`
#' @param parallel Boolean. Parallelize the computation over crop rotations.
#' @param ...      Further arguments to pass to [write.results()].
#' @param output_f Output format. If `output_f = ".RData"`, the output list will be saved as a unique `.RData` file. Any other value:
#'                 write the output list in several `.csv` and `.txt` files. Default: `.RData`
#' @param Inpath   Path to the input parameter list folder, Default: `NULL` (take package values)
#' @param Outpath  Path pointing to the folder were the results will be writen, Default: `Outpath = Inpath`
#' @param Simulation_Name Character name of the simulation file name if `WriteIt = T`. Default: `"DynACof"`
#' @param FileName A list of input file names :
#' \describe{
#'   \item{Site}{Site parameters file name, see details. Default: `'1-Site.R'`}
#'   \item{Meteo}{Meteo parameters file name, see details. Default: `'2-Meteorology.txt'`}
#'   \item{Soil}{Soil parameters file name, see details. Default: `'3-Soil.R'`}
#'   \item{Coffee}{Coffee parameters file name, see details. Default: `'4-Coffee.R'`}
#'   \item{Tree}{Shade tree parameters file name, see details. Default: `NULL`}
#' }
#' Default input files are provided with the package as an example parameterization.
#'
#' @return Return invisibly a list containing three objects (Parameters, Meteo and Sim):
#'
#' * Sim: A data.frame of the simulation outputs at daily time-step:
#' \tabular{llll}{
#' *Type*                       \tab *Var*                    \tab *unit*              \tab *Definition*                                                                \cr
#' General                      \tab Cycle                    \tab -                   \tab Plantation cycle ID                                                                \cr
#'                              \tab Plot_Age                 \tab year                \tab Plantation age (starting at 1)                                                     \cr
#'                              \tab Plot_Age_num             \tab year (numeric)      \tab Numeric age of plantation                                                          \cr
#'                              \tab LAIplot                  \tab m2 leaves m-2 soil  \tab Plot (Coffee + Shade Tree if any) Leaf Area Index                                  \cr
#' Suffixes for Coffee organs   \tab x_RE                     \tab -                   \tab Reserves                                                                           \cr
#'                              \tab x_SCR                    \tab -                   \tab Stump and Coarse roots                                                             \cr
#'                              \tab x_Fruit                  \tab -                   \tab Fruit                                                                              \cr
#'                              \tab x_Shoot                  \tab -                   \tab Resprout wood (= branches)                                                         \cr
#'                              \tab x_FRoot                  \tab -                   \tab Fine roots                                                                         \cr
#'                              \tab x_Leaf                   \tab                     \tab Leaves                                                                             \cr
#' Suffixes for Shade Tree org. \tab x_RE_Tree                \tab -                   \tab Reserves                                                                           \cr
#'                              \tab x_Stem_Tree              \tab -                   \tab Stem (= trunk)                                                                     \cr
#'                              \tab x_Branch_Tree            \tab -                   \tab Branches                                                                           \cr
#'                              \tab x_CoarseRoot_Tree        \tab -                   \tab Coarse roots                                                                       \cr
#'                              \tab x_FRoot_Tree             \tab -                   \tab Fine roots                                                                         \cr
#'                              \tab x_Leaf_Tree              \tab                     \tab Leaves                                                                             \cr
#' Energy                       \tab Rn_tot                   \tab MJ m-2 d-1          \tab System net radiation                                                               \cr
#'                              \tab Rn_Tree                  \tab MJ m-2 d-1          \tab Shade tree net radiation                                                           \cr
#'                              \tab Rn_Coffee                \tab MJ m-2 d-1          \tab Coffee net radiation                                                               \cr
#'                              \tab Rn_Soil                  \tab MJ m-2 d-1          \tab Soil net radiation                                                                 \cr
#'                              \tab Rn_Soil_SW               \tab MJ m-2 d-1          \tab Soil net radiation computed using Shuttleworth & Wallace (1985) for reference      \cr
#'                              \tab LE_x                     \tab MJ m-2 d-1          \tab System/Coffee/Tree/Soil latent heat                                                \cr
#'                              \tab H_x                      \tab MJ m-2 d-1          \tab System/Coffee/Tree/Soil sensible heat                                              \cr
#'                              \tab Q_Soil                   \tab MJ m-2 d-1          \tab Soil heat transport                                                                \cr
#'                              \tab Transmittance_Tree       \tab fraction            \tab Fraction of light transmitted by the shade trees                                   \cr
#'                              \tab PAR_Trans_Tree           \tab MJ m-2 d-1          \tab Light transmitted by the shade trees canopy                                        \cr
#'                              \tab PAR_Trans                \tab MJ m-2 d-1          \tab Light transmitted by the Coffea canopy                                             \cr
#'                              \tab K_Dir                    \tab -                   \tab Direct light extinction coefficient                                                \cr
#'                              \tab K_Dif                    \tab -                   \tab Diffuse light extinction coefficient                                               \cr
#'                              \tab APAR                     \tab MJ m-2 d-1          \tab Absorbed PAR by the plant                                                          \cr
#'                              \tab APAR_Dif                 \tab MJ m-2 d-1          \tab Absorbed diffuse PAR (Direct is APAR-APAR_Dif)                                     \cr
#'                              \tab lue                      \tab gC MJ               \tab Light use efficiency                                                               \cr
#'                              \tab Tleaf_Coffee             \tab deg C               \tab Coffee canopy temperature computed by DynACof                                      \cr
#'                              \tab WindSpeed_x              \tab m s-1               \tab Wind speed at the center of the layer                                              \cr
#'                              \tab TairCanopy_x             \tab deg C               \tab Air tempetature at the center of the layer                                         \cr
#'                              \tab DegreeDays_Tcan          \tab deg C               \tab Growing degree days computed using Coffee Canopy Temperature                       \cr
#' Carbon                       \tab GPP                      \tab gC m-2 d-1          \tab Gross primary productivity                                                         \cr
#'                              \tab Consumption_RE           \tab gC m-2 d-1          \tab Daily reserve consumption                                                          \cr
#'                              \tab Carbon_Lack_Mortality    \tab gC m-2 d-1          \tab Mortality from a higher carbon consumption than Supply                             \cr
#'                              \tab Rm                       \tab gC m-2 d-1          \tab Total Coffee maintenance respiration                                               \cr
#'                              \tab Rm_x                     \tab gC m-2 d-1          \tab Maintenance respiration at organ scale                                             \cr
#'                              \tab Rg                       \tab gC m-2 d-1          \tab Total Coffee growth respiration                                                    \cr
#'                              \tab Rg_x                     \tab gC m-2 d-1          \tab Growth respiration at organ scale                                                  \cr
#'                              \tab Ra                       \tab gC m-2 d-1          \tab Coffee layer autotrophic respiration (=Rm+Rg)                                      \cr
#'                              \tab Demand_x                 \tab gC m-2 d-1          \tab C demand at organ scale (fruit, leaf and fine root only)                           \cr
#'                              \tab Alloc_x                  \tab gC m-2 d-1          \tab C allocation to organ net of Rm (NPP+Rg)                                           \cr
#'                              \tab Supply                   \tab gC m-2 d-1          \tab C supply at the begining of the day at layer scale (GPP+Reserve consumption-Rm)    \cr
#'                              \tab Supply_x                 \tab gC m-2 d-1          \tab C supply to organ, net of Rm                                                       \cr
#'                              \tab NPP                      \tab gC m-2 d-1          \tab Net primary productivity at layer scale                                            \cr
#'                              \tab NPP_x                    \tab gC m-2 d-1          \tab Net primary productivity at organ scale                                            \cr
#'                              \tab Mnat_x                   \tab gC m-2 d-1          \tab Organ natural mortality (= due to lifespan)                                        \cr
#'                              \tab Mprun_x                  \tab gC m-2 d-1          \tab Organ mortality due to pruning                                                     \cr
#'                              \tab M_ALS                    \tab gC m-2 d-1          \tab Coffee leaf mortality from American Leaf Spot                                      \cr
#'                              \tab Mortality_x              \tab gC m-2 d-1          \tab Total organ mortality                                                              \cr
#'                              \tab LAI                      \tab m2 leaves m-2 soil  \tab Leaf Area Index                                                                    \cr
#'                              \tab CM_x                     \tab gC m-2 d-1          \tab Organ C mass                                                                       \cr
#'                              \tab DM_x                     \tab gDM m-2 d-1         \tab Organ dry mass                                                                     \cr
#' Fruit development            \tab BudInitPeriod            \tab boolean             \tab Bud initiation period (BIP)                                                        \cr
#'                              \tab Budinit                  \tab Buds d-1            \tab Total Number of Buds Initiated per day                                             \cr
#'                              \tab ratioNodestoLAI          \tab Nodes LAI-1         \tab Number of fruiting nodes per LAI unit                                              \cr
#'                              \tab Temp_cor_Bud             \tab fraction            \tab Temperature correction factor for bud development                                  \cr
#'                              \tab pbreak                   \tab 0-1                 \tab Daily probability of bud dormancy break                                            \cr
#'                              \tab BudBreak                 \tab Buds d-1            \tab Total number of buds breaking dormancy per day                                     \cr
#'                              \tab SM                       \tab g m-2 d-1           \tab Coffee Fruit Sucrose Mass                                                          \cr
#'                              \tab SC                       \tab g Sugar gDM         \tab Coffee Fruit Sucrose Content                                                       \cr
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
#'                              \tab Drain_\[1-3\]            \tab mm                  \tab Water drainage from soil layer 1, 2 or 3                                           \cr
#'                              \tab WSurfaceRes              \tab mm                  \tab Soil water content from the surface layer                                          \cr
#'                              \tab W_tot                    \tab mm                  \tab Total soil profile water content                                                   \cr
#'                              \tab W_\[1-3\]                \tab mm                  \tab Soil water content from the layer 1, 2 or 3                                        \cr
#'                              \tab REW_tot                  \tab -                   \tab Relative extractable water from the soil                                           \cr
#'                              \tab REW_\[1-3\]              \tab -                   \tab Relative extractable water from the layer 1, 2 or 3                                \cr
#'                              \tab EW_tot                   \tab mm                  \tab Extractable water from the soil                                                    \cr
#'                              \tab EW_\[1-3\]               \tab mm                  \tab Extractable water from the layer 1, 2 or 3                                         \cr
#'                              \tab SWD                      \tab mm                  \tab soil water deficit                                                                 \cr
#'                              \tab RootWaterExtract_\[1-3\] \tab mm                  \tab Root water extraction for soil layer 1 to 3                                        \cr
#'                              \tab IntercRevapor            \tab mm                  \tab Evaporation by canopy                                                              \cr
#'                              \tab T_x                      \tab mm                  \tab Transpiration at system/Coffee/Tree scale                                          \cr
#'                              \tab E_Soil                   \tab mm                  \tab Soil evaporation                                                                   \cr
#'                              \tab ETR                      \tab mm                  \tab System evapotranspiration                                                          \cr
#'                              \tab SoilWaterPot             \tab MPa                 \tab Soil water potential                                                               \cr
#'                              \tab PSIL                     \tab Mpa                 \tab Coffee leaf water potential                                                        \cr
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
#'                              \tab MThinning_x_Tree         \tab gc m-2 d-1          \tab Mortality due to thining at organ scale
#'}
#'
#' * Meteo: A data.frame of the input meteorology, potentially coming from the output of [Meteorology()]:
#' \tabular{llll}{
#' *Var*           \tab *unit*      \tab *Definition*                                 \tab *If missing* \cr
#' Date            \tab POSIXct     \tab Date in POSIXct format                       \tab Computed from start date parameter, or set a dummy date if missing \cr
#' year            \tab year        \tab Year of the simulation                       \tab Computed from Date \cr
#' DOY             \tab day         \tab day of the year                              \tab Computed from Date \cr
#' Rain            \tab mm          \tab Rainfall                                     \tab Assume no rain \cr
#' Tair            \tab Celsius     \tab Air temperature (above canopy)               \tab Computed from Tmax and Tmin \cr
#' Tmax            \tab Celsius     \tab Maximum air temperature during the day       \tab Required (error) \cr
#' Tmin            \tab Celsius     \tab Minimum air temperature during the day       \tab Required (error) \cr
#' RH              \tab %           \tab Relative humidity                            \tab Not used, but prefered over VPD for Rn computation \cr
#' RAD             \tab MJ m-2 d-1  \tab Incident shortwave radiation                 \tab Computed from PAR \cr
#' Pressure        \tab hPa         \tab Atmospheric pressure                         \tab Computed from VPD, Tair and Elevation, or alternatively from Tair and Elevation. \cr
#' WindSpeed       \tab m s-1       \tab Wind speed                                   \tab Taken as constant: `Parameters$WindSpeed` \cr
#' CO2             \tab ppm         \tab Atmospheric CO2 concentration                \tab Taken as constant: `Parameters$CO2` \cr
#' DegreeDays      \tab Celsius     \tab Growing degree days                          \tab Computed using [GDD()] \cr
#' PAR             \tab MJ m-2 d-1  \tab Incident photosynthetically active radiation \tab Computed from RAD \cr
#' FDiff           \tab Fraction    \tab Diffuse light fraction                       \tab Computed using [Diffuse_d()] using Spitters et al. (1986) formula \cr
#' VPD             \tab hPa         \tab Vapor pressure deficit                       \tab Computed from RH \cr
#' Rn              \tab MJ m-2 d-1  \tab Net radiation (will be depreciated)          \tab Computed using [Rad_net()] with RH, or VPD \cr
#' DaysWithoutRain \tab day         \tab Number of consecutive days with no rainfall  \tab Computed from Rain \cr
#' Air_Density     \tab kg m-3      \tab Air density of moist air (\eqn{\rho}) above canopy \tab Computed using [bigleaf::air.density()] \cr
#' ZEN             \tab radian      \tab Solar zenithal angle at noon                 \tab Computed from Date, Latitude, Longitude and Timezone
#' }
#'
#' * Parameters: A list of the input parameters (see [site()])
#'
#' @details The user can import a simulation using [base::readRDS()].
#'          Almost all variables for coffee exist also for shade trees with the suffix
#'          `_Tree` after the name of the variable, e.g. : LAI = coffee LAI,
#'          LAI_Tree = shade tree LAI.
#'          Special shade tree variables (see return section) are only optional,
#'          and it may have more variables upon parameterization because variables can be added in
#'          the metamodels parameter file in \strong{[Metamodels()][Light_extinction_K()]} or
#'          \strong{[Allometries()]}.
#'          Important :
#'          It is highly recommended to set the system environment timezone to the one from the meteorology file.
#'          For example the default meteorology file ([Aquiares()]) has to be set to `Sys.setenv(TZ="UTC")`.
#'
#' @note All variable units are available as attributes (see example).
#'
#' For simulations with custom initialisations (*e.g.* at age > 0), or running a simulation day by day, see [dynacof_i()].
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
#' @seealso [Meteorology()] [site()]
#' @importFrom bigleaf air.density
#' @importFrom dplyr n
#' @importFrom foreach %dopar%
#' @importFrom methods is new
#' @importFrom doParallel registerDoParallel
#' @importFrom crayon red green bold underline
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
DynACof= function(Period=NULL, WriteIt= F,...,parallel= TRUE,
                  output_f=".RData",Inpath=NULL,Outpath=Inpath,Simulation_Name="DynACof",
                  FileName= list(Site="1-Site.R",Meteo=NULL,Soil="3-Soil.R",
                                 Coffee="4-Coffee.R",Tree=NULL)){

  Cycle= Plot_Age= cy= varnames= NULL # to avoid check notes

  # Importing the parameters ------------------------------------------------

  Parameters= Import_Parameters(path = Inpath, Names= FileName[-grep("Meteo",names(FileName))])

  test_parameters(Parameters, isTree= !is.null(FileName$Tree))

  # Importing the meteo -----------------------------------------------------
  meteo_path=
    if(!is.null(FileName$Meteo)){
      file.path(Inpath,FileName$Meteo)
    }else{
      NULL
    }

  Meteo= Meteorology(file= meteo_path, Period= Period,Parameters= Parameters)
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

  if(NCycles>1&parallel){
    # Setting the parallel computation over cycles: ---------------------------

    # Set the maximum number of cores working on the model computation
    NbCores= parallel::detectCores()-1
    cl= parallel::makeCluster(min(NbCores,NCycles))
    doParallel::registerDoParallel(cl)

    Table= foreach::foreach(cy= 1:NCycles,.combine=rbind,
                            .packages = c("dplyr","zoo")) %dopar% {

                              mainfun(cy,Direction,Meteo,Parameters)

                            }
    parallel::stopCluster(cl)

  }else{
    CycleList=
      lapply(1:NCycles, function(x){
        mainfun(x,Direction,Meteo,Parameters)
      })
    Table= dplyr::bind_rows(CycleList)
  }

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
#' is called by [DynACof()] under the hood, and users should always call
#' [DynACof()] instead of this function because it imports the files,
#' format the simulation inputs, checks for errors, takes care automatically
#' of the computation distribution along nodes, and format the outputs.
#'
#' @param cy         The growing cycle (crop rotation)
#' @param Direction  Simulation directives as a data.frame
#' @param Meteo      Meteorology data.frame
#' @param Parameters Simulation parameters
#'
#' @details The Direction `data.frame` has to contain at least one column
#' named Cycle that denotes the crop rotation, Plot_Age, an integer for the plot age,
#' Plot_Age_num for the plot age as a continuous variable (see example).
#'
#' @return The simulation output as a data.frame.
#'
mainfun= function(cy,Direction,Meteo,Parameters){

  .= NULL
  # Initializing the Simulation object:

  S= SimulationClass$new()
  S$Parameters= Parameters

  # Initializing the table:
  S$Sim= as.list(Direction[Direction$Cycle==cy,])
  S$Met_c= as.list(Meteo[Direction$Cycle==cy,])

  Init_Sim(S)
  bud_init_period(S)

  S$Sim$BudInitPeriod= S$Sim$BudInitPeriod[1:length(S$Sim$Cycle)]

  S$Sim$ALS=
    suppressMessages(ALS(Elevation= S$Parameters$Elevation, SlopeAzimut= S$Parameters$SlopeAzimut,
                         Slope= S$Parameters$Slope, RowDistance= S$Parameters$RowDistance,
                         Shade= S$Parameters$Shade, CanopyHeight.Coffee= S$Parameters$Height_Coffee,
                         Fertilization= S$Parameters$Fertilization, ShadeType= S$Parameters$ShadeType,
                         CoffeePruning= S$Parameters$CoffeePruning,
                         df_rain= data.frame(year=S$Met_c$year[1:length(S$Sim$Cycle)],
                                             DOY=S$Met_c$DOY[1:length(S$Sim$Cycle)],
                                             Rain=S$Met_c$Rain[1:length(S$Sim$Cycle)])))

  # Main Loop -----------------------------------------------------------------------------------

  pb= txtProgressBar(max= length(S$Sim$LAI), style=3)

  for (i in 1:length(S$Sim$LAI)){

    setTxtProgressBar(pb, i)

    energy_water_models(S,i) # the soil is in here also
    # Shade Tree computation if any
    if (S$Sim$Stocking_Tree[i] > 0.0){
      tree_model(S,i)
    }
    coffee_model(S,i)
  }
  return(S$Sim%>%as.data.frame)
}



#' Step-by-step DynACof
#'
#' @description Using DynACof one iteration after another. Allows to run a DynACof simulation step by step to
#' e.g. modify a variable simulated by DynACof using another model for model coupling
#'
#' @param i Either an integer, or a range giving the day of simulation needed. Match the row index, so `i=1` make
#' a simulation for the first row of Sim and Met (i.e. the first day).
#' @param S The simulation list (see [DynACof()]).
#' @param verbose Boolean. Prints progress bar if `TRUE` (default).
#' @param Period (Initalization) The maximum period that will be simulated (given at initialization)
#' @param Inpath (Initalization) Path to the input parameter list folder, Default: `NULL` (take package values)
#' @param FileName (Initalization) A list of input file names :
#' \describe{
#'   \item{Site}{Site parameters file name, see details. Default: `'1-Site.R'`}
#'   \item{Meteo}{Meteo parameters file name, see details. Default: `'2-Meteorology.txt'`}
#'   \item{Soil}{Soil parameters file name, see details. Default: `'3-Soil.R'`}
#'   \item{Coffee}{Coffee parameters file name, see details. Default: `'4-Coffee.R'`}
#'   \item{Tree}{Shade tree parameters file name, see details. Default: `NULL`}
#' }
#' Default input files are provided with the package as an example parameterization.
#'
#' @return Either an initialized simulation list (if S is null) or rhe modified simulation list `S`
#' @export
#'
#' @examples
#'\dontrun{
#' # First, initialize a simulation:
#' S= dynacof_i(1:100,Period= as.POSIXct(c("1979-01-01", "1980-12-31")))
#'
#' # Then, compute the simulation for the next day:
#' S= dynacof_i(101,S)
#'
#' # We can modifiy the value of some variables before computing the next day and compare with
#' # unmodified value:
#'
#' # Make a copy of the simulation list:
#' S2= S
#'
#' # Changing the value of Tair in the meteorology for day 102 for S2:
#' S2$Meteo$Tair[102]= S2$Meteo$Tair[102]+10.0
#'
#' # Make a computation for each:
#' S= dynacof_i(102,S)
#' S2= dynacof_i(102,S2)
#'
#' # Compare the values of e.g. the maitenance respiration:
#' S$Sim$Rm[102]
#' S2$Sim$Rm[102]
#'
#' # To run DynACof for several days, use a range for i:
#' S= dynacof_i(102:nrow(S$Meteo),S)
#' # NB: S$Meteo is the maximum length we can simulate. To increase a simulation, initialize it
#' # with a wider range for the "Period" argument.
#'
#'}
dynacof_i= function(i,S=NULL,verbose= TRUE,Period=NULL,Inpath=NULL,
                    FileName= list(Site="1-Site.R",Meteo=NULL,Soil="3-Soil.R",
                                   Coffee="4-Coffee.R",Tree=NULL)){

  if(is.null(S)){
    # S is not provided, initializing a simulation
    message(paste("\n", crayon::green$bold$underline("Starting simulation initialization"),"\n"))

    Parameters= Import_Parameters(path = Inpath, Names= FileName[-grep("Meteo",names(FileName))])

    test_parameters(Parameters, isTree= !is.null(FileName$Tree))

    # Importing the meteo -----------------------------------------------------
    meteo_path=
      if(!is.null(FileName$Meteo)){
        file.path(Inpath,FileName$Meteo)
      }else{
        NULL
      }

    Meteo= Meteorology(file= meteo_path, Period= Period,Parameters= Parameters)
    Parameters$files$Meteorology= file.path(Inpath,FileName$Meteo) # save the meteo file path


    # Setting the simulation --------------------------------------------------

    # Number of cycles (rotations) to do over the period (given by the Meteo file):
    NCycles= ceiling((max(Meteo$year)-min(Meteo$year))/Parameters$AgeCoffeeMax)

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

    CycleList=
      lapply(1:NCycles, function(x){
        mainfun(cy = x, Direction = Direction[i,],Meteo,Parameters)
      })

    message(paste("\n", crayon::green$bold$underline("Simulation initialization completed"),"\n"))

    return(list(Sim= dplyr::bind_rows(CycleList),Meteo= Meteo, Parameters= Parameters))
  }else{
    # S is provided, the user wants to simulate by steps starting from there.

    # Checking that S was not already simulated for the "i" requested because it is not allowed due
    # to some variables that are cumulatively computed.

    if(min(i)<nrow(S$Sim)){
      stop(paste(crayon::red$bold$underline("Index or range requested ('i') was already simulated."),
                 "Please provide an index/range starting from",nrow(S$Sim)+1))
    }

    # Computing the directions:
    # Number of cycles (rotations) to do over the period (given by the Meteo file):
    NCycles= ceiling((max(S$Meteo$year[1:max(i)])-min(S$Meteo$year[1:max(i)]))/
                       S$Parameters$AgeCoffeeMax)

    #Day number and Years After Plantation
    ndaysYear= sapply(X= unique(S$Meteo$year[1:max(i)]), FUN= function(x){
      length(Meteo$year[1:max(i)][S$Meteo$year[1:max(i)]==x])})

    Direction= data.frame(
      Cycle= rep.int(rep(1:NCycles, each= S$Parameters$AgeCoffeeMax)[1:length(unique(S$Meteo$year[1:max(i)]))],
                     times= ndaysYear),
      Plot_Age= rep.int(rep_len(seq(S$Parameters$AgeCoffeeMin,S$Parameters$AgeCoffeeMax),
                                length.out= length(unique(S$Meteo$year[1:max(i)]))),times= ndaysYear))
    Direction%<>%
      group_by(Cycle,Plot_Age)%>%
      mutate(Plot_Age_num= seq(min(Plot_Age),min(Plot_Age)+1, length.out= n()))%>%ungroup()

    # Initializing the table:

    Z= SimulationClass$new()
    Z$Parameters= S$Parameters
    Z$Met_c= as.list(S$Meteo)

    Z$Sim= as.list(Direction)

    Init_Sim(Z)
    bud_init_period(Z)
    Z$Sim$BudInitPeriod= Z$Sim$BudInitPeriod[1:length(Z$Sim$Cycle)]

    Z$Sim$ALS=
      suppressMessages(ALS(Elevation= Z$Parameters$Elevation, SlopeAzimut= Z$Parameters$SlopeAzimut,
                           Slope= Z$Parameters$Slope, RowDistance= Z$Parameters$RowDistance,
                           Shade= Z$Parameters$Shade, CanopyHeight.Coffee= Z$Parameters$Height_Coffee,
                           Fertilization= Z$Parameters$Fertilization, ShadeType= Z$Parameters$ShadeType,
                           CoffeePruning= Z$Parameters$CoffeePruning,
                           df_rain= data.frame(year=Z$Met_c$year[1:length(Z$Sim$Cycle)],
                                               DOY=Z$Met_c$DOY[1:length(Z$Sim$Cycle)],
                                               Rain=Z$Met_c$Rain[1:length(Z$Sim$Cycle)])))


    Zsim_df= as.data.frame(Z$Sim)
    Zsim_df[1:nrow(S$Sim),]= S$Sim

    Z$Sim= as.list(Zsim_df)
    Z$Met_c= as.list(S$Meteo)

    # Main Loop -----------------------------------------------------------------------------------

    if(verbose){pb= txtProgressBar(max= max(i), style=3)}

    for (j in i){
      if(verbose){setTxtProgressBar(pb, j)}
      energy_water_models(Z,j) # the soil is in here also
      # Shade Tree computation if any
      if(Z$Sim$Stocking_Tree[j] > 0.0){
        tree_model(Z,j)
      }
      # LE_Tree (sum of transpiration + leaf evap)
      coffee_model(Z,j)
    }

    S$Sim= as.data.frame(Z$Sim)
  }

  return(S)
}
