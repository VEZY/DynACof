#' @title Dynamic Agroforestry Coffee Crop Model
#' @description   The DynACof process-based model computes plot-scale Net Primary Productivity, carbon allocation, growth, yield,
#'                energy, and water balance of coffee plantations according to management, while accounting for spatial effects using
#'                metamodels from the 3D process-based MAESPA. The model also uses coffee bud and fruit cohorts for reproductive
#'                development to better represent fruit carbon demand distribution along the year.
#' @param Period   Period of time to be simulated, see details. Default: \code{NULL}
#' @param WriteIt  If \code{TRUE}, write the resulting list, see details. Default: \code{FALSE}
#' @param ...      PARAM_DESCRIPTION
#' @param output_f Output format, if '.RData', the output list will be saved as a unique \code{.RData} file, if it is different from
#'                 \code{.RData}, the output list will be writen in several \code{.csv} and \code{.txt} format. Default: \code{.RData}
#' @param Inpath   Path to the input parameter list folder, Default: \code{"1-Input/Default"}
#' @param Outpath  Path pointing to the folder were the results will be writen, Default: \code{=Inpath}
#' @param Simulation_Name Character name of the simulation file name if \code{WriteIt=T}. Default: \code{DynACof}
#' @param FileName A list of input file names :
#' \describe{
#'   \item{Site}{Site parameters file name, see details. Default: \code{'1-Site.R'}}
#'   \item{Meteo}{Meteo parameters file name, see details. Default: \code{'2-Meteorology.txt'}}
#'   \item{Soil}{Soil parameters file name, see details. Default: \code{'3-Soil.R'}}
#'   \item{Coffee}{Coffee parameters file name, see details. Default: \code{'4-Coffee.R'}}
#'   \item{Tree}{Shade tree parameters file name, see details. Default: \code{NULL}}
#' }
#' Default input filenames are provided with the package so the model can run with no input parameter files at all.
#'
#' @return Return invisibly a list containing three objects (Parameters, Meteo and Sim):
#' \itemize{
#'   \item A data.frame of the simulation outputs at daily time-step: \tabular{llll}{
#' \strong{Type} \tab \strong{Var} \tab \strong{unit} \tab \strong{Definition}\cr
#' General                      \tab Cycle                    \tab -                   \tab Plantation cycle ID                                                                \cr
#'                              \tab Plot_Age                 \tab year                \tab Plantation age                                                                     \cr
#'                              \tab Plot_Age_num             \tab year (numeric)      \tab Numeric age of plantation                                                          \cr
#'                              \tab LAIplot                  \tab m2 leaves m-2 soil  \tab Plot (Coffee + Shade Tree if any) Leaf Area Index                                  \cr
#' Organs Coffee                \tab *_RE                     \tab -                   \tab Reserves                                                                           \cr
#'                              \tab *_SCR                    \tab -                   \tab Stump and Coarse roots                                                             \cr
#'                              \tab *_Fruit                  \tab -                   \tab Fruit                                                                              \cr
#'                              \tab *_RsWood                 \tab -                   \tab Resprout wood (= branches)                                                         \cr
#'                              \tab *_FRoot                  \tab -                   \tab Fine roots                                                                         \cr
#'                              \tab *_Leaf                   \tab                     \tab Leaves                                                                             \cr
#' Organs shade tree            \tab *_RE_Tree                \tab -                   \tab Reserves                                                                           \cr
#'                              \tab *_Stem_Tree              \tab -                   \tab Stem (= trunk)                                                                     \cr
#'                              \tab *_Branch_Tree            \tab -                   \tab Branches                                                                           \cr
#'                              \tab *_CoarseRoot_Tree        \tab -                   \tab Coarse roots                                                                       \cr
#'                              \tab *_FRoot_Tree             \tab -                   \tab Fine roots                                                                         \cr
#'                              \tab *_Leaf_Tree              \tab                     \tab Leaves                                                                             \cr
#' Energy                       \tab Rn_tot                   \tab MJ m-2 d-1          \tab System net radiation                                                               \cr
#'                              \tab Rn_Tree                  \tab MJ m-2 d-1          \tab Shade tree net radiation                                                           \cr
#'                              \tab Rn_Coffee                \tab MJ m-2 d-1          \tab Coffee net radiation                                                               \cr
#'                              \tab Rn_Soil                  \tab MJ m-2 d-1          \tab Soil net radiation                                                                 \cr
#'                              \tab LE_*                     \tab MJ m-2 d-1          \tab System/Coffee/Tree/Soil latent heat                                                \cr
#'                              \tab H_*                      \tab MJ m-2 d-1          \tab System/Coffee/Tree/Soil sensible heat                                              \cr
#'                              \tab Q_Soil                   \tab MJ m-2 d-1          \tab Soil heat transport                                                                \cr
#'                              \tab Transmittance_Tree       \tab fraction            \tab Light transmitted under the shade trees canopy                                     \cr
#'                              \tab K_Dir                    \tab -                   \tab Direct light extinction coefficient                                                \cr
#'                              \tab K_Dif                    \tab -                   \tab Diffuse light extinction coefficient                                               \cr
#'                              \tab APAR                     \tab MJ m-2 d-1          \tab Absorbed PAR by the plant                                                          \cr
#'                              \tab APAR_Dif                 \tab MJ m-2 d-1          \tab Absorbed diffuse PAR (Direct is APAR-APAR_Dif)                                     \cr
#'                              \tab lue                      \tab gC MJ               \tab Light use efficiency                                                               \cr
#'                              \tab Tcan_MAESPA_Coffee       \tab deg C               \tab Coffee canopy temperature computed using MAESPA metamodel                          \cr
#'                              \tab Tcan_Coffee              \tab deg C               \tab Coffee canopy temperature computed by DynACof                                      \cr
#'                              \tab WindSpeed_*              \tab m s-1               \tab Wind speed at the center of the layer                                              \cr
#'                              \tab TairCanopy_*             \tab deg C               \tab Air tempetature at the center of the layer                                         \cr
#'                              \tab DegreeDays_Tcan          \tab deg C               \tab Growing degree days computed using Coffee Canopy Temperature                       \cr
#' Carbon                       \tab GPP                      \tab gC m-2 d-1          \tab Gross primary productivity                                                         \cr
#'                              \tab Consumption_RE           \tab gC m-2 d-1          \tab Daily reserve consumption                                                          \cr
#'                              \tab Carbon_Lack_Mortality    \tab gC m-2 d-1          \tab Mortality from a higher carbon consumption than carbon offer                       \cr
#'                              \tab Rm                       \tab gC m-2 d-1          \tab Total Coffee maintenance respiration                                               \cr
#'                              \tab Rm_*                     \tab gC m-2 d-1          \tab Maintenance respiration at organ scale                                             \cr
#'                              \tab Rc                       \tab gC m-2 d-1          \tab Total Coffee growth respiration                                                    \cr
#'                              \tab Rc_*                     \tab gC m-2 d-1          \tab Growth respiration at organ scale                                                  \cr
#'                              \tab Ra                       \tab gC m-2 d-1          \tab Coffee layer autotrophic respiration (=maintenance+growth)                         \cr
#'                              \tab Demand_*                 \tab gC m-2 d-1          \tab C demand at organ scale (fruit, leaf and fine root only)                           \cr
#'                              \tab Alloc_*                  \tab gC m-2 d-1          \tab C allocation to organ net of Rm (NPP+Rc)                                           \cr
#'                              \tab lambdaSCRage             \tab gC gC-1             \tab Age related allocation coefficient to Stump and Coars Roots                        \cr
#'                              \tab Offer                    \tab gC m-2 d-1          \tab C offer at the begining of the day at layer scale (GPP+Reserve consumption-Rm)     \cr
#'                              \tab Offer_*                  \tab gC m-2 d-1          \tab C offer to organ net of Rm                                                         \cr
#'                              \tab Demand_*                 \tab gC m-2 d-1          \tab Total C demand from organ                                                          \cr
#'                              \tab NPP                      \tab gC m-2 d-1          \tab Net primary productivity at layer scale                                            \cr
#'                              \tab NPP_*                    \tab gC m-2 d-1          \tab Net primary productivity at organ scale                                            \cr
#'                              \tab Mnat_*                   \tab gC m-2 d-1          \tab Organ natural mortality (= due to lifespan)                                        \cr
#'                              \tab Mprun_*                  \tab gC m-2 d-1          \tab Organ mortality due to pruning                                                     \cr
#'                              \tab M_ALS                     \tab gC m-2 d-1          \tab Coffee leaf mortality from American Leaf Spot                                      \cr
#'                              \tab Mortality_*              \tab gC m-2 d-1          \tab Total organ mortality                                                              \cr
#'                              \tab LAI                      \tab m2 leaves m-2 soil  \tab Leaf Area Index                                                                    \cr
#'                              \tab CM_*                     \tab gC m-2 d-1          \tab Organ C mass                                                                       \cr
#'                              \tab DM_*                     \tab gDM m-2 d-1         \tab Organ dry mass                                                                     \cr
#'                              \tab Height_Tree              \tab m                   \tab Shade tree total height (used for boundary conductance), set to 0 if no shade trees\cr
#' Fruit development            \tab BudInitPeriod            \tab boolean             \tab Bud initiation period                                                              \cr
#'                              \tab Budinit                  \tab Buds d-1            \tab Total Number of Buds Initiated per day                                             \cr
#'                              \tab ratioNodestoLAI          \tab Nodes LAI-1         \tab Number of fruiting nodes per LAI unit                                              \cr
#'                              \tab Temp_cor_Bud             \tab fraction            \tab Temperature correction factor for bud development                                  \cr
#'                              \tab p_budbreakperday         \tab 0-1                 \tab Daily probability of bud dormancy break                                            \cr
#'                              \tab BudBreak                 \tab Buds d-1            \tab Total number of buds breaking dormancy per day                                     \cr
#'                              \tab Sucrose_Mass             \tab g m-2 d-1           \tab Coffee Fruit Sucrose Mass                                                          \cr
#'                              \tab Sucrose_Content          \tab g Sugar gDM         \tab Coffee Fruit Sucrose Content                                                       \cr
#'                              \tab Maturation_duration    \tab days Fruit cohort-1 \tab Coffee Fruit Total Maturation Duration for each cohort                             \cr
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
#'                              \tab DBH_Tree_m               \tab m                   \tab Diameter at breast height                                                          \cr
#'                              \tab LAD_Tree                 \tab m2 m-3              \tab Shade tree Leaf Area Density                                                       \cr
#'                              \tab CrownRad_Tree            \tab m                   \tab Crown radius                                                                       \cr
#'                              \tab CrownProj_Tree           \tab m2 crown tree-1     \tab Crown projection                                                                   \cr
#'                              \tab Stocking_Tree            \tab tree m-2            \tab Shade tree density                                                                 \cr
#'                              \tab TimetoThin_Tree          \tab boolean             \tab Is it time to thin the shade tree                                                  \cr
#'                              \tab MThinning_*_Tree         \tab gc m-2 d-1          \tab Mortality due to thining at organ scale
#'}
#'
#'   \item A data.frame of the input meteorology, potentially coming from the output of \code{\link{Meteorology}}: \tabular{llll}{\strong{Var} \tab \strong{unit} \tab \strong{Definition} \tab \strong{If missing} \cr
#' Date            \tab POSIXct date\tab Date in POSIXct format                       \tab Computed from start date parameter, or set a dummy date if missing\cr
#' year            \tab year        \tab Year of the simulation                       \tab Computed from Date \cr
#' DOY             \tab day         \tab day of the year                              \tab Computed from Date \cr
#' Rain            \tab mm          \tab Rainfall                                     \tab Assume no rain \cr
#' Tair            \tab deg C       \tab Air temperature (above canopy)               \tab Computed from Tmax and Tmin \cr
#' Tmax            \tab deg C       \tab Maximum air temperature durnig the day       \tab Required (error) \cr
#' Tmin            \tab deg C       \tab Minimum air temperature durnig the day       \tab Required (error) \cr
#' RH              \tab \%          \tab Relative humidity                            \tab Not used, but prefered over VPD for Rn computation \cr
#' RAD             \tab MJ m-2 d-1  \tab Incident shortwave radiation                 \tab Computed from PAR \cr
#' Pressure        \tab hPa         \tab Atmospheric pressure                         \tab Try to compute from VPD, Tair and Elevation, or Tair and Elevation. \cr
#' WindSpeed       \tab m s-1       \tab Wind speed                                   \tab Try to set it to constant: \code{Parameters$WindSpeed} \cr
#' CO2             \tab ppm         \tab Atmospheric CO2 concentration                \tab Try to set it to constant: \code{Parameters$CO2}\cr
#' DegreeDays      \tab deg C       \tab Growing degrre days                          \tab Computed using \code{\link{GDD}} \cr
#' PAR             \tab MJ m-2 d-1  \tab Incident photosynthetically active radiation \tab Computed from RAD \cr
#' FDiff           \tab Fraction    \tab Diffuse light fraction                       \tab Computed using \code{\link{Diffuse_d}} usinf Spitters formula \cr
#' VPD             \tab hPa         \tab Vapor pressure deficit                       \tab Computed from RH \cr
#' Rn              \tab MJ m-2 d-1  \tab Net radiation (will soon be depreciated)     \tab Computed using \code{\link{Rad_net}} with RH, or VPD \cr
#' DaysWithoutRain \tab day         \tab Number of consecutive days with no rainfall  \tab Computed from Rain \cr
#' Air_Density     \tab kg m-3      \tab Air density of moist air (\eqn{\rho}) above canopy \tab Computed using \code{\link[bigleaf]{air.density}}}
#'   \item A list of the input parameters (see \code{\link{site}})
#' }
#'
#' @author R. Vezy; O. Roupsard
#' @details Almost all variables for coffee exist also for shade trees with the suffix
#'          \code{_Tree} after the name of the variable, e.g. : LAI = coffee LAI,
#'          LAI_Tree = shade tree LAI.
#'          Special shade tree variables (see return section) are only optional,
#'          and it may have more variables upon parameterization because variables can be added in
#'          the metamodels parameter file in \strong{\code{\link{Metamodels}}} or
#'          \strong{\code{\link{Allometries}}}.
#'          Important :
#'          It is highly recommended to set the system environment timezone to the one from the meteorology file.
#'          For example the default meteorology file (\code{\link{Aquiares}}) has to be set to \code{Sys.setenv(TZ="UTC")}.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  Sys.setenv(TZ="UTC")
#'  DynACof(Period= as.POSIXct(c("1979-01-01", "1980-12-31")))
#'
#'  # Get the units of the input variables:
#'  attr(S$Met_c,"unit")
#'
#'  # Get the units of the output variables:
#'  attr(S$Sim,"unit")
#'  }
#' }
#' @export
#' @rdname DynACof
#' @seealso \code{\link{Meteorology}} \code{\link{site}}
#' @importFrom bigleaf air.density
#' @importFrom dplyr n
#' @importFrom foreach %dopar%
#' @importFrom methods is new
#' @importFrom doParallel registerDoParallel
#'
DynACof= function(Period=NULL, WriteIt= F,...,
                  output_f=".RData",Inpath=NULL,Outpath=Inpath,Simulation_Name="DynACof",
                  FileName= list(Site="1-Site.R",Meteo="2-Meteorology.txt",Soil="3-Soil.R",
                                 Coffee="4-Coffee.R",Tree=NULL)){


  # Importing the parameters ------------------------------------------------

  Parameters= Import_Parameters(path = Inpath, Names= FileName[-grep("Meteo",FileName)])

  # Importing the meteo -----------------------------------------------------

  Meteo= Meteorology(file= file.path(Inpath,FileName$Meteo),Period= Period,Parameters= Parameters)
  Parameters$files$Meteorology= file.path(Inpath,FileName$Meteo) # save the meteo file path
  ########## Coffee plant Cycles ##########
  # Number of cycles to do over the period (given by the Meteo file):
  NCycles= ceiling((max(Meteo$year)-min(Meteo$year))/Parameters$AgeCoffeeMax)

  ########## TIME ####
  # Create vectors date+semih,Meteo$year, Month, Week, Day of month,DOY : daily file
  #Day number and Years After Plantation
  ndaysYear= sapply(X= unique(Meteo$year), FUN= function(x){
    length(Meteo$year[Meteo$year==x])})
  #To create cycles = Rotations and Plot Age in cycle
  # Direction gives the directions to GO+ : The Cycle and Age of the plot for each day
  Direction= data.frame(
    Cycle= rep.int(rep(1:NCycles, each= Parameters$AgeCoffeeMax)[1:length(unique(Meteo$year))],
                   times= ndaysYear),
    Plot_Age= rep.int(rep_len(seq(Parameters$AgeCoffeeMin,Parameters$AgeCoffeeMax),
                              length.out= length(unique(Meteo$year))),times= ndaysYear))
  Cycle= Plot_Age= cy= .= varnames= NULL # to avoid check notes
  Direction%<>%
    group_by(Cycle,Plot_Age)%>%
    mutate(Plot_Age_num= seq(min(Plot_Age),min(Plot_Age)+1, length.out= n()))%>%ungroup()
  # Variables are reinitialized so each cycle is independant from the others -> mandatory for
  # parallel processing

  message(paste("Starting a simulation from",min(Meteo$Date),"to",max(Meteo$Date),"over",NCycles,
          "plantation cycle(s)"))

  # Parallel loop over cycles:
  NbCores= parallel::detectCores()-1 # Set the maximum number of cores working on the model computation
  cl= parallel::makeCluster(min(NbCores,NCycles))
  doParallel::registerDoParallel(cl)

  CycleList= foreach::foreach(cy= 1:NCycles,.combine=rbind,
                              .packages = c("dplyr","zoo")) %dopar% {
  # pb <- txtProgressBar(max=100, style=3)
  # for(cy in 1:NCycles){


    # Initializing the Simulation object:

    S= SimulationClass$new()
    S$Parameters= Parameters

    # Initializing the table:
    S$Sim= as.list(Direction[Direction$Cycle==cy,])
    S$Met_c= as.list(Meteo[Direction$Cycle==cy,])
    Init_Sim(S)
    # Compute cumulative degree-days based on previous daily DD from semi-hourly data:
    CumulDegreeDays= cumsum(S$Met_c$DegreeDays)
    # Trick to avoid all the ifelse conditions because of the first time-step (evaluated at each
    # time step so increasing computing time):

    ########### Bud induction window computation ####
    # Bud induction can start only at S$Parameters$Tffb degree-days after vegetative growth stops.
    # Source: Rodriguez et al. 2011.
    # The following module finds the vegetative growth end day, and add the Tffb parameter (Time of
    # first floral buds, in dd), then find the very first flowering of the year and set the vector
    # BudInitPeriod to TRUE between the two dates. So buds will appear between plant Tffb parameter
    # and the first flowering day only.

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
      DateBudinit[i]= tail(which(CumsumRelativeToVeget[i,]<S$Parameters$Tffb),1)
      CumsumRelativeToBudinit[i,]=
        CumulDegreeDays-CumulDegreeDays[DateBudinit[i]-1]
      # Minimum date of first bud development end (i.e. without dormancy):
      BudDevelEnd= tail(which(CumsumRelativeToBudinit[i,]<S$Parameters$Budstage1),1)-1
      # Maximum date of first bud development end (i.e. with maximum dormancy):
      MaxDormancy= tail(which(CumsumRelativeToBudinit[i,]<S$Parameters$Budstage2),1)-1
      # Cumulative rainfall within the period of potential dormancy:
      CumRain= cumsum(S$Met_c$Rain[BudDevelEnd:MaxDormancy])
      # Effective (real) day of first buds breaking dormancy:
      BudDormancyBreakDay= BudDevelEnd + sum(CumRain<S$Parameters$RainForBudBreak)-1
      # Effective day of first flowers:
      DateFFlowering[i]=
        tail(which(CumsumRelativeToBudinit[i,]<CumsumRelativeToBudinit[i,BudDormancyBreakDay]+100),1)
      # +100 because it takes 100dd to break dormancy until flowering (Rodriguez et al. 2011)
      # Effective dates between which buds can appear (from DateBudinit until first flowering)
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

    for (i in 1:length(S$Sim$LAI)){


      # Shade Tree computation if any

      Treefun(S,i)
      # Should output at least APAR_Tree, LAI_Tree, T_Tree, Rn_Tree, H_Tree,
      # LE_Tree (sum of transpiration + leaf evap)


      # Coffee computation:

      # Metamodels for K, Paper 2, trained on 2011 only :
      S$Sim$K_Dif[i]= 0.39
      S$Sim$K_Dir[i]= 0.34

      # # Metamodels for K, Paper 3, trained on all ages and structure :
      # S$Sim$K_Dif[i]= 0.40
      # S$Sim$K_Dir[i]= 0.35

      #APAR coffee
      PARcof= S$Met_c$PAR[i]-S$Sim$APAR_Tree[i] # PAR above coffee layer

      S$Sim$APAR_Dif[i]=
        max(0,((S$Met_c$PAR[i]-S$Sim$APAR_Tree[i])*S$Met_c$FDiff[i])*
              (1-exp(-S$Sim$K_Dif[i]*S$Sim$LAI[previous_i(i,1)])))#MJ m-2 d-1
      APAR_Dir= max(0,((S$Met_c$PAR[i]-S$Sim$APAR_Tree[i])*(1-S$Met_c$FDiff[i]))*
                      (1-exp(-S$Sim$K_Dir[i]*S$Sim$LAI[previous_i(i,1)])))#MJ m-2 d-1
      # APAR_Dir is not part of S$Sim because it can be easily computed by
      # S$Met_c$PARm2d1-S$Sim$APAR_Dif
      S$Sim$APAR[i]= APAR_Dir+S$Sim$APAR_Dif[i]
      # S$Sim$APAR[i]= max(0,(S$Met_c$PAR[i]-S$Sim$APAR_Tree[i])*(1-
      # exp(-S$Sim$k[i]*S$Sim$LAI[previous_i(i,1)])))#MJ m-2 d-1

      # Metamodel Coffee Tcanopy, Paper 2
      S$Sim$Tcan_MAESPA_Coffee[i]=
        -0.07741 + 0.99456*S$Met_c$Tair[i] - 0.06948*S$Met_c$VPD[i] -
        1.87975*(1-S$Met_c$FDiff[i]) + 0.19615*PARcof
      S$Sim$DegreeDays_Tcan[i]=
        GDD(Tmean = S$Sim$Tcan_MAESPA_Coffee[i],MinTT = S$Parameters$MinTT,
            MaxTT = S$Parameters$MaxTT)

      # S$Sim$Tcan_Diurnal_Cof[i]=
      #     0.90479 + 0.97384*S$Met_c$Diurnal_TAIR[i] + 0.24677*PARcof + 0.01163*S$Met_c$VPD[i] -
      #     2.53554*(1-S$Met_c$FDiff[i]) - 0.04597*S$Sim$LAI_Tree[i]
      # NB 1: MAESPA simulates a Coffee canopy temperature (= average leaf temperature) very similar
      # to the air temperature within the canopy, so we can use it interchangeably.
      # NB 2: could use sqrt of FBEAM and PARCof for a better fit (little less rmse) but we keep the
      # metamodel without it to decrease the risk of any overfitting or issue on extrapolation.


      # Metamodel Coffee Tcanopy, Paper 3
      # S$Sim$Tcan_MAESPA_Coffee[i]=
      #     0.92921 + 0.95568*S$Met_c$Tair[i] + 0.01241*S$Met_c$VPD[i] -
      #     0.47802*(1-S$Met_c$FDiff[i]) + 0.10599*PARcof-
      #     0.04573*S$Sim$LAI_Tree[i]
      # S$Sim$Tcan_Diurnal_Cof[i]=
      #     0.90479 + 0.97384*S$Met_c$Diurnal_TAIR[i] + 0.24677*PARcof + 0.01163*S$Met_c$VPD[i] -
      #     2.53554*(1-S$Met_c$FDiff[i]) - 0.04597*S$Sim$LAI_Tree[i]
      # NB 1: MAESPA simulates a Coffee canopy temperature (= average leaf temperature) very similar
      # to the air temperature within the canopy, so we can use it interchangeably.
      # NB 2: could use sqrt of FBEAM and PARCof for a better fit (little less rmse) but we keep the
      # metamodel without it to decrease the risk of any overfitting or issue on extrapolation.

      # Metamodel LUE coffee, Paper 2:
      S$Sim$lue[i]=
        2.174236 + 0.012514*S$Met_c$Tair[i] + 0.007653*S$Met_c$VPD[i] -
        1.861276*sqrt(1-S$Met_c$FDiff[i]) - 0.254475*sqrt(PARcof)

      # Metamodel LUE coffee, Paper 3:
      # S$Sim$lue[i]=
      #     1.968619 - 0.128587*PARcof - 1.140032*(1-S$Met_c$FDiff[i]) +
      #     0.001167*S$Met_c$CO2_ppm[i] - 0.012697*S$Sim$Tcan_MAESPA_Coffee[i]
      # S$Sim$lue[i][S$Sim$lue[i]<0.578]= 0.578

      #GPP Coffee
      S$Sim$GPP[i]= S$Sim$lue[i]*S$Sim$APAR[i] # gC m-2 d-1 With coffee lue Metamodel

      ########## Potential use of reserves####
      # NPP_RE_Tree is filled in sequence by leaves and by roots at end of day
      # Thus NPP_RE_Tree must reset to zero for each time-step (day) begining and then
      S$Sim$NPP_RE[i]= 0
      S$Sim$Consumption_RE[i]=
        S$Parameters$kres*S$Sim$CM_RE[previous_i(i,1)]





      # Maintenance respiration -------------------------------------------------

      # Rm is computed at the beginning of the day on the drymass of the previous day.
      # This is considered as the highest priority for the plant (to maintain its dry mass)

      #Maintenance Respiration Rm_RsWood
      S$Sim$Rm_RsWood[i]=
        after(i,2)*
        (S$Parameters$PaliveRsWood*S$Sim$DM_RsWood[previous_i(i,1)]*
           S$Parameters$NContentRsWood*S$Parameters$MRN*
           S$Parameters$Q10RsWood^((S$Sim$Tcan_MAESPA_Coffee[i]-S$Parameters$TMR)/10))

      # Stump and Coarse roots (perennial wood)
      S$Sim$Rm_SCR[i]=
        after(i,2)*
        (S$Parameters$PaliveSCR*
           S$Sim$DM_SCR[previous_i(i,1)]*
           S$Parameters$NContentSCR*S$Parameters$MRN*
           S$Parameters$Q10SCR^(
             (S$Sim$Tcan_MAESPA_Coffee[i]-S$Parameters$TMR)/10))

      # Fruits
      S$Sim$Rm_Fruit[i]=
        after(i,2)*
        (S$Parameters$PaliveFruit*S$Sim$DM_Fruit[previous_i(i,1)]*
           S$Parameters$NContentFruit*S$Parameters$MRN*
           S$Parameters$Q10Fruit^((S$Sim$Tcan_MAESPA_Coffee[i]-S$Parameters$TMR)/10))
      # Leaves
      S$Sim$Rm_Leaf[i]=
        after(i,2)*
        (S$Parameters$PaliveLeaf*S$Sim$DM_Leaf[previous_i(i,1)]*
           S$Parameters$NContentLeaf*S$Parameters$MRN*
           S$Parameters$Q10Leaf^((S$Sim$Tcan_MAESPA_Coffee[i]-S$Parameters$TMR)/10))

      # Fine roots
      S$Sim$Rm_FRoot[i]=
        after(i,2)*
        (S$Parameters$PaliveFRoot*S$Sim$DM_FRoot[previous_i(i,1)]*
           S$Parameters$NContentFRoot*S$Parameters$MRN*
           S$Parameters$Q10FRoot^((S$Sim$Tcan_MAESPA_Coffee[i]-S$Parameters$TMR)/10))

      # Total plant maintenance respiration
      S$Sim$Rm[i]=
        S$Sim$Rm_Fruit[i]+S$Sim$Rm_Leaf[i]+
        S$Sim$Rm_RsWood[i]+S$Sim$Rm_SCR[i]+
        S$Sim$Rm_FRoot[i]


      ##############################----- Coffee Allocation ----##############################


      # Offer function ----------------------------------------------------------
      S$Sim$Offer[i]=
        max(S$Sim$GPP[i]-S$Sim$Rm[i]+S$Sim$Consumption_RE[i],0)

      # If the respiration is greater than the GPP + reserves use, then take this carbon
      # from mortality of each compartments' biomass equally (not for fruits or reserves):
      S$Sim$Carbon_Lack_Mortality[i]=
        -min(0,S$Sim$GPP[i]-S$Sim$Rm[i]+S$Sim$Consumption_RE[i])


      # 1-Resprout wood ---------------------------------------------------------
      # Allocation priority 1, see Charbonnier 2012.

      # Offer
      S$Sim$Alloc_RsWood[i]= S$Parameters$lambdaRsWood*S$Sim$Offer[i]
      # NPP (Offer-Rc)
      S$Sim$NPP_RsWood[i]= S$Parameters$epsilonRsWood*S$Sim$Alloc_RsWood[i]
      # Rc (growth respiration
      S$Sim$Rc_RsWood[i]= (1-S$Parameters$epsilonRsWood)*S$Sim$Alloc_RsWood[i]
      # Natural Mortality
      S$Sim$Mnat_RsWood[i]=
        S$Sim$CM_RsWood[previous_i(i,1)]/S$Parameters$lifespanRsWood
      # Pruning
      if(S$Sim$Plot_Age[i]>=S$Parameters$MeanAgePruning&
         S$Sim$Plot_Age[i]<=(S$Parameters$MeanAgePruning+2)&
         S$Met_c$DOY[i]==S$Parameters$date_pruning){
        S$Sim$Mprun_RsWood[i]=S$Sim$CM_RsWood[previous_i(i,1)]/3
      }else if(S$Sim$Plot_Age[i]>(S$Parameters$MeanAgePruning+2)&
               S$Met_c$DOY[i]==S$Parameters$date_pruning){
        S$Sim$Mprun_RsWood[i]=S$Sim$CM_RsWood[previous_i(i,1)]/2.5
      }
      S$Sim$Mortality_RsWood[i]=
        min((S$Sim$Mnat_RsWood[i]+S$Sim$Mprun_RsWood[i]),
            S$Sim$CM_RsWood[previous_i(i,1)])

      # 2-Stump and coarse roots (perennial wood) ------------------------------
      # coef d'alloc is more important for old ages, see Defrenet et al., 2016
      S$Sim$lambdaSCRage[i]=
        S$Parameters$lambdaSCR0-
        S$Sim$Plot_Age[i]/40*
        (S$Parameters$lambdaSCR0-S$Parameters$lambdaSCR40)
      #Offer
      S$Sim$Alloc_SCR[i]=
        S$Sim$lambdaSCRage[i]*S$Sim$Offer[i]
      #NPP
      S$Sim$NPP_SCR[i]=
        S$Parameters$epsilonSCR*S$Sim$Alloc_SCR[i]
      #Rc
      S$Sim$Rc_SCR[i]=
        (1-S$Parameters$epsilonSCR)*S$Sim$Alloc_SCR[i]
      #Mortality
      S$Sim$Mnat_SCR[i]=
        S$Sim$CM_SCR[previous_i(i,1)]/S$Parameters$lifespanSCR
      S$Sim$Mortality_SCR[i]= S$Sim$Mnat_SCR[i]

      # Ratio of number of new nodes per LAI unit as affected by canopy air temperature according to
      # Drinnan & Menzel, 1995
      #Source "0 Effect T on yield and vegetative growth.xlsx", sheet "Std20dComposWinterNodeperBr"
      # NB: computed at the end of the vegetatitve growth only to have Tcan of the
      # whole period already computed
      # NB2 : This is the total number of productive nodes on the coffee plant, i.e. the number of
      # green wood nodes that potentially carry flower buds. Green wood mass (and so number of nodes)
      # are related to leaf area (new leaves appear on nodes) : GUTIERREZ et al. (1998)
      if(S$Met_c$DOY[i]==S$Parameters$VGS_Stop){
        S$Sim$ratioNodestoLAI[S$Met_c$year>=S$Met_c$year[i]]=
          mean(S$Sim$Tcan_MAESPA_Coffee[S$Met_c$year==S$Met_c$year[i]&
                                                S$Met_c$DOY>=S$Parameters$VGS_Start&
                                                S$Met_c$DOY <= S$Parameters$VGS_Stop])%>%
            {S$Parameters$RNL_base*(0.0005455*.^3 - 0.0226364*.^2+0.2631364*. + 0.4194773)}
      }

      # Flower Buds + Flower + Fruits -------------------------------------------

      # (1) Buds induction
      # Buds start appearing for the very first time from 5500 dd. After that,
      # they appear every "S$Parameters$Tffb" degree days until flowering starts
      if(S$Sim$BudInitPeriod[i]){
        S$Sim$Budinit[i]=
          (S$Parameters$a_Budinit+S$Parameters$b_Budinit*2.017*PARcof)*
          S$Sim$LAI[i-1]*S$Sim$ratioNodestoLAI[i-1]*S$Sim$DegreeDays_Tcan[i]
        # Number of nodes: S$Sim$LAI[i-1]*S$Sim$ratioNodestoLAI[i-1]
        S$Sim$Bud_available[i]= S$Sim$Budinit[i]
      }
      # NB: 2.017 is conversion factor to estimate RAD above Coffea from PAR above Coffee
      # NB : number of fruits ~1200 / year / coffee tree, source : Castro-Tanzi et al. (2014)
      # S$Sim%>%group_by(Plot_Age)%>%summarise(N_Flowers= sum(BudBreak))

      # (2) Cumulative degree days experienced by each bud cohort :
      DegreeDay_i= round(cumsum(S$Sim$DegreeDays_Tcan[i:previous_i(i,1000)]),2)

      # (3) Find the window where buds are under dormancy (find the dormant cohorts)
      # Bud develops during Budstage1 (840) degree days after initiation, so they cannot be dormant
      # less than Budstage1 before i. But they can stay under dormancy until Budstage2 dd maximum,
      # so they cannot be older than Budstage2 dd before i.
      OldestDormancy= i - (max(which(DegreeDay_i<S$Parameters$Budstage2))-1)
      YoungestDormancy= i - (max(which(DegreeDay_i<S$Parameters$Budstage1))-1)
      # Idem above (reduce the days computed, Budstage2 is ~300 days and Budstage1 ~80-100 days)

      # (4) Test if the condition of minimum required rain for budbreak is met, and if not, which cohort
      # first met the condition (starting from younger to older cohorts):
      CumRain= cumsum(S$Met_c$Rain[YoungestDormancy:OldestDormancy])
      # (5) Compute the period were all cohorts have encountered all conditions to break dormancy :
      DormancyBreakPeriod= OldestDormancy:(YoungestDormancy-sum(CumRain<S$Parameters$RainForBudBreak))

      # (6) As temperature increases, the number of nodes on coffee increases due to increased vegetative
      # growth, but the number of buds per nodes decreases. This is computed by using a temperature correction
      # factor that decrease with increasing mean temperature during bud development (0-1, and =1 if mean T < 23).
      # This factor is then applied on the number of buds that break dormancy (less buds break dormancy with
      # increasing T).
      # Source: Drinnan, J. and C. Menzel, Temperature affects vegetative growth and flowering of coffee (Coffea arabica L.).
      # Journal of Horticultural Science, 1995. 70(1): p. 25-34. The correction is fitted like this :

      # (6.1) Using daytime temperature only (more variability):
      # Data_Buds= data.frame(Daily_Air_T=c(18,23,28,33), # diurnal data only
      #                       Buds_per_Node=c(2.6,3.2,1.5,0))
      # Data_Buds= Data_Buds[-1,]
      # Data_Buds$Buds_per_Node_cor= Data_Buds$Buds_per_Node/Data_Buds$Buds_per_Node[1]
      # lmbuds= lm(Buds_per_Node_cor~Daily_Air_T,data=Data_Buds)
      # if(mean(S$Sim$Tcan_Diurnal_Cof[DormancyBreakPeriod])>23){
      #     S$Sim$Temp_cor_Bud[DormancyBreakPeriod]=
      #         (3.29 - 0.1*mean(S$Sim$Tcan_Diurnal_Cof[DormancyBreakPeriod]))
      # }
      # (6.2) Using daily temperature (simpler):
      # Data_Buds_day= data.frame(Air_T=c(15.5,20.5,25.5,30.5),
      #                           Buds_per_Node=c(2.6,3.2,1.5,0))
      # Data_Buds_day= Data_Buds_day[-1,]
      # Data_Buds_day$Buds_per_Node_cor= Data_Buds_day$Buds_per_Node/Data_Buds_day$Buds_per_Node[1]
      # lmbuds_day= lm(Buds_per_Node_cor~Air_T,data=Data_Buds_day)
      if(mean(S$Sim$Tcan_MAESPA_Coffee[DormancyBreakPeriod])>23){
        S$Sim$Temp_cor_Bud[DormancyBreakPeriod]=
          (3.04 - 0.1*mean(S$Sim$Tcan_MAESPA_Coffee[DormancyBreakPeriod]))
      }

      # (7) Bud dormancy break, Source, Drinnan 1992 and Rodriguez et al., 2011 eq. 13
      S$Sim$p_budbreakperday[i]= 1/(1+exp(S$Parameters$a_p+S$Parameters$b_p*
                                               S$Sim$LeafWaterPotential[previous_i(i,1)]))
      # (8) Compute the number of buds that effectively break dormancy in each cohort:
      S$Sim$BudBreak_cohort[DormancyBreakPeriod]=
        pmin(S$Sim$Bud_available[DormancyBreakPeriod],
             S$Sim$Budinit[DormancyBreakPeriod]*S$Sim$p_budbreakperday[i]*
               S$Sim$Temp_cor_Bud[DormancyBreakPeriod])
      # NB 1: cannot exceed the number of buds of each cohort
      # NB 2: using Budinit and not Bud_available because p_budbreakperday is fitted on total bud cohort

      # (9) Remove buds that did break dormancy from the pool of dormant buds
      S$Sim$Bud_available[DormancyBreakPeriod]=
        S$Sim$Bud_available[DormancyBreakPeriod]-S$Sim$BudBreak_cohort[DormancyBreakPeriod]

      # (10) Sum the buds that break dormancy from each cohort to compute the total number of buds
      # that break dormancy on day i :
      S$Sim$BudBreak[i]= min(sum(S$Sim$BudBreak_cohort[DormancyBreakPeriod]),12)
      # Rodriguez et al. state that the maximum number of buds that may break dormancy
      # during each dormancy-terminating episode was set to 12 (see Table 1).

      # Fruits :
      FruitingPeriod= i-which(DegreeDay_i<(S$Parameters$FruitOverripe))+1
      # NB : Fruits that are older than the FruitingPeriod are overripped

      # Demand from each fruits cohort present on the coffee tree (not overriped), same as Demand_Fruit but keeping each value :
      Demand_Fruit_Cohort_Period=
        S$Sim$BudBreak[FruitingPeriod]*S$Parameters$Opti_C_DemandFruit*
        F_Integ_Dens(DegreeDay_i,FruitingPeriod,S$Parameters$u_log,S$Parameters$s_log)
      Demand_Fruit_Cohort_Period[is.na(Demand_Fruit_Cohort_Period)]= 0
      # Total C demand of the fruits :
      S$Sim$Demand_Fruit[i]= sum(Demand_Fruit_Cohort_Period)
      # C offer to Fruits (i.e. what is left from Offer after removing the consumption by previous compartments and Rm):
      S$Sim$Offer_Fruit[i]=
        S$Sim$Offer[i]-S$Sim$Alloc_RsWood[i]-
        S$Sim$Alloc_SCR[i]

      # Total C allocation to all fruits on day i :
      S$Sim$Alloc_Fruit[i]= min(S$Sim$Demand_Fruit[i],S$Sim$Offer_Fruit[i])
      # Allocation to each cohort, relative to each cohort demand :
      S$Sim$Alloc_Fruit_Cohort[FruitingPeriod]=
        S$Sim$Alloc_Fruit[i]*(Demand_Fruit_Cohort_Period/S$Sim$Demand_Fruit[i])
      S$Sim$Alloc_Fruit_Cohort[FruitingPeriod][is.nan(S$Sim$Alloc_Fruit_Cohort[FruitingPeriod])]= 0
      S$Sim$NPP_Fruit_Cohort[FruitingPeriod]=
        S$Parameters$epsilonFruit*S$Sim$Alloc_Fruit_Cohort[FruitingPeriod]
      S$Sim$CM_Fruit_Cohort[FruitingPeriod]= S$Sim$CM_Fruit_Cohort[FruitingPeriod]+
        S$Sim$NPP_Fruit_Cohort[FruitingPeriod]
      S$Sim$DM_Fruit_Cohort[FruitingPeriod]= S$Sim$CM_Fruit_Cohort[FruitingPeriod]/S$Parameters$CContent_Fruit
      # Overriped fruits that fall onto the ground (= to mass of the cohort that overripe) :
      S$Sim$Overriped_Fruit[i]= S$Sim$CM_Fruit_Cohort[max(min(FruitingPeriod)-1,1)]
      # S$Sim$Overriped_Fruit[i]= S$Sim$CM_Fruit_Cohort[min(FruitingPeriod)-1]*S$Parameters$epsilonFruit

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
                sum(S$Sim$DM_Fruit_Cohort[FruitingPeriod]*((S$Parameters$S_y0 + S$Parameters$S_a)/100)),3)
      # NB : here harvest maturity is computed as the average maturity of the cohorts. It could be computed
      # as the percentage of cohorts that are fully mature (Pezzopane et al. 2012 say at 221 days after flowering)
      # Optimal sucrose concentration around 8.8% of the dry mass

      # Harvest. Made one day only for now (TODO: make it a period of harvest)
      # Made as soon as the fruit dry mass is decreasing (overriping more important than fruit maturation):
      if(S$Sim$Plot_Age[i]>=S$Parameters$ageMaturity&
         S$Sim$CM_Fruit[previous_i(i,1)]<(S$Sim$CM_Fruit[previous_i(i,3)]+0.1)&
         S$Sim$CM_Fruit[previous_i(i,1)]>40){
        # Save the date of harvest:
        S$Sim$Date_harvest[i]= S$Met_c$DOY[i]
        S$Sim$Harvest_Fruit[i]= S$Sim$CM_Fruit[i-1]
        S$Sim$Harvest_Maturity[S$Met_c$year==S$Met_c$year[i]]= S$Sim$Harvest_Maturity_Pot[i]
        S$Sim$CM_Fruit[i-1]= S$Sim$Alloc_Fruit[i]=
          S$Sim$Overriped_Fruit[i]= 0
        S$Sim$CM_Fruit_Cohort= rep(0,length(S$Sim$CM_Fruit_Cohort))
        # RV: could harvest mature fruits only (To do).
      }else{
        S$Sim$Harvest_Fruit[i]= NA_real_
      }

      S$Sim$NPP_Fruit[i]= S$Parameters$epsilonFruit*S$Sim$Alloc_Fruit[i]
      S$Sim$Rc_Fruit[i]= (1-S$Parameters$epsilonFruit)*S$Sim$Alloc_Fruit[i]



      # Leaves ------------------------------------------------------------------

      S$Sim$Offer_Leaf[i]=
        S$Parameters$lambdaLeaf_remain*
        (S$Sim$Offer[i]-S$Sim$Alloc_Fruit[i]-
           S$Sim$Alloc_RsWood[i]-S$Sim$Alloc_SCR[i])

      S$Sim$Alloc_Leaf[i]=max(0,min(S$Parameters$Demand_Leaf*(S$Parameters$Stocking_Coffee/10000),
                                          S$Sim$Offer_Leaf[i]))

      S$Sim$NPP_Leaf[i]= S$Parameters$epsilonLeaf*S$Sim$Alloc_Leaf[i]

      S$Sim$Rc_Leaf[i]= (1-S$Parameters$epsilonLeaf)*S$Sim$Alloc_Leaf[i]

      S$Sim$NPP_RE[i]= S$Sim$NPP_RE[i]+(S$Sim$Offer_Leaf[i]-S$Sim$Alloc_Leaf[i])

      S$Sim$Mnat_Leaf[i]=S$Sim$CM_Leaf[previous_i(i,1)]/S$Parameters$lifespanLeaf

      S$Sim$M_ALS[i]=
        after(i,2)*max(0,S$Sim$CM_Leaf[previous_i(i,1)]*S$Sim$ALS[i])

      if(S$Sim$Plot_Age[i]>=S$Parameters$MeanAgePruning&S$Met_c$DOY[i]==S$Parameters$date_pruning){
        S$Sim$Mprun_Leaf[i]= S$Sim$CM_Leaf[previous_i(i,1)]*S$Parameters$LeafPruningRate
      }else{
        S$Sim$Mprun_Leaf[i]= 0
      }

      S$Sim$Mortality_Leaf[i]= S$Sim$Mnat_Leaf[i] + S$Sim$Mprun_Leaf[i]+S$Sim$M_ALS[i]


      # Fine Roots --------------------------------------------------------------

      S$Sim$Demand_FRoot[i]= S$Parameters$Demand_Leaf

      S$Sim$Offer_FRoot[i]=
        S$Parameters$lambdaFRoot_remain*
        (S$Sim$Offer[i]-S$Sim$Alloc_Fruit[i]-
           S$Sim$Alloc_RsWood[i]-S$Sim$Alloc_SCR[i])

      S$Sim$Alloc_FRoot[i]=max(0,min(S$Sim$Demand_FRoot[i],S$Sim$Offer_FRoot[i]))

      S$Sim$NPP_FRoot[i]=S$Parameters$epsilonFRoot*S$Sim$Alloc_FRoot[i]

      S$Sim$Rc_FRoot[i]=(1-S$Parameters$epsilonFRoot)*S$Sim$Alloc_FRoot[i]

      S$Sim$NPP_RE[i]= S$Sim$NPP_RE[i]+(S$Sim$Offer_FRoot[i]-S$Sim$Alloc_FRoot[i])

      S$Sim$Mnat_FRoot[i]=S$Sim$CM_FRoot[previous_i(i,1)]/S$Parameters$lifespanFRoot
      S$Sim$Mprun_FRoot[i]=S$Parameters$M_RateFRootprun*S$Sim$Mprun_Leaf[i]
      S$Sim$Mortality_FRoot[i]=S$Sim$Mnat_FRoot[i]+S$Sim$Mprun_FRoot[i]


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

      S$Sim$DM_Leaf[i]= S$Sim$CM_Leaf[i]/S$Parameters$CContent_Leaf
      S$Sim$DM_RsWood[i]= S$Sim$CM_RsWood[i]/S$Parameters$CContent_RsWood
      S$Sim$DM_Fruit[i]=S$Sim$CM_Fruit[i]/S$Parameters$CContent_Fruit
      S$Sim$DM_SCR[i]= S$Sim$CM_SCR[i]/
        S$Parameters$CContent_SCR
      S$Sim$DM_FRoot[i]= S$Sim$CM_FRoot[i]/S$Parameters$CContent_FRoots
      S$Sim$DM_RE[i]=S$Sim$CM_RE[i]/S$Parameters$CContent_SCR


      # Total Respiration and NPP -----------------------------------------------

      S$Sim$Rc[i]= S$Sim$Rc_Fruit[i]+S$Sim$Rc_Leaf[i]+
        S$Sim$Rc_RsWood[i]+S$Sim$Rc_SCR[i]+
        S$Sim$Rc_FRoot[i]
      S$Sim$Ra[i]=S$Sim$Rm[i]+S$Sim$Rc[i]
      S$Sim$NPP[i]=S$Sim$NPP_RsWood[i]+S$Sim$NPP_SCR[i]+
        S$Sim$NPP_Fruit[i]+S$Sim$NPP_Leaf[i]+S$Sim$NPP_FRoot[i]

      # LAI ---------------------------------------------------------------------

      # Caution: CM is in gC m-2soil, so use C content to transform in dry mass
      S$Sim$LAI[i]= S$Sim$CM_Leaf[i]*S$Parameters$SLA/1000/S$Parameters$CContent_Leaf
      S$Sim$LAIplot[i]= S$Sim$LAIplot[i]+S$Sim$LAI[i]

      # Metamodel Coffee leaf water potential
      S$Sim$LeafWaterPotential[i]=
        0.040730 - 0.005074*S$Met_c$VPD[i] - 0.037518*PARcof + 2.676284*S$Sim$SoilWaterPot[i]

      # S$Sim$LeafWaterPotential[i]=
      #     -0.096845 - 0.080517*S$Met_c$PARm2d1 +
      #     0.481117*(1-S$Met_c$FDiff) - 0.001692*S$Met_c$DaysWithoutRain


      # soil (+canopy evap) water balance ---------------------------------------

      Soilfun(S,i)

      # Metamodel Transpiration Coffee, and filter out for negative values
      S$Sim$T_Cof[i]=
        -0.42164 + 0.03467*S$Met_c$VPD[i] + 0.10559*S$Sim$LAI[i] +
        0.11510*PARcof
      S$Sim$T_Cof[i][S$Sim$T_Cof[i]<0]= 0
      #Plot transpiration
      S$Sim$T_tot[i]= S$Sim$T_Tree[i]+S$Sim$T_Cof[i]

      #7/ Evapo-Transpiration ETR
      S$Sim$ETR[i]=
        S$Sim$T_tot[i]+S$Sim$E_Soil[i]+S$Sim$IntercRevapor[i]

      #10/ Latent (LEmod) and Sensible (Hmod) heat fluxes, in kgH2O m-2 d-1 * MJ kgH2O-1 = MJ m-2 d-1
      S$Sim$LE_Plot[i]= S$Sim$ETR[i]*S$Parameters$lambda

      S$Sim$LE_Coffee[i]=
        (S$Sim$T_Cof[i]+S$Sim$IntercRevapor[i]*
           (S$Sim$LAI[i]/S$Sim$LAIplot[i]))*S$Parameters$lambda

      # Metamodel for H :
      S$Sim$H_Coffee[i]=
        -1.80160 + 0.03139*S$Met_c$Tair[i] - 0.06046*S$Met_c$VPD[i]+
        1.93064*(1-S$Met_c$FDiff[i]) + 0.58368*PARcof+0.25838*S$Sim$LAI[i]

      S$Sim$Rn_Coffee[i]=
        S$Sim$H_Coffee[i] + S$Sim$LE_Coffee[i]

      # Tree LE and Rn (can not compute them in the Tree function because we need IntercRevapor)
      S$Sim$LE_Tree[i]=
        (S$Sim$T_Tree[i]+S$Sim$IntercRevapor[i]*
           (S$Sim$LAI_Tree[i]/S$Sim$LAIplot[i]))*S$Parameters$lambda
      S$Sim$Rn_Tree[i]= S$Sim$H_Tree[i] + S$Sim$LE_Tree[i]

      # Total plot heat flux:
      S$Sim$H_tot[i]= S$Sim$H_Coffee[i]+S$Sim$H_Tree[i]+S$Sim$H_Soil[i]
      # Total plot latent flux:
      S$Sim$LE_tot[i]=
        S$Sim$LE_Coffee[i]+S$Sim$LE_Tree[i]+S$Sim$LE_Soil[i]

      # Total plot net radiation:
      S$Sim$Rn_tot[i]=
        S$Sim$Rn_Coffee[i]+S$Sim$Rn_Tree[i]+S$Sim$Rn_Soil[i]

      #11/ Tcanopy Coffee : using bulk conductance if no trees, interlayer conductance if trees
      # Source: Van de Griend and Van Boxel 1989.
      if(S$Sim$Height_Tree[previous_i(i,1)]>S$Parameters$Height_Coffee){

        S$Sim$TairCanopy[i]=
          S$Sim$TairCanopy_Tree[i]+((S$Sim$H_Coffee[i]+S$Sim$H_Soil[i])*Parameters$MJ_to_W)/
          (bigleaf::air.density(S$Sim$TairCanopy_Tree[i],S$Met_c$Pressure[i]/10)*
             S$Parameters$Cp*
             G_interlay(Wind= S$Met_c$WindSpeed[i], ZHT = S$Parameters$ZHT,
                        LAI_top= S$Sim$LAI_Tree[previous_i(i,1)],
                        LAI_bot= S$Sim$LAI[previous_i(i,1)],
                        Z_top= S$Sim$Height_Tree[previous_i(i,1)],
                        extwind = S$Parameters$extwind))

        S$Sim$Tleaf_Coffee[i]=
          S$Sim$TairCanopy[i]+(S$Sim$H_Coffee[i]*Parameters$MJ_to_W)/
          (bigleaf::air.density(S$Sim$TairCanopy[i],S$Met_c$Pressure[i]/10)*
             S$Parameters$Cp*
             1/(1/G_interlay(Wind= S$Met_c$WindSpeed[i], ZHT = S$Parameters$ZHT,
                             LAI_top= S$Sim$LAI_Tree[previous_i(i,1)],
                             LAI_bot= S$Sim$LAI[previous_i(i,1)],
                             Z_top= S$Sim$Height_Tree[previous_i(i,1)],
                             extwind = S$Parameters$extwind)+
                  1/Gb_h(Wind = S$Met_c$WindSpeed[i], wleaf= S$Parameters$wleaf,
                         LAI_lay=S$Sim$LAI[previous_i(i,1)],
                         LAI_abv=S$Sim$LAI_Tree[previous_i(i,1)],
                         ZHT = S$Parameters$ZHT,
                         Z_top = S$Sim$Height_Tree[previous_i(i,1)],
                         extwind= S$Parameters$extwind)))


      }else{
        S$Sim$TairCanopy[i]=
          S$Met_c$Tair[i]+((S$Sim$H_Coffee[i]+S$Sim$H_Soil[i])*Parameters$MJ_to_W)/
          (bigleaf::air.density(S$Sim$TairCanopy_Tree[i],S$Met_c$Pressure[i]/10)*
             S$Parameters$Cp*
             G_bulk(Wind = S$Met_c$WindSpeed[i], ZHT = S$Parameters$ZHT,
                    Z_top = S$Parameters$Height_Coffee,
                    LAI = S$Sim$LAI[previous_i(i,1)],
                    extwind = S$Parameters$extwind))

        S$Sim$Tleaf_Coffee[i]=
          S$Sim$TairCanopy[i]+(S$Sim$H_Coffee[i]*Parameters$MJ_to_W)/
          (bigleaf::air.density(S$Sim$TairCanopy[i],S$Met_c$Pressure[i]/10)*
             S$Parameters$Cp*
             1/(1/G_bulk(Wind = S$Met_c$WindSpeed[i], ZHT = S$Parameters$ZHT,
                         Z_top = S$Parameters$Height_Coffee,
                         LAI = S$Sim$LAI[previous_i(i,1)],
                         extwind = S$Parameters$extwind)+
                  1/Gb_h(Wind= S$Met_c$WindSpeed[i], wleaf= S$Parameters$wleaf,
                         LAI_lay= S$Sim$LAI[previous_i(i,1)],
                         LAI_abv= S$Sim$LAI_Tree[previous_i(i,1)],
                         ZHT= S$Parameters$ZHT,
                         Z_top= S$Parameters$Height_Coffee,
                         extwind= S$Parameters$extwind)))
      }
      # NB : if no trees, TairCanopy_Tree= Tair
    }
    CycleList=list(Sim= S$Sim%>%as.data.frame)
  }
  parallel::stopCluster(cl)
  # Reordering the lists to make the results as previous versions of the model:
  Table= do.call(rbind, CycleList[c(1:NCycles)])

  # Force to keep interesting output variables only:
  UnwantedVarnames= c('.Fruit_Cohort',"Bud_available","BudBreak_cohort")
  Table= Table[,!colnames(Table)%in%UnwantedVarnames]

  attr(Table,"unit")= data.frame(varnames)

  message(paste("\n", "Simulation completed successfully", "\n"))
  FinalList= list(Sim= Table,Meteo= Meteo, Parameters= Parameters)
  if(WriteIt){
    write.results(FinalList,output_f,Simulation_Name,Outpath,...)
  }
  invisible(FinalList)
}
