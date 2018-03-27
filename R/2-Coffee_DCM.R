#' @title Dynamic Agroforestry Coffee Crop Model
#' @description   The DynACof process-based model computes plot-scale Net Primary Productivity, carbon allocation, growth, yield,
#'                energy, and water balance of coffee plantations according to management, while accounting for spatial effects using
#'                metamodels from the 3D process-based MAESPA. The model also uses coffee bud and fruit cohorts for reproductive
#'                development to better represent fruit carbon demand distribution along the year.
#' @param Period   Period of time to be simulated, see details. Default: \code{NULL}
#' @param WriteIt  If \code{TRUE}, write the resulting list, see details. Default: \code{FALSE}
#' @param returnIt Are the results to be returned by the function ? Default: \code{FALSE}
#' @param ...      PARAM_DESCRIPTION
#' @param output_f Output format, if '.RData', the output list will be saved as a unique \code{.RData} file, if it is different from
#'                 \code{.RData}, the output list will be writen in several \code{.csv} and \code{.txt} format. Default: \code{.RData}
#' @param Inpath   Path to the input parameter list folder, Default: \code{"1-Input/Default"}
#' @param Outpath  Path pointing to the folder were the results will be writen, Default: \code{=Inpath}
#' @param Simulation_Name Character name of the simulation file name if \code{WriteIt=T}. Default: \code{DynACof}
#' @param Site     Site parameters file name, see details. Default: \code{'1-Site.R'}
#' @param Meteo    Meteo parameters file name, see details. Default: \code{'2-Meteorology.txt'}
#' @param Soil     Soil parameters file name, see details. Default: \code{'3-Soil.R'}
#' @param Coffee   Coffee parameters file name, see details. Default: \code{'4-Coffee.R'}
#' @param Tree     Shade tree parameters file name, see details. Default: \code{NULL}
#' @return A list containing three objects :
#' \itemize{
#'   \item A data.frame of the simulation outputs at daily time-step:  #' \tabular{llll}{
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
#'   \item A data.frame of the input meteorology, potentially coming from the output of \code{\link{Meteorology}}: \tabular{lll}{\strong{Var} \tab \strong{unit} \tab \strong{Definition}\cr
#'                     year            \tab year        \tab Year of the simulation                       \cr
#'                     DOY             \tab day         \tab day of the year                              \cr
#'                     Date            \tab POSIXct date\tab Date in POSICct format                       \cr
#'                     Rain            \tab mm          \tab Rainfall                                     \cr
#'                     Tair            \tab deg C       \tab Air temperature (above canopy)               \cr
#'                     RH              \tab \%          \tab Relative humidity                            \cr
#'                     RAD             \tab MJ m-2 d-1  \tab Incident shortwave radiation                 \cr
#'                     Pressure        \tab hPa         \tab Atmospheic pressure                          \cr
#'                     WindSpeed       \tab m s-1       \tab Wind speed                                   \cr
#'                     CO2             \tab ppm         \tab Atmospheric CO2 concentration                \cr
#'                     DegreeDays      \tab deg C       \tab Growing degrre days                          \cr
#'                     PAR             \tab MJ m-2 d-1  \tab Incident photosynthetically active radiation \cr
#'                     FDiff           \tab Fraction    \tab Diffuse light fraction                       \cr
#'                     VPD             \tab hPa         \tab Vapor pressure deficit                       \cr
#'                     Rn              \tab MJ m-2 d-1  \tab Net radiation (will be removed further)      \cr
#'                     Tmax            \tab deg C       \tab Maximum air temperature durnig the day       \cr
#'                     Tmin            \tab deg C       \tab Minimum air temperature durnig the day       \cr
#'                     DaysWithoutRain \tab day         \tab Number of consecutive days with no rainfall
#' }
#'   \item A list of the input parameters (see \code{\link{site}})
#' }
#'
#' @author R. Vezy; O. Roupsard
#' @details Almost all variables for coffee exist also for shade trees with a suffix
#'          \code{_Tree} after the name of the variable, e.g. : LAI = coffee LAI,
#'          LAI_Tree = shade tree LAI.
#'          Special shade tree variables (see return section) are only optional,
#'          and may be longer upon parameterization because variables can be added in
#'          the metamodels parameter file in \strong{\code{\link{Metamodels}}} or
#'          \strong{\code{\link{Allometries}}}.
#'          It is highly recommended to set the system environment timezone to the one from the meteorology file.
#'          For example the default meteorology file (\code{\link{Aquiares}}) has to be set to \code{Sys.setenv(TZ="UTC")}.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  Sys.setenv(TZ="UTC")
#'  DynACof(WriteIt = T, Period= as.POSIXct(c("1979-01-01", "1982-01-01")),Outpath = "Results")
#'  }
#' }
#' @export
#' @rdname DynACof
#' @seealso \code{\link[bigleaf]{aerodynamic.conductance}}
#' \code{\link{Meteorology}} \code{\link{site}}
#' @importFrom bigleaf aerodynamic.conductance
#' @importFrom foreach %dopar%
#'
DynACof= function(Period=NULL, WriteIt= F,returnIt=F,...,
                  output_f=".RData",Inpath=NULL,Outpath=Inpath,Simulation_Name="DynACof",
                  Site="1-Site.R",Meteo="2-Meteorology.txt",Soil="3-Soil.R",
                  Coffee="4-Coffee.R",Tree=NULL){


  # Importing the parameters ------------------------------------------------

  Parameters= Import_Parameters(path = Inpath, Names = list(Site,Soil,Coffee,Tree))

  # Importing the meteo -----------------------------------------------------

  Meteo= Meteorology(file=Inpath,Period= Period,Parameters= Parameters)

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
  Direction%<>%
    group_by(Cycle,Plot_Age)%>%
    mutate(Plot_Age_num= seq(min(Plot_Age),min(Plot_Age)+1, length.out= n()))%>%ungroup()
  # Variables are reinitialized so each cycle is independant from the others -> mandatory for
  # parallel processing

  # Parallel loop over cycles:
  NbCores= parallel::detectCores()-1 # Set the maximum number of cores working on the model computation
  cl= parallel::makeCluster(min(NbCores,NCycles))
  doSNOW::registerDoSNOW(cl)
  CycleList= foreach::foreach(cy= 1:NCycles,.combine=rbind,.packages = c("dplyr","zoo")) %dopar% {
  # for(cy in 1:NCycles){

    # Initializing the Simulation object:

    S= SimulationClass$new()
    S$Parameters= Parameters

    # Initializing the table:
    S$Table_Day= Direction[Direction$Cycle==cy,]
    S$Met_c= Meteo[Direction$Cycle==cy,]
    Init_Table_Day(S)
    # Compute cumulative degree-days based on previous daily DD from semi-hourly data:
    CumulDegreeDays= cumsum(S$Met_c$DegreeDays)
    # Trick to avoid all the ifelse conditions because of the first time-step (evaluated at each
    # time step so increasing computing time):
    S$Zero_then_One= c(0,rep(1, nrow(S$Table_Day)-1))
    # S$Zero_then_One takes the value 0 at the first time step and then 1. It is used for two
    # different techniques:
    # 1- for taking the first value (i) instead of i-1, so avoid computation errors.
    # 2- for seting some variable value to 0 the first step instead of doing the computation (as
    # it was before with ifelse conditions). It makes the code less readable but much more efficient.

    ########### Bud induction window computation ####
    # Bud induction can start only at S$Parameters$Tffb degree-days after vegetative growth stops.
    # Source: Rodriguez et al. 2011.
    # The following module finds the vegetative growth end day, and add the Tffb parameter (Time of
    # first floral buds, in dd), then find the very first flowering of the year and set the vector
    # BudInitPeriod to TRUE between the two dates. So buds will appear between plant Tffb parameter
    # and the first flowering day only.

    # Day of vegetative growth end:
    VegetGrowthEndDay= which(S$Met_c$DOY==S$Parameters$VGS_Stop)
    # Variable of interest declaration:
    S$Table_Day$BudInitPeriod= FALSE
    S$Table_Day$Cohort_Reset= FALSE
    # Temporary variables declaration:
    CumsumRelativeToVeget= CumsumRelativeToBudinit=
      matrix(data = NA, nrow = length(VegetGrowthEndDay), ncol = nrow(S$Met_c))
    DateBudinit= DateFFlowering= NULL
    Cohort= 1
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
      S$Table_Day$BudInitPeriod[DateBudinit[i]:DateFFlowering[i]]= TRUE
      S$Table_Day$Cohort_Reset[DateBudinit[i]]= TRUE
    }
    S$Table_Day$BudInitPeriod[CumulDegreeDays<S$Parameters$VF_Flowering]= FALSE

    # Search for the species specific tree function:
    if(S$Parameters$Tree_Species=="No_Shade"){
      Treefun= No_Shade
    }else{
      Treefun= Shade.Tree
    }

    # American Leaf Spot:
    S$Table_Day$ALS= ALS(Elevation= S$Parameters$Elevation, SlopeAzimut= S$Parameters$SlopeAzimut,
                         Slope= S$Parameters$Slope, RowDistance= S$Parameters$RowDistance,
                         Shade= S$Parameters$Shade, CanopyHeight.Coffee= S$Parameters$Height_Coffee,
                         Fertilization= S$Parameters$Fertilization, ShadeType= S$Parameters$ShadeType,
                         CoffeePruning= S$Parameters$CoffeePruning, df_rain= S$Met_c)

    # Main Loop -----------------------------------------------------------------------------------

    for (i in 1:nrow(S$Table_Day)){


      # Shade Tree computation if any

      Treefun(S,i)
      # Should output at least APAR_Tree, LAI_Tree, T_Tree, Rn_Tree, H_Tree,
      # LE_Tree (sum of transpiration + leaf evap)


      # Coffee computation:

      # Metamodels for K, Paper 2, trained on 2011 only :
      S$Table_Day$K_Dif[i]= 0.39
      S$Table_Day$K_Dir[i]= 0.34

      # # Metamodels for K, Paper 3, trained on all ages and structure :
      # S$Table_Day$K_Dif[i]= 0.40
      # S$Table_Day$K_Dir[i]= 0.35

      #APAR coffee
      PARcof= S$Met_c$PAR[i]-S$Table_Day$APAR_Tree[i] # PAR above coffee layer

      S$Table_Day$APAR_Dif[i]=
        max(0,((S$Met_c$PAR[i]-S$Table_Day$APAR_Tree[i])*S$Met_c$FDiff[i])*
              (1-exp(-S$Table_Day$K_Dif[i]*S$Table_Day$LAI[i-S$Zero_then_One[i]])))#MJ m-2 d-1
      APAR_Dir= max(0,((S$Met_c$PAR[i]-S$Table_Day$APAR_Tree[i])*(1-S$Met_c$FDiff[i]))*
                      (1-exp(-S$Table_Day$K_Dir[i]*S$Table_Day$LAI[i-S$Zero_then_One[i]])))#MJ m-2 d-1
      # APAR_Dir is not part of S$Table_Day because it can be easily computed by
      # S$Met_c$PARm2d1-S$Table_Day$APAR_Dif
      S$Table_Day$APAR[i]= APAR_Dir+S$Table_Day$APAR_Dif[i]
      # S$Table_Day$APAR[i]= max(0,(S$Met_c$PAR[i]-S$Table_Day$APAR_Tree[i])*(1-
      # exp(-S$Table_Day$k[i]*S$Table_Day$LAI[i-S$Zero_then_One[i]])))#MJ m-2 d-1

      # Metamodel Coffee Tcanopy, Paper 2
      S$Table_Day$Tcan_MAESPA_Coffee[i]=
        -0.07741 + 0.99456*S$Met_c$Tair[i] - 0.06948*S$Met_c$VPD[i] -
        1.87975*(1-S$Met_c$FDiff[i]) + 0.19615*PARcof
      S$Table_Day$DegreeDays_Tcan[i]=
        GDD(Tmean = S$Table_Day$Tcan_MAESPA_Coffee[i],MinTT = S$Parameters$MinTT,
            MaxTT = S$Parameters$MaxTT)

      # S$Table_Day$Tcan_Diurnal_Cof[i]=
      #     0.90479 + 0.97384*S$Met_c$Diurnal_TAIR[i] + 0.24677*PARcof + 0.01163*S$Met_c$VPD[i] -
      #     2.53554*(1-S$Met_c$FDiff[i]) - 0.04597*S$Table_Day$LAI_Tree[i]
      # NB 1: MAESPA simulates a Coffee canopy temperature (= average leaf temperature) very similar
      # to the air temperature within the canopy, so we can use it interchangeably.
      # NB 2: could use sqrt of FBEAM and PARCof for a better fit (little less rmse) but we keep the
      # metamodel without it to decrease the risk of any overfitting or issue on extrapolation.


      # Metamodel Coffee Tcanopy, Paper 3
      # S$Table_Day$Tcan_MAESPA_Coffee[i]=
      #     0.92921 + 0.95568*S$Met_c$Tair[i] + 0.01241*S$Met_c$VPD[i] -
      #     0.47802*(1-S$Met_c$FDiff[i]) + 0.10599*PARcof-
      #     0.04573*S$Table_Day$LAI_Tree[i]
      # S$Table_Day$Tcan_Diurnal_Cof[i]=
      #     0.90479 + 0.97384*S$Met_c$Diurnal_TAIR[i] + 0.24677*PARcof + 0.01163*S$Met_c$VPD[i] -
      #     2.53554*(1-S$Met_c$FDiff[i]) - 0.04597*S$Table_Day$LAI_Tree[i]
      # NB 1: MAESPA simulates a Coffee canopy temperature (= average leaf temperature) very similar
      # to the air temperature within the canopy, so we can use it interchangeably.
      # NB 2: could use sqrt of FBEAM and PARCof for a better fit (little less rmse) but we keep the
      # metamodel without it to decrease the risk of any overfitting or issue on extrapolation.

      # Metamodel LUE coffee, Paper 2:
      S$Table_Day$lue[i]=
        2.174236 + 0.012514*S$Met_c$Tair[i] + 0.007653*S$Met_c$VPD[i] -
        1.861276*sqrt(1-S$Met_c$FDiff[i]) - 0.254475*sqrt(PARcof)

      # Metamodel LUE coffee, Paper 3:
      # S$Table_Day$lue[i]=
      #     1.968619 - 0.128587*PARcof - 1.140032*(1-S$Met_c$FDiff[i]) +
      #     0.001167*S$Met_c$CO2_ppm[i] - 0.012697*S$Table_Day$Tcan_MAESPA_Coffee[i]
      # S$Table_Day$lue[i][S$Table_Day$lue[i]<0.578]= 0.578

      #GPP Coffee
      S$Table_Day$GPP[i]= S$Table_Day$lue[i]*S$Table_Day$APAR[i] # gC m-2 d-1 With coffee lue Metamodel

      ########## Potential use of reserves####
      # NPP_RE_Tree is filled in sequence by leaves and by roots at end of day
      # Thus NPP_RE_Tree must reset to zero for each time-step (day) begining and then
      S$Table_Day$NPP_RE[i]= 0
      S$Table_Day$Consumption_RE[i]=
        S$Parameters$kres*S$Table_Day$CM_RE[i-S$Zero_then_One[i]]





      # Maintenance respiration -------------------------------------------------

      # Rm is computed at the beginning of the day on the drymass of the previous day.
      # This is considered as the highest priority for the plant (to maintain its dry mass)

      #Maintenance Respiration Rm_RsWood
      S$Table_Day$Rm_RsWood[i]=
        S$Zero_then_One[i]*
        (S$Parameters$PaliveRsWood*S$Table_Day$DM_RsWood[i-S$Zero_then_One[i]]*
           S$Parameters$NContentRsWood*S$Parameters$MRN*
           S$Parameters$Q10RsWood^((S$Table_Day$Tcan_MAESPA_Coffee[i]-S$Parameters$TMR)/10))

      # Stump and Coarse roots (perennial wood)
      S$Table_Day$Rm_SCR[i]=
        S$Zero_then_One[i]*
        (S$Parameters$PaliveSCR*
           S$Table_Day$DM_SCR[i-S$Zero_then_One[i]]*
           S$Parameters$NContentSCR*S$Parameters$MRN*
           S$Parameters$Q10SCR^(
             (S$Table_Day$Tcan_MAESPA_Coffee[i]-S$Parameters$TMR)/10))

      # Fruits
      S$Table_Day$Rm_Fruit[i]=
        S$Zero_then_One[i]*
        (S$Parameters$PaliveFruit*S$Table_Day$DM_Fruit[i-S$Zero_then_One[i]]*
           S$Parameters$NContentFruit*S$Parameters$MRN*
           S$Parameters$Q10Fruit^((S$Table_Day$Tcan_MAESPA_Coffee[i]-S$Parameters$TMR)/10))
      # Leaves
      S$Table_Day$Rm_Leaf[i]=
        S$Zero_then_One[i]*
        (S$Parameters$PaliveLeaf*S$Table_Day$DM_Leaf[i-S$Zero_then_One[i]]*
           S$Parameters$NContentLeaf*S$Parameters$MRN*
           S$Parameters$Q10Leaf^((S$Table_Day$Tcan_MAESPA_Coffee[i]-S$Parameters$TMR)/10))

      # Fine roots
      S$Table_Day$Rm_FRoot[i]=
        S$Zero_then_One[i]*
        (S$Parameters$PaliveFRoot*S$Table_Day$DM_FRoot[i-S$Zero_then_One[i]]*
           S$Parameters$NContentFRoot*S$Parameters$MRN*
           S$Parameters$Q10FRoot^((S$Table_Day$Tcan_MAESPA_Coffee[i]-S$Parameters$TMR)/10))

      # Total plant maintenance respiration
      S$Table_Day$Rm[i]=
        S$Table_Day$Rm_Fruit[i]+S$Table_Day$Rm_Leaf[i]+
        S$Table_Day$Rm_RsWood[i]+S$Table_Day$Rm_SCR[i]+
        S$Table_Day$Rm_FRoot[i]


      ##############################----- Coffee Allocation ----##############################


      # Offer function ----------------------------------------------------------
      S$Table_Day$Offer[i]=
        max(S$Table_Day$GPP[i]-S$Table_Day$Rm[i]+S$Table_Day$Consumption_RE[i],0)

      # If the respiration is greater than the GPP + reserves use, then take this carbon
      # from mortality of each compartments' biomass equally (not for fruits or reserves):
      S$Table_Day$Carbon_Lack_Mortality[i]=
        -min(0,S$Table_Day$GPP[i]-S$Table_Day$Rm[i]+S$Table_Day$Consumption_RE[i])


      # 1-Resprout wood ---------------------------------------------------------
      # Allocation priority 1, see Charbonnier 2012.

      # Offer
      S$Table_Day$Alloc_RsWood[i]= S$Parameters$lambdaRsWood*S$Table_Day$Offer[i]
      # NPP (Offer-Rc)
      S$Table_Day$NPP_RsWood[i]= S$Parameters$epsilonRsWood*S$Table_Day$Alloc_RsWood[i]
      # Rc (growth respiration
      S$Table_Day$Rc_RsWood[i]= (1-S$Parameters$epsilonRsWood)*S$Table_Day$Alloc_RsWood[i]
      # Natural Mortality
      S$Table_Day$Mnat_RsWood[i]=
        S$Table_Day$CM_RsWood[i-S$Zero_then_One[i]]/S$Parameters$lifespanRsWood
      # Pruning
      if(S$Table_Day$Plot_Age[i]>=S$Parameters$MeanAgePruning&
         S$Table_Day$Plot_Age[i]<=(S$Parameters$MeanAgePruning+2)&
         S$Met_c$DOY[i]==S$Parameters$date_pruning){
        S$Table_Day$Mprun_RsWood[i]=S$Table_Day$CM_RsWood[i-S$Zero_then_One[i]]/3
      }else if(S$Table_Day$Plot_Age[i]>(S$Parameters$MeanAgePruning+2)&
               S$Met_c$DOY[i]==S$Parameters$date_pruning){
        S$Table_Day$Mprun_RsWood[i]=S$Table_Day$CM_RsWood[i-S$Zero_then_One[i]]/2.5
      }
      S$Table_Day$Mortality_RsWood[i]=
        min((S$Table_Day$Mnat_RsWood[i]+S$Table_Day$Mprun_RsWood[i]),
            S$Table_Day$CM_RsWood[i-S$Zero_then_One[i]])

      # 2-Stump and coarse roots (perennial wood) ------------------------------
      # coef d'alloc is more important for old ages, see Defrenet et al., 2016
      S$Table_Day$lambdaSCRage[i]=
        S$Parameters$lambdaSCR0-
        S$Table_Day$Plot_Age[i]/40*
        (S$Parameters$lambdaSCR0-S$Parameters$lambdaSCR40)
      #Offer
      S$Table_Day$Alloc_SCR[i]=
        S$Table_Day$lambdaSCRage[i]*S$Table_Day$Offer[i]
      #NPP
      S$Table_Day$NPP_SCR[i]=
        S$Parameters$epsilonSCR*S$Table_Day$Alloc_SCR[i]
      #Rc
      S$Table_Day$Rc_SCR[i]=
        (1-S$Parameters$epsilonSCR)*S$Table_Day$Alloc_SCR[i]
      #Mortality
      S$Table_Day$Mnat_SCR[i]=
        S$Table_Day$CM_SCR[i-S$Zero_then_One[i]]/S$Parameters$lifespanSCR
      S$Table_Day$Mortality_SCR[i]= S$Table_Day$Mnat_SCR[i]

      # Ratio of number of new nodes per LAI unit as affected by air temperature according to
      # Drinnan & Menzel, 1995
      #Source "0 Effect T on yield and vegetative growth.xlsx", sheet "Std20dComposWinterNodeperBr"
      # NB: computed at the end of the vegetatitve growth only to have Tcan of the
      # whole period already computed
      # NB2 : This is the total number of productive nodes on the coffee plant, i.e. the number of
      # green wood nodes that potentially carry flower buds. Green wood mass (and so number of nodes)
      # are related to leaf area (new leaves appear on nodes) : GUTIERREZ et al. (1998)
      if(S$Met_c$DOY[i]==S$Parameters$VGS_Stop){
        S$Table_Day$ratioNodestoLAI[S$Met_c$year>=S$Met_c$year[i]]=
          S$Table_Day[S$Met_c$year==S$Met_c$year[i]&
                        S$Met_c$DOY>=S$Parameters$VGS_Start&
                        S$Met_c$DOY <= S$Parameters$VGS_Stop,]%>%
          summarise(AverTaVegGrowSeason_Year= mean(Tcan_MAESPA_Coffee))%>%
          mutate(Perc_Nodes_compared_to_20deg_ref=
                   0.0005455*AverTaVegGrowSeason_Year^3 - 0.0226364*
                   AverTaVegGrowSeason_Year^2+0.2631364*
                   AverTaVegGrowSeason_Year + 0.4194773)%>%
          transmute(ratio_Nodes_to_LAI=
                      S$Parameters$RNL_base*Perc_Nodes_compared_to_20deg_ref)%>%
          as.matrix()%>%as.vector()
      }

      ########## Flower Buds + Flower + Fruits (See Rodriguez et al. 2011)####


      # BudInit in number of buds per day: depends on radiation and number
      # of nodes in the plant (more nodes with higher temperature).

      # (1) Buds induction
      # Buds start appearing for the very first time from 5500 dd. After that,
      # they appear every "S$Parameters$Tffb" degree days until flowering starts
      if(S$Table_Day$BudInitPeriod[i]){
        S$Table_Day$Budinit[i]=
          (S$Parameters$a_Budinit+S$Parameters$b_Budinit*2.017*PARcof)*
          S$Table_Day$LAI[i-1]*S$Table_Day$ratioNodestoLAI[i-1]*S$Table_Day$DegreeDays_Tcan[i]
        # Number of nodes: S$Table_Day$LAI[i-1]*S$Table_Day$ratioNodestoLAI[i-1]
        if(S$Table_Day$Cohort_Reset[i]){Cohort=1}
        S$Table_Day$Bud_available[i]= S$Table_Day$Budinit[i]
      }
      # NB: 2.017 is conversion factor to estimate RAD above Coffea from PAR above Coffee
      # NB : number of fruits ~1200 / year / coffee tree, source : Castro-Tanzi et al. (2014)
      # S$Table_Day%>%group_by(Plot_Age)%>%summarise(N_Flowers= sum(BudBreak))


      # (2) Cumulative degree days experienced by each bud cohort :
      DegreeDay_i= round(cumsum(S$Table_Day$DegreeDays_Tcan[i:previous_i(i,1000)]),2)

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

      # (7) As temperature increases, the number of nodes on coffee increases due to increased vegetative
      # growth, but the number of buds per nodes decreases. This is computed by using a temperature correction
      # factor that decrease with increasing mean temperature during bud development (0-1, and =1 if mean T < 23).
      # This factor is then applied on the number of buds that break dormancy (less buds break dormancy with
      # increasing T).
      # Source: Drinnan, J. and C. Menzel, Temperature affects vegetative growth and flowering of coffee (Coffea arabica L.).
      # Journal of Horticultural Science, 1995. 70(1): p. 25-34. The correction is fitted like this :

      # (7.1) Using daytime temperature only (more variability):
      # Data_Buds= data.frame(Daily_Air_T=c(18,23,28,33), # diurnal data only
      #                       Buds_per_Node=c(2.6,3.2,1.5,0))
      # Data_Buds= Data_Buds[-1,]
      # Data_Buds$Buds_per_Node_cor= Data_Buds$Buds_per_Node/Data_Buds$Buds_per_Node[1]
      # lmbuds= lm(Buds_per_Node_cor~Daily_Air_T,data=Data_Buds)
      # if(mean(S$Table_Day$Tcan_Diurnal_Cof[DormancyBreakPeriod])>23){
      #     S$Table_Day$Temp_cor_Bud[DormancyBreakPeriod]=
      #         (3.29 - 0.1*mean(S$Table_Day$Tcan_Diurnal_Cof[DormancyBreakPeriod]))
      # }
      # (7.2) Using daily temperature (simpler):
      # Data_Buds_day= data.frame(Air_T=c(15.5,20.5,25.5,30.5),
      #                           Buds_per_Node=c(2.6,3.2,1.5,0))
      # Data_Buds_day= Data_Buds_day[-1,]
      # Data_Buds_day$Buds_per_Node_cor= Data_Buds_day$Buds_per_Node/Data_Buds_day$Buds_per_Node[1]
      # lmbuds_day= lm(Buds_per_Node_cor~Air_T,data=Data_Buds_day)
      if(mean(S$Table_Day$Tcan_MAESPA_Coffee[DormancyBreakPeriod])>23){
        S$Table_Day$Temp_cor_Bud[DormancyBreakPeriod]=
          (3.04 - 0.1*mean(S$Table_Day$Tcan_MAESPA_Coffee[DormancyBreakPeriod]))
      }

      # (6) Bud dormancy break, Source, Drinnan 1992 and Rodriguez et al., 2011 eq. 13
      S$Table_Day$p_budbreakperday= 1/(1+exp(S$Parameters$a_p+S$Parameters$b_p*
                                               S$Table_Day$LeafWaterPotential[i-S$Zero_then_One[i]]))
      # (7) Compute the number of buds that effectively break dormancy in each cohort:
      S$Table_Day$BudBreak_cohort[DormancyBreakPeriod]=
        pmin(S$Table_Day$Bud_available[DormancyBreakPeriod],
             S$Table_Day$Budinit[DormancyBreakPeriod]*S$Table_Day$p_budbreakperday[i]*
               S$Table_Day$Temp_cor_Bud[DormancyBreakPeriod])
      # NB 1: cannot exceed the number of buds of each cohort
      # NB 2: using Budinit and not Bud_available because p_budbreakperday is fitted on total bud cohort

      # (8) Remove buds that did break dormancy from the pool of dormant buds
      S$Table_Day$Bud_available[DormancyBreakPeriod]=
        S$Table_Day$Bud_available[DormancyBreakPeriod]-S$Table_Day$BudBreak_cohort[DormancyBreakPeriod]

      # (9) Sum the buds that break dormancy from each cohort to compute the total number of buds
      # that break dormancy on day i :
      S$Table_Day$BudBreak[i]= min(sum(S$Table_Day$BudBreak_cohort[DormancyBreakPeriod]),12)
      # Rodriguez et al. state that the maximum number of buds that may break dormancy
      # during each dormancy-terminating episode was set to 12 (see Table 1).

      # Fruits :
      FruitingPeriod= i-which(DegreeDay_i<(S$Parameters$FruitOverripe))+1
      # NB : Fruits that are older than the FruitingPeriod are overripped

      # Demand from each fruits cohort present on the coffee tree (not overriped), same as Demand_Fruit but keeping each value :
      Demand_Fruit_Cohort_Period=
        S$Table_Day$BudBreak[FruitingPeriod]*S$Parameters$Opti_C_DemandFruit*
        F_Integ_Dens(DegreeDay_i,FruitingPeriod,S$Parameters$u_log,S$Parameters$s_log)
      Demand_Fruit_Cohort_Period[is.na(Demand_Fruit_Cohort_Period)]= 0
      # Total C demand of the fruits :
      S$Table_Day$Demand_Fruit[i]= sum(Demand_Fruit_Cohort_Period)
      # C offer to Fruits (i.e. what is left from Offer after removing the consumption by previous compartments and Rm):
      S$Table_Day$Offer_Fruit[i]=
        S$Table_Day$Offer[i]-S$Table_Day$Alloc_RsWood[i]-
        S$Table_Day$Alloc_SCR[i]

      # Total C allocation to all fruits on day i :
      S$Table_Day$Alloc_Fruit[i]= min(S$Table_Day$Demand_Fruit[i],S$Table_Day$Offer_Fruit[i])
      # Allocation to each cohort, relative to each cohort demand :
      S$Table_Day$Alloc_Fruit_Cohort[FruitingPeriod]=
        S$Table_Day$Alloc_Fruit[i]*(Demand_Fruit_Cohort_Period/S$Table_Day$Demand_Fruit[i])
      S$Table_Day$Alloc_Fruit_Cohort[FruitingPeriod][is.nan(S$Table_Day$Alloc_Fruit_Cohort[FruitingPeriod])]= 0
      S$Table_Day$NPP_Fruit_Cohort[FruitingPeriod]=
        S$Parameters$epsilonFruit*S$Table_Day$Alloc_Fruit_Cohort[FruitingPeriod]
      S$Table_Day$CM_Fruit_Cohort[FruitingPeriod]= S$Table_Day$CM_Fruit_Cohort[FruitingPeriod]+
        S$Table_Day$NPP_Fruit_Cohort[FruitingPeriod]
      S$Table_Day$DM_Fruit_Cohort[FruitingPeriod]= S$Table_Day$CM_Fruit_Cohort[FruitingPeriod]/S$Parameters$CContent_Fruit
      # Overriped fruits that fall onto the ground (= to mass of the cohort that overripe) :
      S$Table_Day$Overriped_Fruit[i]= S$Table_Day$CM_Fruit_Cohort[max(min(FruitingPeriod)-1,1)]
      # S$Table_Day$Overriped_Fruit[i]= S$Table_Day$CM_Fruit_Cohort[min(FruitingPeriod)-1]*S$Parameters$epsilonFruit

      # Duration of the maturation of each cohort born in the ith day (in days):
      S$Table_Day$Maturation_duration[FruitingPeriod]=
        seq_along(FruitingPeriod)
      # Sucrose content of each cohort:
      S$Table_Day$Sucrose_Content[FruitingPeriod]=
        Sucrose_cont_perc(S$Table_Day$Maturation_duration[FruitingPeriod],
                          a= S$Parameters$S_a, b= S$Parameters$S_b,
                          x0= S$Parameters$S_x0, y0=S$Parameters$S_y0)
      # Sucrose mass of each cohort
      S$Table_Day$Sucrose_Mass[FruitingPeriod]=
        S$Table_Day$DM_Fruit_Cohort[FruitingPeriod]*S$Table_Day$Sucrose_Content[FruitingPeriod]
      # Harvest maturity:
      S$Table_Day$Harvest_Maturity_Pot[i]=
        round(sum(S$Table_Day$Sucrose_Mass[FruitingPeriod])/
                sum(S$Table_Day$DM_Fruit_Cohort[FruitingPeriod]*((S$Parameters$S_y0 + S$Parameters$S_a)/100)),3)
      # NB : here harvest maturity is computed as the average maturity of the cohorts. It could be computed
      # as the percentage of cohorts that are fully mature (Pezzopane et al. 2012 say at 221 days after flowering)
      # Optimal sucrose concentration around 8.8% of the dry mass

      # Harvest. Made one day only for now (TODO: make it a period of harvest)
      # Made as soon as the fruit dry mass is decreasing (overriping more important than fruit maturation):
      if(S$Table_Day$Plot_Age[i]>=S$Parameters$ageMaturity&
         S$Table_Day$CM_Fruit[i-S$Zero_then_One[i]]<(S$Table_Day$CM_Fruit[previous_i(i,3)]+0.1)&
         S$Table_Day$CM_Fruit[i-S$Zero_then_One[i]]>40){
        # Save the date of harvest:
        S$Table_Day$Date_harvest[i]= S$Met_c$DOY[i]
        S$Table_Day$Harvest_Fruit[i]= S$Table_Day$CM_Fruit[i-1]
        S$Table_Day$Harvest_Maturity[S$Met_c$year==S$Met_c$year[i]]= S$Table_Day$Harvest_Maturity_Pot[i]
        S$Table_Day$CM_Fruit[i-1]= S$Table_Day$Alloc_Fruit[i]=
          S$Table_Day$CM_Fruit_Cohort= S$Table_Day$Overriped_Fruit[i]= 0
        # RV: could harvest mature fruits only (To do).
      }else{
        S$Table_Day$Harvest_Fruit[i]= NA_real_
      }

      S$Table_Day$NPP_Fruit[i]= S$Parameters$epsilonFruit*S$Table_Day$Alloc_Fruit[i]
      S$Table_Day$Rc_Fruit[i]= (1-S$Parameters$epsilonFruit)*S$Table_Day$Alloc_Fruit[i]



      ############# Leaves ####
      #offer : what is left after deduction of the previous compartments
      #GlM: ce qui reste apres production de bois, grains et CepsRacine(=
      #offre)est repartis entre racine et feuilles selon la meme proportion
      #que la repartition de la NPP entre Racines et feuilles. Les lambdas ne
      #peuvent etre utilises que sur NPP totale
      S$Table_Day$Offer_Leaf[i]=
        S$Parameters$lambdaLeaf_remain*
        (S$Table_Day$Offer[i]-S$Table_Day$Alloc_Fruit[i]-
           S$Table_Day$Alloc_RsWood[i]-S$Table_Day$Alloc_SCR[i])
      #Demand : the demand is actually S$Parameters$Demand_Leaf
      #Min(Demand, Offer)
      S$Table_Day$Alloc_Leaf[i]=max(0,min(S$Parameters$Demand_Leaf, S$Table_Day$Offer_Leaf[i]))
      #NPP
      S$Table_Day$NPP_Leaf[i]= S$Parameters$epsilonLeaf*S$Table_Day$Alloc_Leaf[i]
      #Rc
      S$Table_Day$Rc_Leaf[i]= (1-S$Parameters$epsilonLeaf)*S$Table_Day$Alloc_Leaf[i]
      #Excess into Reserves
      S$Table_Day$NPP_RE[i]= S$Table_Day$NPP_RE[i]+(S$Table_Day$Offer_Leaf[i]-S$Table_Day$Alloc_Leaf[i])

      #Mortality
      # By natural litterfall assuming no diseases
      S$Table_Day$Mnat_Leaf[i]=S$Table_Day$CM_Leaf[i-S$Zero_then_One[i]]/S$Parameters$lifespanLeaf
      # By American Leaf Spot # Litterfall by ALS is difference between 2 dates
      S$Table_Day$M_ALS[i]=
        S$Zero_then_One[i]*max(0,S$Table_Day$CM_Leaf[i-S$Zero_then_One[i]]*S$Table_Day$ALS[i])

      #By pruning
      if(S$Table_Day$Plot_Age[i]>=S$Parameters$MeanAgePruning&S$Met_c$DOY[i]==S$Parameters$date_pruning){
        S$Table_Day$Mprun_Leaf[i]=S$Table_Day$CM_Leaf[i-S$Zero_then_One[i]]*
          S$Parameters$LeafPruningRate}else{S$Table_Day$Mprun_Leaf[i]=0}

      S$Table_Day$Mortality_Leaf[i]= S$Table_Day$Mnat_Leaf[i] + S$Table_Day$Mprun_Leaf[i]+S$Table_Day$M_ALS[i]


      ############# Fine Roots #####
      #Demand : the demand is actually S$Parameters$Demand_Leaf
      #S$Table_Day$Demand_FRoot[i]= S$Table_Day$Alloc_Leaf[i]
      S$Table_Day$Demand_FRoot[i]= S$Parameters$Demand_Leaf
      # f(T,SW     D,...)#suppose egalite entre feuilles et racines fines pour demande
      #Offer
      S$Table_Day$Offer_FRoot[i]=
        S$Parameters$lambdaFRoot_remain*
        (S$Table_Day$Offer[i]-S$Table_Day$Alloc_Fruit[i]-
           S$Table_Day$Alloc_RsWood[i]-S$Table_Day$Alloc_SCR[i])

      S$Table_Day$Alloc_FRoot[i]=max(0,min(S$Table_Day$Demand_FRoot[i],S$Table_Day$Offer_FRoot[i]))

      S$Table_Day$NPP_FRoot[i]=S$Parameters$epsilonFRoot*S$Table_Day$Alloc_FRoot[i]
      #Rc
      S$Table_Day$Rc_FRoot[i]=(1-S$Parameters$epsilonFRoot)*S$Table_Day$Alloc_FRoot[i]
      #Excess into reserves
      S$Table_Day$NPP_RE[i]= S$Table_Day$NPP_RE[i]+(S$Table_Day$Offer_FRoot[i]-S$Table_Day$Alloc_FRoot[i])

      #Mortality
      S$Table_Day$Mnat_FRoot[i]=S$Table_Day$CM_FRoot[i-S$Zero_then_One[i]]/S$Parameters$lifespanFRoot
      S$Table_Day$Mprun_FRoot[i]=S$Parameters$M_RateFRootprun*S$Table_Day$Mprun_Leaf[i]
      S$Table_Day$Mortality_FRoot[i]=S$Table_Day$Mnat_FRoot[i]+S$Table_Day$Mprun_FRoot[i]


      ############# Update of Biomass & Rm, Rc, Ra & LAI & & Buds ####



      S$Table_Day$CM_Leaf[i]=S$Table_Day$CM_Leaf[i-S$Zero_then_One[i]]+
        S$Table_Day$NPP_Leaf[i]-S$Table_Day$Mortality_Leaf[i]-
        S$Table_Day$Carbon_Lack_Mortality[i]*0.25
      S$Table_Day$CM_RsWood[i]= S$Table_Day$CM_RsWood[i-S$Zero_then_One[i]]+
        S$Table_Day$NPP_RsWood[i]-S$Table_Day$Mortality_RsWood[i]-
        S$Table_Day$Carbon_Lack_Mortality[i]*0.25
      S$Table_Day$CM_Fruit[i]=S$Table_Day$CM_Fruit[i-S$Zero_then_One[i]]+
        S$Table_Day$NPP_Fruit[i]-S$Table_Day$Overriped_Fruit[i]
      # NB: S$Table_Day$Overriped_Fruit is negative (loss by falling if overriped)
      S$Table_Day$CM_SCR[i]= S$Table_Day$CM_SCR[i-S$Zero_then_One[i]]+
        S$Table_Day$NPP_SCR[i]-S$Table_Day$Mortality_SCR[i]-
        S$Table_Day$Carbon_Lack_Mortality[i]*0.25
      S$Table_Day$CM_FRoot[i]= S$Table_Day$CM_FRoot[i-S$Zero_then_One[i]]+
        S$Table_Day$NPP_FRoot[i]-S$Table_Day$Mortality_FRoot[i]-
        S$Table_Day$Carbon_Lack_Mortality[i]*0.25
      S$Table_Day$CM_RE[i]=S$Table_Day$CM_RE[i-S$Zero_then_One[i]]+S$Table_Day$NPP_RE[i]-
        S$Table_Day$Consumption_RE[i]
      S$Table_Day$Rc[i]= S$Table_Day$Rc_Fruit[i]+S$Table_Day$Rc_Leaf[i]+
        S$Table_Day$Rc_RsWood[i]+S$Table_Day$Rc_SCR[i]+
        S$Table_Day$Rc_FRoot[i]
      S$Table_Day$Ra[i]=S$Table_Day$Rm[i]+S$Table_Day$Rc[i]

      S$Table_Day$NPP[i]=S$Table_Day$NPP_RsWood[i]+S$Table_Day$NPP_SCR[i]+
        S$Table_Day$NPP_Fruit[i]+S$Table_Day$NPP_Leaf[i]+S$Table_Day$NPP_FRoot[i]

      #Update LAI m2leaf m-2soil, caution, CM is in gC m-2soil, so use C content to transform in dry mass
      S$Table_Day$LAI[i]= S$Table_Day$CM_Leaf[i]*S$Parameters$SLA/1000/S$Parameters$CContent_Leaf

      # Dry mass computation:
      S$Table_Day$DM_Leaf[i]= S$Table_Day$CM_Leaf[i]/S$Parameters$CContent_Leaf
      S$Table_Day$DM_RsWood[i]= S$Table_Day$CM_RsWood[i]/S$Parameters$CContent_RsWood
      S$Table_Day$DM_Fruit[i]=S$Table_Day$CM_Fruit[i]/S$Parameters$CContent_Fruit
      S$Table_Day$DM_SCR[i]= S$Table_Day$CM_SCR[i]/
        S$Parameters$CContent_SCR
      S$Table_Day$DM_FRoot[i]= S$Table_Day$CM_FRoot[i]/S$Parameters$CContent_FRoots
      S$Table_Day$DM_RE[i]=S$Table_Day$CM_RE[i]/S$Parameters$CContent_SCR

      ######################## Water balance (from BILJOU model) #############################

      # Soil Water potential, Campbell (1974) equation
      S$Table_Day$SoilWaterPot[i]=
        S$Parameters$PSIE*(((S$Table_Day$W_1[i-S$Zero_then_One[i]]+S$Table_Day$W_2[i-S$Zero_then_One[i]]+
                               S$Table_Day$W_3[i-S$Zero_then_One[i]])/3750)/S$Parameters$PoreFrac)^(-S$Parameters$B)

      # Metamodel Coffee leaf water potential
      S$Table_Day$LeafWaterPotential[i]=
        0.040730 - 0.005074*S$Met_c$VPD[i] - 0.037518*PARcof + 2.676284*S$Table_Day$SoilWaterPot[i]

      # S$Table_Day$LeafWaterPotential[i]=
      #     -0.096845 - 0.080517*S$Met_c$PARm2d1 +
      #     0.481117*(1-S$Met_c$FDiff) - 0.001692*S$Met_c$DaysWithoutRain

      # LAI plot is the sum of the LAI of the Tree + coffee LAI
      # (LAIplot[i] is first equal to 0, then added LAI of tree, and here adding LAI of coffee)
      S$Table_Day$LAIplot[i]= S$Table_Day$LAIplot[i]+S$Table_Day$LAI[i]

      #Rn or AE per layer (radiation reaching every layer, valid only during dailight hours,
      # not during night hours)
      # Rn understorey, source Shuttleworth & Wallace, 1985, eq. 21
      S$Table_Day$Rn_Soil[i]=
        S$Met_c$Rn[i]*exp(-S$Parameters$k_Rn*S$Table_Day$LAIplot[i])
      # source: Shuttleworth & Wallace, 1985, eq. 2.
      # NB: soil heat storage is negligible at daily time-step (or will equilibrate soon),
      # removing it

      #1/ Rainfall interception, source Gomez-Delgado et al.2011, Box A: IntercMax=AX;
      S$Table_Day$IntercMax[i]= S$Parameters$IntercSlope*S$Table_Day$LAIplot[i]

      S$Table_Day$CanopyHumect[i]=
        max(0,S$Table_Day$CanopyHumect[i-S$Zero_then_One[i]]+S$Met_c$Rain[i])

      Potential_LeafEvap=
        PENMON(Rn= S$Met_c$Rn[i], Wind= S$Met_c$WindSpeed[i], Tair = S$Met_c$Tair[i],
               ZHT = S$Parameters$ZHT,TREEH = max(S$Table_Day$Height_Tree[i],S$Parameters$Height_Coffee),
               Pressure = S$Met_c$Pressure[i],
               Gs = 1E09, VPD = S$Met_c$VPD[i])

      if(S$Table_Day$CanopyHumect[i]<=S$Table_Day$IntercMax[i]){
        S$Table_Day$Throughfall[i]= 0
        S$Table_Day$IntercRevapor[i]= min(S$Table_Day$CanopyHumect[i], Potential_LeafEvap)
        S$Table_Day$CanopyHumect[i]= max(0,S$Table_Day$CanopyHumect[i]-S$Table_Day$IntercRevapor[i])
      }else{
        S$Table_Day$Throughfall[i]=S$Table_Day$CanopyHumect[i]-S$Table_Day$IntercMax[i]
        S$Table_Day$IntercRevapor[i]=min(S$Table_Day$IntercMax[i],Potential_LeafEvap)
        S$Table_Day$CanopyHumect[i]=max(0,S$Table_Day$IntercMax[i]-S$Table_Day$IntercRevapor[i])
      }

      # 2/ SURFACE RUNOFF / INFILTRATION source Gomez-Delgado et al. 2011,
      # Box B:WSurfResMax = BX; WSurfaceRes=Bt;
      #ExcessRunoff=QB2; SuperficialRunoff=QB1;  TotSuperficialRunoff=QB; Infiltration=i
      # 2.a Adding throughfall to superficial-box, calculation of surface runoff, updating of
      # stock in superficial-box
      S$Table_Day$WSurfaceRes[i]=
        S$Table_Day$WSurfaceRes[i-S$Zero_then_One[i]] + S$Table_Day$Throughfall[i]

      if(S$Table_Day$WSurfaceRes[i] > S$Parameters$WSurfResMax){
        S$Table_Day$ExcessRunoff[i] = S$Table_Day$WSurfaceRes[i]-S$Parameters$WSurfResMax
        S$Table_Day$WSurfaceRes[i]= S$Parameters$WSurfResMax # removing ExcessRunoff
        S$Table_Day$SuperficialRunoff[i] = S$Parameters$kB * S$Table_Day$WSurfaceRes[i]
        #Subsuperficial runoff from runoffbox
        S$Table_Day$TotSuperficialRunoff[i] =
          S$Table_Day$ExcessRunoff[i] + S$Table_Day$SuperficialRunoff[i]
        S$Table_Day$WSurfaceRes[i] =
          S$Table_Day$WSurfaceRes[i] - S$Table_Day$SuperficialRunoff[i]
      }else{
        #updating WSurfaceRes, the ExcessRunoff has already been retrieved
        S$Table_Day$ExcessRunoff[i]=0
        S$Table_Day$SuperficialRunoff[i] = S$Parameters$kB * S$Table_Day$WSurfaceRes[i]
        S$Table_Day$TotSuperficialRunoff[i] = S$Table_Day$SuperficialRunoff[i]
        S$Table_Day$WSurfaceRes[i] = S$Table_Day$WSurfaceRes[i] -
          S$Table_Day$SuperficialRunoff[i]}

      # 2.b Computing the infiltration capacity as a function of soil water content in W_1
      S$Table_Day$W_1[i]= S$Table_Day$W_1[i-S$Zero_then_One[i]]

      if(S$Table_Day$W_1[i] <= S$Parameters$Wm1){
        S$Table_Day$InfilCapa[i]= S$Parameters$fo # InfilCapa: infiltration capacity
      }else{
        if(S$Table_Day$W_1[i]<= S$Parameters$Wf1){
          S$Table_Day$InfilCapa[i]= S$Parameters$fo-(S$Table_Day$W_1[i]-S$Parameters$Wm1)*
            (S$Parameters$fo - S$Parameters$fc) / (S$Parameters$Wf1 - S$Parameters$Wm1)
        }else{
          S$Table_Day$InfilCapa[i]=S$Parameters$fc
        }
      }

      # 2.c Calculating infiltration from superficial-box to soil-boxes and updating stock in superficial-box
      if(S$Table_Day$InfilCapa[i]<= S$Table_Day$WSurfaceRes[i]){
        S$Table_Day$Infiltration[i]= S$Table_Day$InfilCapa[i]   # infiltration (m?dt-1)
        S$Table_Day$WSurfaceRes[i]=
          S$Table_Day$WSurfaceRes[i] - S$Table_Day$Infiltration[i]
      }else{
        S$Table_Day$Infiltration[i]= S$Table_Day$WSurfaceRes[i]
        S$Table_Day$WSurfaceRes[i]= 0
      }

      #3/ Adding Infiltration to soil water content of the previous day, computing drainage,
      # source Gomez-Delgado et al. 2010
      # RV: same as CanopyHumect
      # S$Table_Day$W_1[i]= S$Table_Day$W_1[i-S$Zero_then_One[i]]+S$Zero_then_One[i]*S$Table_Day$Infiltration[i]
      S$Table_Day$W_1[i]= S$Table_Day$W_1[i-S$Zero_then_One[i]]+S$Table_Day$Infiltration[i]

      #Preventing W_1 to be larger than the soil storage at field capacity:
      if(S$Table_Day$W_1[i] > S$Parameters$Wf1){
        S$Table_Day$Drain_1[i]= S$Table_Day$W_1[i] - S$Parameters$Wf1
        S$Table_Day$W_1[i] = S$Parameters$Wf1
      }else{S$Table_Day$Drain_1[i]= 0}     # Water excess in the root-box that drains (m)

      # RV: same as CanopyHumect
      # S$Table_Day$W_2[i]= S$Table_Day$W_2[i-S$Zero_then_One[i]]+S$Zero_then_One[i]*S$Table_Day$Drain_1[i]
      S$Table_Day$W_2[i]= S$Table_Day$W_2[i-S$Zero_then_One[i]]+S$Table_Day$Drain_1[i]

      #Preventing W_2 to be larger than the soil storage at field capacity:
      if(S$Table_Day$W_2[i] > S$Parameters$Wf2){
        S$Table_Day$Drain_2[i]= S$Table_Day$W_2[i] - S$Parameters$Wf2
        S$Table_Day$W_2[i] = S$Parameters$Wf2
      }else{S$Table_Day$Drain_2[i]= 0}     # Water excess in the root-box that drains (m)

      # RV: same as CanopyHumect
      # S$Table_Day$W_3[i]= S$Table_Day$W_3[i-S$Zero_then_One[i]]+S$Zero_then_One[i]*S$Table_Day$Drain_2[i]
      S$Table_Day$W_3[i]= S$Table_Day$W_3[i-S$Zero_then_One[i]]+S$Table_Day$Drain_2[i]

      #Preventing W_3 to be larger than the soil storage at field capacity:
      if(S$Table_Day$W_3[i] > S$Parameters$Wf3){
        S$Table_Day$Drain_3[i]= S$Table_Day$W_3[i] - S$Parameters$Wf3
        S$Table_Day$W_3[i] = S$Parameters$Wf3
      }else{S$Table_Day$Drain_3[i]= 0}     # Water excess in the root-box that drains (m)

      #3/First computing water per soil layer
      S$Table_Day$EW_1[i]= S$Table_Day$W_1[i]-S$Parameters$Wm1 # Extractable water (m)
      # Relative extractable water (dimensionless):
      S$Table_Day$REW_1[i]= S$Table_Day$EW_1[i]/(S$Parameters$Wf1-S$Parameters$Wm1)
      S$Table_Day$EW_2[i]= S$Table_Day$W_2[i]-S$Parameters$Wm2
      S$Table_Day$REW_2[i]= S$Table_Day$EW_2[i]/(S$Parameters$Wf2-S$Parameters$Wm2)
      S$Table_Day$EW_3[i]= S$Table_Day$W_3[i]-S$Parameters$Wm3
      S$Table_Day$REW_3[i]= S$Table_Day$EW_3[i]/(S$Parameters$Wf3-S$Parameters$Wm3)
      S$Table_Day$EW_tot[i]= S$Table_Day$EW_1[i]+S$Table_Day$EW_2[i]+S$Table_Day$EW_3[i]
      S$Table_Day$REW_tot[i]= S$Table_Day$EW_tot[i]/
        ((S$Parameters$Wf1-S$Parameters$Wm1)+(S$Parameters$Wf2-S$Parameters$Wm2)+
           (S$Parameters$Wf3-S$Parameters$Wm3))

      #4/Evaporation of the Understorey, E_Soil (from W_1 only)
      S$Table_Day$E_Soil[i]= S$Table_Day$Rn_Soil[i]*S$Parameters$Soil_LE_p/S$Parameters$lambda

      #Avoiding depleting W_1 below Wm1 and udating Wx after retrieving actual E_Soil
      if((S$Table_Day$W_1[i]-S$Table_Day$E_Soil[i])>=S$Parameters$Wm1){
        S$Table_Day$W_1[i]= S$Table_Day$W_1[i]-S$Table_Day$E_Soil[i]
      }else{S$Table_Day$E_Soil[i]= S$Table_Day$W_1[i]-S$Parameters$Wm1
      S$Table_Day$W_1[i]= S$Parameters$Wm1}

      #5/Transpiration T, source Granier et al., 1999
      #Transpiration T_tot_ per layer, source Metamodels from MAESPA:
      # NB OR, I divided the values from metamodel

      # Metamodel Transpiration Coffee, and filter out for negative values
      S$Table_Day$T_Cof[i]=
        -0.42164 + 0.03467*S$Met_c$VPD[i] + 0.10559*S$Table_Day$LAI[i] +
        0.11510*PARcof
      S$Table_Day$T_Cof[i][S$Table_Day$T_Cof[i]<0]= 0
      #Plot transpiration
      S$Table_Day$T_tot[i]= S$Table_Day$T_Tree[i]+S$Table_Day$T_Cof[i]

      #6/ Root Water Extraction by soil layer, source Granier et al., 1999
      S$Table_Day$RootWaterExtract_1[i]= S$Table_Day$T_tot[i]*S$Parameters$RootFraction1
      S$Table_Day$RootWaterExtract_2[i]= S$Table_Day$T_tot[i]*S$Parameters$RootFraction2
      S$Table_Day$RootWaterExtract_3[i]= S$Table_Day$T_tot[i]*S$Parameters$RootFraction3
      #Avoiding depleting Wx below Wmx, and udating Wx after retrieving actual RootWaterExtract
      if((S$Table_Day$W_1[i]-S$Table_Day$RootWaterExtract_1[i])>=S$Parameters$Wm1){
        S$Table_Day$W_1[i]= S$Table_Day$W_1[i]-S$Table_Day$RootWaterExtract_1[i]
      }else{S$Table_Day$RootWaterExtract_1[i]=S$Table_Day$W_1[i]-S$Parameters$Wm1
      S$Table_Day$W_1[i]=S$Parameters$Wm1}

      if((S$Table_Day$W_2[i]-S$Table_Day$RootWaterExtract_2[i])>=S$Parameters$Wm2){
        S$Table_Day$W_2[i]=S$Table_Day$W_2[i]-S$Table_Day$RootWaterExtract_2[i]
      }else{S$Table_Day$RootWaterExtract_2[i]=S$Table_Day$W_2[i]-S$Parameters$Wm2
      S$Table_Day$W_2[i]=S$Parameters$Wm2}

      if((S$Table_Day$W_3[i]-S$Table_Day$RootWaterExtract_3[i])>=S$Parameters$Wm3){
        S$Table_Day$W_3[i]=S$Table_Day$W_3[i]-S$Table_Day$RootWaterExtract_3[i]
      }else{S$Table_Day$RootWaterExtract_3[i]=S$Table_Day$W_3[i]-S$Parameters$Wm3
      S$Table_Day$W_3[i]=S$Parameters$Wm3}


      #7/ Evapo-Transpiration ETR
      S$Table_Day$ETR[i]=
        S$Table_Day$T_tot[i]+S$Table_Day$E_Soil[i]+S$Table_Day$IntercRevapor[i]

      #8/Second Updating water per soil layer
      S$Table_Day$W_tot[i]= S$Table_Day$W_1[i]+S$Table_Day$W_2[i]+S$Table_Day$W_3[i]
      S$Table_Day$EW_1[i]= S$Table_Day$W_1[i]-S$Parameters$Wm1 # Extractable water (m)
      S$Table_Day$REW_1[i]= S$Table_Day$EW_1[i]/(S$Parameters$Wf1-S$Parameters$Wm1)
      # Relative extractable water (dimensionless)
      S$Table_Day$EW_2[i]= S$Table_Day$W_2[i]-S$Parameters$Wm2
      S$Table_Day$REW_2[i]= S$Table_Day$EW_2[i]/(S$Parameters$Wf2-S$Parameters$Wm2)
      S$Table_Day$EW_3[i]= S$Table_Day$W_3[i]-S$Parameters$Wm3
      S$Table_Day$REW_3[i]= S$Table_Day$EW_3[i]/(S$Parameters$Wf3-S$Parameters$Wm3)
      S$Table_Day$EW_tot[i]= S$Table_Day$EW_1[i]+S$Table_Day$EW_2[i]+S$Table_Day$EW_3[i]
      S$Table_Day$REW_tot[i]= S$Table_Day$EW_tot[i]/
        ((S$Parameters$Wf1-S$Parameters$Wm1)+(S$Parameters$Wf2-S$Parameters$Wm2)+
           (S$Parameters$Wf3-S$Parameters$Wm3))

      #9/ Soil water deficit
      if(S$Parameters$REWc*S$Parameters$EWMtot-S$Table_Day$EW_tot[i]>0){
        S$Table_Day$SWD[i]= S$Parameters$REWc*S$Parameters$EWMtot-S$Table_Day$EW_tot[i]
      }else{S$Table_Day$SWD[i]= 0}

      #10/ Latent (LEmod) and Sensible (Hmod) heat fluxes, In kgH2O m-2 d-1 * MJ kgH2O-1 = MJ m-2 d-1
      S$Table_Day$LE_Plot[i]= S$Table_Day$ETR[i]*S$Parameters$lambda#kgH2O m-2 d-1 * MJ kgH2O-1

      S$Table_Day$LE_Coffee[i]=
        (S$Table_Day$T_Cof[i]+S$Table_Day$IntercRevapor[i]*
           (S$Table_Day$LAI[i]/S$Table_Day$LAIplot[i]))*S$Parameters$lambda
      # S$Table_Day$H_Coffee[i]= S$Table_Day$Rn_Coffee[i]-S$Table_Day$LE_Coffee[i]

      # Metamodel for H :
      S$Table_Day$H_Coffee[i]=
        -1.80160 + 0.03139*S$Met_c$Tair[i] - 0.06046*S$Met_c$VPD[i]+
        1.93064*(1-S$Met_c$FDiff[i]) + 0.58368*PARcof+
        0.25838*S$Table_Day$LAI[i]


      S$Table_Day$Rn_Coffee[i]=
        S$Table_Day$H_Coffee[i] + S$Table_Day$LE_Coffee[i]

      S$Table_Day$LE_Soil[i]= S$Table_Day$E_Soil[i]*S$Parameters$lambda

      S$Table_Day$H_Soil[i]= S$Table_Day$Rn_Soil[i]*(1-S$Parameters$Soil_LE_p)

      S$Table_Day$Q_Soil[i]= 0
      # RV: Q_Soil is negligible at yearly time-step, and equilibriate between several
      # days anyway.
      S$Table_Day$Rn_Soil[i]=
        S$Table_Day$H_Soil[i] + S$Table_Day$LE_Soil[i] + S$Table_Day$Q_Soil[i]


      # Tree LE and Rn (can not compute them in the Tree function because we need IntercRevapor)

      S$Table_Day$LE_Tree[i]=
        (S$Table_Day$T_Tree[i]+S$Table_Day$IntercRevapor[i]*
           (S$Table_Day$LAI_Tree[i]/S$Table_Day$LAIplot[i]))*S$Parameters$lambda
      S$Table_Day$Rn_Tree[i]= S$Table_Day$H_Tree[i] + S$Table_Day$LE_Tree[i]

      # Total plot heat flux:
      S$Table_Day$H_tot[i]= S$Table_Day$H_Coffee[i]+S$Table_Day$H_Tree[i]+
        S$Table_Day$H_Soil[i]
      # Total plot latent flux:
      S$Table_Day$LE_tot[i]=
        S$Table_Day$LE_Coffee[i]+S$Table_Day$LE_Tree[i]+
        S$Table_Day$LE_Soil[i]

      # Total plot net radiation:
      S$Table_Day$Rn_tot[i]= S$Table_Day$Rn_Coffee[i]+S$Table_Day$Rn_Tree[i]+
        S$Table_Day$Rn_Soil[i]


      #11/ Tcanopy Coffee : using bulk conductance if no trees, interlayer conductance if trees
      if(S$Table_Day$Height_Tree[i-S$Zero_then_One[i]]>S$Parameters$Height_Coffee){

        S$Table_Day$TairCanopy[i]=
          S$Table_Day$TairCanopy_Tree[i]+(S$Table_Day$H_Coffee[i]*Parameters$MJ_to_W)/
          (bigleaf::air.density(S$Table_Day$TairCanopy_Tree[i],S$Met_c$Pressure[i]/10)*
             S$Parameters$Cp*
             G_interlay(Wind= S$Met_c$WindSpeed[i], ZHT = S$Parameters$ZHT,
                        LAI_top= S$Table_Day$LAI_Tree[i-S$Zero_then_One[i]],
                        LAI_bot= S$Table_Day$LAI[i-S$Zero_then_One[i]],
                        Z_top= S$Table_Day$Height_Tree[i-S$Zero_then_One[i]],
                        extwind = S$Parameters$extwind))

        S$Table_Day$Tleaf_Coffee[i]=
          S$Table_Day$TairCanopy_Tree[i]+(S$Table_Day$H_Coffee[i]*Parameters$MJ_to_W)/
          (bigleaf::air.density(S$Table_Day$TairCanopy_Tree[i],S$Met_c$Pressure[i]/10)*
             S$Parameters$Cp*
             1/(1/G_interlay(Wind= S$Met_c$WindSpeed[i], ZHT = S$Parameters$ZHT,
                             LAI_top= S$Table_Day$LAI_Tree[i-S$Zero_then_One[i]],
                             LAI_bot= S$Table_Day$LAI[i-S$Zero_then_One[i]],
                             Z_top= S$Table_Day$Height_Tree[i-S$Zero_then_One[i]],
                             extwind = S$Parameters$extwind)+
                  1/Gb_h(Wind = S$Met_c$WindSpeed[i], wleaf= S$Parameters$wleaf,
                         LAI_lay=S$Table_Day$LAI[i-S$Zero_then_One[i]],
                         LAI_abv=S$Table_Day$LAI_Tree[i-S$Zero_then_One[i]],
                         ZHT = S$Parameters$ZHT,
                         Z2 = S$Table_Day$Height_Tree[i-S$Zero_then_One[i]],
                         extwind= S$Parameters$extwind)))


      }else{
        S$Table_Day$TairCanopy[i]=
          S$Table_Day$TairCanopy_Tree[i]+(S$Table_Day$H_Coffee[i]*Parameters$MJ_to_W)/
          (bigleaf::air.density(S$Table_Day$TairCanopy_Tree[i],S$Met_c$Pressure[i]/10)*
             S$Parameters$Cp*
             G_bulk(Wind = S$Met_c$WindSpeed[i], ZHT = S$Parameters$ZHT,
                    Z_top = S$Parameters$Height_Coffee,
                    LAI = S$Table_Day$LAI[i-S$Zero_then_One[i]],
                    extwind = S$Parameters$extwind))

        S$Table_Day$Tleaf_Coffee[i]=
          S$Table_Day$TairCanopy_Tree[i]+(S$Table_Day$H_Coffee[i]*Parameters$MJ_to_W)/
          (bigleaf::air.density(S$Table_Day$TairCanopy_Tree[i],S$Met_c$Pressure[i]/10)*
             S$Parameters$Cp*
             1/(1/G_bulk(Wind = S$Met_c$WindSpeed[i], ZHT = S$Parameters$ZHT,
                         Z_top = S$Parameters$Height_Coffee,
                         LAI = S$Table_Day$LAI[i-S$Zero_then_One[i]],
                         extwind = S$Parameters$extwind)+
                  1/Gb_h(Wind= S$Met_c$WindSpeed[i], wleaf= S$Parameters$wleaf,
                         LAI_lay= S$Table_Day$LAI[i-S$Zero_then_One[i]],
                         LAI_abv= S$Table_Day$LAI_Tree[i-S$Zero_then_One[i]],
                         ZHT= S$Parameters$ZHT,
                         Z_top= S$Parameters$Height_Coffee,
                         extwind= S$Parameters$extwind)))
      }
      # NB : if no trees, TairCanopy_Tree= Tair

    }
    list(Table_Day= S$Table_Day)
  }
  snow::stopCluster(cl)
  # Reordering the lists to make the results as previous versions of the model:
  Table= data.frame(do.call(rbind, CycleList[c(1:NCycles)]))

  # Force to keep interesting output variables only:
  UnwantedVarnames= c('.Fruit_Cohort',"Bud_available","BudBreak_cohort")
  Table= Table[,!colnames(Table)%in%UnwantedVarnames]

  attr(Table,"unit")= data.frame(varnames)

  cat("\n", "Simulation completed successfully", "\n")
  FinalList= list(Table_Day= Table,Met_c= Meteo, Parameters= Parameters)
  if(WriteIt){
    write.results(FinalList,output_f,Simulation_Name,Outpath,...)
  }
  if(returnIt){
    return(FinalList)
  }
}
