# Main Code
# Purpose : Prototype for GO+
# Daily time-step, gC m-2 ou gC m-2 d-1; Works for an average elementary space
# Only coffee simulated for the moment, being in the shade conditions of Aquiares, similar in Tarrazu.
# Model based on the laws of minimum between Supply and Demand.
# Resprout wood, aerial stump and coarse roots are fed first, according to a constant allocation factor
# Second come Flower buds, that were initiated during the previous year, adapting the Rodriguez et al.,
# 2011 model, a  function of the ratio nodes to LAI, thermal time, and a negative function of LAI
# Third come fruits, that are fed according to a % of mature resprouts & to the number of initiated
# flower buds.
# Fruits start from flowering buds that trigger in cohorts, then grow in cohorts according to a
# logistic function.
# Fourth come leaves. Leaf mortality is enhanced by American Leaf Spot (Ojo de Gallo) (module de
# Avelino et al., 2007).
# Last come fine roots.
# The remainder of C recharges reserves. C from Reserves can be used sparingly, it contributes to C
# supply computation in the next cycle.
# Sementic: Variable_Tree = variable for shade tree
# Contacts : O Roupsard & Guerric le Maire & Remi Vezy & Jacques Avelino

# Defining the class SimulationClass to leverage R Reference classes:





#' Dynamic Agroforestry Coffee Crop Model
#'
#' @description The DynACof process-based model computes plot-scale Net Primary Productivity, carbon allocation, growth, yield,
#'              energy, and water balance of coffee plantations according to management, while accounting for spatial effects using
#'              metamodels from the 3D process-based MAESPA. The model also uses coffee bud and fruit cohorts for reproductive
#'              development to better represent fruit carbon demand distribution along the year.
#'
#' @param file        Either the file name to read, a shell command that preprocesses the file (e.g. fread("grep blah filename"))
#'                    or the input itself as a string, see \code{\link[data.table]{fread}}. In both cases, a length 1 character string.
#'                    A filename input is passed through path.expand for convenience and may be a URL starting http:// or file://
#' @param Period      The desired time period to be returned inthe form of a vector of two POSIX dates (min and max).
#' @param WriteIt     If TRUE, will write the output of the function in the same path.
#' @param ...         Additional parameters to pass to the \code{\link[data.table]{fwrite}} function (sep is fixed to \code{;}
#'                    and colnames to \code{T}).
#' @param Parameters  A list of parameters:
#' \itemize{
#'   \item Start_Date: optional, the date of the first meteo file record. Only needed if the Date column is missing (Posixct).
#'   \item FPAR      : Fraction of global radiation that is PAR (source: MAESPA model), only needed if either RAD or PAR is missing (fraction).
#'   \item Elevation : elevation of the site, only needed if atmospheric pressure is missing (m)
#'   \item Latitude  : latitude of the site, only needed if the diffuse fraction of light is missing (degree)
#'   \item WindSpeed : constant wind speed, only needed if windspeed is missing (m s-1)
#'   \item CO2       : constant atmospheric \eqn{CO_2} concentration, only needed if \eqn{CO_2} is missing (ppm)
#'   \item MinTT     : minimum temperature threshold for degree days computing, see \code{\link{GDD}}
#'   \item MaxTT     : maximum temperature threshold for degree days computing, see \code{\link{GDD}}
#'   \item albedo    : site shortwave surface albedo, only needed if net radiation is missing, see \code{\link{Rad_net}}
#' }
#'
#'
#' @details The imported file is expected to be at daily time-step.
#'          The albedo is used to compute the system net radiation that is then used to compute the soil net radiation using an
#'          extinction coefficient with the plot LAI following the Shuttleworth & Wallace (1985) formulation. This computation is
#'          likely to change in the near future to add a more uniform process-based formulation (such as Choudhury & Monteith 1988).
#'
#' @return \item{A daily timestep meteorology data.frame with different columns : \tabular{ll}{\strong{Var} \tab \strong{unit}\cr
#'                    year            \tab year        \cr
#'                     DOY             \tab day         \cr
#'                     Date            \tab POSIXct date\cr
#'                     Rain            \tab mm          \cr
#'                     Tair            \tab deg C       \cr
#'                     RH              \tab %           \cr
#'                     RAD             \tab MJ m-2 d-1  \cr
#'                     Pressure        \tab hPa         \cr
#'                     WindSpeed       \tab m s-1       \cr
#'                     CO2             \tab ppm         \cr
#'                     DegreeDays      \tab deg C       \cr
#'                     PAR             \tab MJ m-2 d-1  \cr
#'                     FDiff           \tab Fraction    \cr
#'                     VPD             \tab hPa         \cr
#'                     Rn              \tab MJ m-2 d-1  \cr
#'                     Tmax            \tab deg C       \cr
#'                     Tmin            \tab deg C       \cr
#'                     rho             \tab kg m-3      \cr
#'                     DaysWithoutRain \tab day
#' }}
#'
#' @author R. Vezy; O. Roupsard
#'
#' @export
GoPlus= function(Period=NULL, WriteIt= F,returnIt=F,
                 output=".RData",...,path="1-Input/Default",Site="1-Site.R",Meteo="2-Meteorology.txt",
                 Soil="3-Soil.R",Coffee="4-Coffee.R",Tree=NULL){

    SimulationClass <- setRefClass("Simulation",
                                   fields = list(Table_Day = "data.frame",
                                                 Met_c= "data.frame",
                                                 PerCohortFruitDemand_c="matrix",
                                                 Parameters= "list",
                                                 Zero_then_One="vector"))

    # Importing the parameters ------------------------------------------------

    Parameters= Import_Parameters(path = path, Names = list(Site,Soil,Coffee,Tree))

    # Importing the meteo -----------------------------------------------------

    Meteo= Meteo_n_Astronomy(file=file.path(path,Meteo),Period= Period,Parameters= Parameters,WriteIt = F,
                             ClimateChange= ClimateChange,CO2Increase= CO2Increase)


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
    NbCores= detectCores()-1 # Set the maximum number of cores working on the model computation
    cl=makeCluster(min(NbCores,NCycles))
    registerDoSNOW(cl)
    CycleList= foreach(cy= 1:NCycles,.combine=rbind,.packages = c("dplyr","zoo")) %dopar% {
        source(file = "2-Code/00-Functions.R")

        # Initializing the Simulation object:

        S= SimulationClass$new()
        S$Parameters= Parameters

        # Initializing the table:
        S$Table_Day= Direction[Direction$Cycle==cy,]
        S$Met_c= Meteo[Direction$Cycle==cy,]
        Init_Table_Day(S)
        # Compute cumulative degree-days based on previous daily DD from semi-hourly data:
        S$Table_Day$CumulDegreeDays= cumsum(S$Met_c$DegreeDays)
        # initializing cohort matrix:
        S$PerCohortFruitDemand_c= matrix(NA, ncol=length(S$Met_c$year), nrow= S$Parameters$MaxCohorts)

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
        VegetGrowthEndDay= which(S$Met_c$DOY==S$Parameters$date_VegetGrowSeason_Stop)
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
                S$Table_Day$CumulDegreeDays-S$Table_Day$CumulDegreeDays[VegetGrowthEndDay[i]-1]
            # Date of first bud initialisation:
            DateBudinit[i]= tail(which(CumsumRelativeToVeget[i,]<S$Parameters$Tffb),1)
            CumsumRelativeToBudinit[i,]=
                S$Table_Day$CumulDegreeDays-S$Table_Day$CumulDegreeDays[DateBudinit[i]-1]
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
        S$Table_Day$BudInitPeriod[S$Table_Day$CumulDegreeDays<S$Parameters$VF_Flowering]= FALSE

        # Search for the species specific tree function:
        if(S$Parameters$Tree_Species=="No_Shade"){
            Treefun= No_Shade
        }else{
            Treefun= Shade.Tree
        }

        # American Leaf Spot:
        S$Table_Day$ALS= ALS(Elevation= S$Parameters$Elevation, SlopeAzimut= S$Parameters$SlopeAzimut,
                             Slope= S$Parameters$Slope, RowDistance= S$Parameters$RowDistance,
                             Shade= S$Parameters$Shade, CanopyHeight.Coffee= S$Parameters$CanopyHeight.Coffee,
                             Fertilization= S$Parameters$Fertilization, ShadeType= S$Parameters$ShadeType,
                             CoffeePruning= S$Parameters$CoffeePruning, df_rain= S$Met_c)

        # Main Loop -----------------------------------------------------------------------------------

        for (i in 1:nrow(S$Table_Day)){


            # Shade Tree computation if any

            Treefun(S,i)
            # Should output at least APAR_Tree, LAI_Tree, T_Tree_mmd, RnTree_MJm2d, H_Tree_MJm2d,
            # LE_Tree_MJm2d (sum of transpiration + leaf evap), DBH_Tree_cm (for LUE),
            # CrownProj_Tree_m2, DBH_Tree_cm, CrownH_Tree_m


            # Coffee computation:

            # Metamodels for K, Paper 2, trained on 2011 only :
            S$Table_Day$K_Dif_Cof[i]= 0.39
            S$Table_Day$K_Dir_Cof[i]= 0.34

            # # Metamodels for K, Paper 3, trained on all ages and structure :
            # S$Table_Day$K_Dif_Cof[i]= 0.40
            # S$Table_Day$K_Dir_Cof[i]= 0.35

            #APAR coffee
            PARcof= S$Met_c$PAR[i]-S$Table_Day$APAR_Tree[i] # PAR above coffee layer

            S$Table_Day$APAR_Dif[i]=
                max(0,((S$Met_c$PAR[i]-S$Table_Day$APAR_Tree[i])*S$Met_c$FDiff[i])*
                       (1-exp(-S$Table_Day$K_Dif_Cof[i]*S$Table_Day$LAI[i-S$Zero_then_One[i]])))#MJ m-2 d-1
            APAR_Dir= max(0,((S$Met_c$PAR[i]-S$Table_Day$APAR_Tree[i])*(1-S$Met_c$FDiff[i]))*
                               (1-exp(-S$Table_Day$K_Dir_Cof[i]*S$Table_Day$LAI[i-S$Zero_then_One[i]])))#MJ m-2 d-1
            # APAR_Dir is not part of S$Table_Day because it can be easily computed by
            # S$Met_c$PARm2d1-S$Table_Day$APAR_Dif
            S$Table_Day$APAR[i]= APAR_Dir+S$Table_Day$APAR_Dif[i]
            # S$Table_Day$APAR[i]= max(0,(S$Met_c$PAR[i]-S$Table_Day$APAR_Tree[i])*(1-
            # exp(-S$Table_Day$k[i]*S$Table_Day$LAI[i-S$Zero_then_One[i]])))#MJ m-2 d-1
            S$Table_Day$PAR_Soil[i]=
                S$Met_c$PAR[i]-S$Table_Day$APAR_Tree[i]-S$Table_Day$APAR[i]

            # Metamodel Coffee Tcanopy, Paper 2
            S$Table_Day$Tcan_MAESPA_Coffee_degC[i]=
                -0.07741 + 0.99456*S$Met_c$Tair[i] - 0.06948*S$Met_c$VPD[i] -
                1.87975*(1-S$Met_c$FDiff[i]) + 0.19615*PARcof
            S$Table_Day$DegreeDays_Tcan[i]=
                degree_days_hourly(S$Table_Day$Tcan_MAESPA_Coffee_degC[i],S$Parameters$MinTT,S$Parameters$MaxTT)
            # NB : Tcan_MAESPA_Coffee_degC is never <10 so we can compute the degree days using the hourly
            # formula directly

            # S$Table_Day$Tcan_Diurnal_Cof_deg[i]=
            #     0.90479 + 0.97384*S$Met_c$Diurnal_TAIR[i] + 0.24677*PARcof + 0.01163*S$Met_c$VPD[i] -
            #     2.53554*(1-S$Met_c$FDiff[i]) - 0.04597*S$Table_Day$LAI_Tree[i]
            # NB 1: MAESPA simulates a Coffee canopy temperature (= average leaf temperature) very similar
            # to the air temperature within the canopy, so we can use it interchangeably.
            # NB 2: could use sqrt of FBEAM and PARCof for a better fit (little less rmse) but we keep the
            # metamodel without it to decrease the risk of any overfitting or issue on extrapolation.


            # Metamodel Coffee Tcanopy, Paper 3
            # S$Table_Day$Tcan_MAESPA_Coffee_degC[i]=
            #     0.92921 + 0.95568*S$Met_c$Tair[i] + 0.01241*S$Met_c$VPD[i] -
            #     0.47802*(1-S$Met_c$FDiff[i]) + 0.10599*PARcof-
            #     0.04573*S$Table_Day$LAI_Tree[i]
            # S$Table_Day$Tcan_Diurnal_Cof_deg[i]=
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
            #     0.001167*S$Met_c$CO2_ppm[i] - 0.012697*S$Table_Day$Tcan_MAESPA_Coffee_degC[i]
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
                     S$Parameters$Q10RsWood^((S$Table_Day$Tcan_MAESPA_Coffee_degC[i]-S$Parameters$TMR)/10))

            # Stump and Coarse roots (perennial wood)
            S$Table_Day$Rm_StumpCoarseRoot[i]=
                S$Zero_then_One[i]*
                (S$Parameters$PaliveStumpCoarseRoot*
                     S$Table_Day$DM_StumpCoarseRoot[i-S$Zero_then_One[i]]*
                     S$Parameters$NContentStumpCoarseRoot*S$Parameters$MRN*
                     S$Parameters$Q10StumpCoarseRoot^(
                         (S$Table_Day$Tcan_MAESPA_Coffee_degC[i]-S$Parameters$TMR)/10))

            # Fruits
            S$Table_Day$Rm_Fruit[i]=
                S$Zero_then_One[i]*
                (S$Parameters$PaliveFruit*S$Table_Day$DM_Fruit[i-S$Zero_then_One[i]]*
                     S$Parameters$NContentFruit*S$Parameters$MRN*
                     S$Parameters$Q10Fruit^((S$Table_Day$Tcan_MAESPA_Coffee_degC[i]-S$Parameters$TMR)/10))
            # Leaves
            S$Table_Day$Rm_Leaf[i]=
                S$Zero_then_One[i]*
                (S$Parameters$PaliveLeaf*S$Table_Day$DM_Leaf[i-S$Zero_then_One[i]]*
                     S$Parameters$NContentLeaf*S$Parameters$MRN*
                     S$Parameters$Q10Leaf^((S$Table_Day$Tcan_MAESPA_Coffee_degC[i]-S$Parameters$TMR)/10))

            # Fine roots
            S$Table_Day$Rm_FineRoot[i]=
                S$Zero_then_One[i]*
                (S$Parameters$PaliveFineRoot*S$Table_Day$DM_FineRoot[i-S$Zero_then_One[i]]*
                     S$Parameters$NContentFineRoot*S$Parameters$MRN*
                     S$Parameters$Q10FineRoot^((S$Table_Day$Tcan_MAESPA_Coffee_degC[i]-S$Parameters$TMR)/10))

            # Total plant maintenance respiration
            S$Table_Day$Rm[i]=
                S$Table_Day$Rm_Fruit[i]+S$Table_Day$Rm_Leaf[i]+
                S$Table_Day$Rm_RsWood[i]+S$Table_Day$Rm_StumpCoarseRoot[i]+
                S$Table_Day$Rm_FineRoot[i]


            ##############################----- Coffee Allocation ----##############################


            # Offer function ----------------------------------------------------------
            S$Table_Day$OfferFunction[i]=
                max(S$Table_Day$GPP[i]-S$Table_Day$Rm[i]+S$Table_Day$Consumption_RE[i],0)

            # If the respiration is greater than the GPP + reserves use, then take this carbon
            # from mortality of each compartments' biomass equally (not for fruits or reserves):
            S$Table_Day$Carbon_Lack_Mortality[i]=
                -min(0,S$Table_Day$GPP[i]-S$Table_Day$Rm[i]+S$Table_Day$Consumption_RE[i])


            # 1-Resprout wood ---------------------------------------------------------
            # Allocation priority 1, see Charbonnier 2012.

            # Offer
            S$Table_Day$PactRc_RsWood[i]= S$Parameters$lambdaRsWood*S$Table_Day$OfferFunction[i]
            # NPP (Offer-Rc)
            S$Table_Day$NPP_RsWood[i]= S$Parameters$epsilonRsWood*S$Table_Day$PactRc_RsWood[i]
            # Rc (growth respiration
            S$Table_Day$Rc_RsWood[i]= (1-S$Parameters$epsilonRsWood)*S$Table_Day$PactRc_RsWood[i]
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
            S$Table_Day$Mact_RsWood[i]=
                min((S$Table_Day$Mnat_RsWood[i]+S$Table_Day$Mprun_RsWood[i]),
                    S$Table_Day$CM_RsWood[i-S$Zero_then_One[i]])

            # 2-Stump and coarse roots (perennial wood) ------------------------------
            # coef d'alloc is more important for old ages, see Defrenet et al., 2016
            S$Table_Day$lambdaStumpCoarseRootage[i]=
                S$Parameters$lambdaStumpCoarseRoot0-
                S$Table_Day$Plot_Age[i]/40*
                (S$Parameters$lambdaStumpCoarseRoot0-S$Parameters$lambdaStumpCoarseRoot40)
            #Offer
            S$Table_Day$PactRc_StumpCoarseRoot[i]=
                S$Table_Day$lambdaStumpCoarseRootage[i]*S$Table_Day$OfferFunction[i]
            #NPP
            S$Table_Day$NPP_StumpCoarseRoot[i]=
                S$Parameters$epsilonStumpCoarseRoot*S$Table_Day$PactRc_StumpCoarseRoot[i]
            #Rc
            S$Table_Day$Rc_StumpCoarseRoot[i]=
                (1-S$Parameters$epsilonStumpCoarseRoot)*S$Table_Day$PactRc_StumpCoarseRoot[i]
            #Mortality
            S$Table_Day$Mnat_StumpCoarseRoot[i]=
                S$Table_Day$CM_StumpCoarseRoot[i-S$Zero_then_One[i]]/S$Parameters$lifespanStumpCoarseRoot
            S$Table_Day$Mact_StumpCoarseRoot[i]= S$Table_Day$Mnat_StumpCoarseRoot[i]

            # Ratio of number of new nodes per LAI unit as affected by air temperature according to
            # Drinnan & Menzel, 1995
            #Source "0 Effect T on yield and vegetative growth.xlsx", sheet "Std20dComposWinterNodeperBr"
            # NB: computed at the end of the vegetatitve growth only to have Tcan of the
            # whole period already computed
            # NB2 : This is the total number of productive nodes on the coffee plant, i.e. the number of
            # green wood nodes that potentially carry flower buds. Green wood mass (and so number of nodes)
            # are related to leaf area (new leaves appear on nodes) : GUTIERREZ et al. (1998)
            if(S$Met_c$DOY[i]==S$Parameters$date_VegetGrowSeason_Stop){
                S$Table_Day$ratioNodestoLAI[S$Met_c$year>=S$Met_c$year[i]]=
                    S$Table_Day[S$Met_c$year==S$Met_c$year[i]&
                                    S$Met_c$DOY>=S$Parameters$date_VegetGrowSeason_Start&
                                    S$Met_c$DOY <= S$Parameters$date_VegetGrowSeason_Stop,]%>%
                    summarise(AverTaVegGrowSeason_Year= mean(Tcan_MAESPA_Coffee_degC))%>%
                    mutate(Perc_Nodes_compared_to_20deg_ref=
                               0.0005455*AverTaVegGrowSeason_Year^3 - 0.0226364*
                               AverTaVegGrowSeason_Year^2+0.2631364*
                               AverTaVegGrowSeason_Year + 0.4194773)%>%
                    transmute(ratio_Nodes_to_LAI=
                                  S$Parameters$ratioNodestoLAI_REF20deg*Perc_Nodes_compared_to_20deg_ref)%>%
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
            # if(mean(S$Table_Day$Tcan_Diurnal_Cof_deg[DormancyBreakPeriod])>23){
            #     S$Table_Day$Temp_cor_Bud[DormancyBreakPeriod]=
            #         (3.29 - 0.1*mean(S$Table_Day$Tcan_Diurnal_Cof_deg[DormancyBreakPeriod]))
            # }
            # (7.2) Using daily temperature (simpler):
            # Data_Buds_day= data.frame(Air_T=c(15.5,20.5,25.5,30.5), # diurnal data only
            #                           Buds_per_Node=c(2.6,3.2,1.5,0))
            # Data_Buds_day= Data_Buds_day[-1,]
            # Data_Buds_day$Buds_per_Node_cor= Data_Buds_day$Buds_per_Node/Data_Buds_day$Buds_per_Node[1]
            # lmbuds_day= lm(Buds_per_Node_cor~Air_T,data=Data_Buds_day)
            if(mean(S$Table_Day$Tcan_MAESPA_Coffee_degC[DormancyBreakPeriod])>23){
                S$Table_Day$Temp_cor_Bud[DormancyBreakPeriod]=
                    (3.04 - 0.1*mean(S$Table_Day$Tcan_Diurnal_Cof_deg[DormancyBreakPeriod]))
            }

            # (6) Bud dormancy break, Source, Drinnan 1992 and Rodriguez et al., 2011 eq. 13
            S$Table_Day$p_budbreakperday= 1/(1+exp(S$Parameters$a_p+S$Parameters$b_p*
                                                       S$Table_Day$LeafWaterPotential_MPa[i-S$Zero_then_One[i]]))
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

            # Demand from each fruits cohort present on the coffee tree (not overriped), same as PDem_FruitYear but keeping each value :
            PDem_Fruit_Cohort_Period=
                S$Table_Day$BudBreak[FruitingPeriod]*S$Parameters$Opti_C_DemandFruit*
                F_Integ_Dens(DegreeDay_i,FruitingPeriod,S$Parameters$u_log,S$Parameters$s_log)
            PDem_Fruit_Cohort_Period[is.na(PDem_Fruit_Cohort_Period)]= 0
            # Total C demand of the fruits :
            S$Table_Day$PDem_FruitYear[i]= sum(PDem_Fruit_Cohort_Period)
            # C offer to Fruits (i.e. what is left from OfferFunction after removing the consumption by previous compartments and Rm):
            S$Table_Day$Poff_Fruit[i]=
                S$Table_Day$OfferFunction[i]-S$Table_Day$PactRc_RsWood[i]-
                S$Table_Day$PactRc_StumpCoarseRoot[i]

            # Total C allocation to all fruits on day i :
            S$Table_Day$PactRc_Fruit[i]= min(S$Table_Day$PDem_FruitYear[i],S$Table_Day$Poff_Fruit[i])
            # Allocation to each cohort, relative to each cohort demand :
            S$Table_Day$PactRc_Fruit_Cohort[FruitingPeriod]=
                S$Table_Day$PactRc_Fruit[i]*(PDem_Fruit_Cohort_Period/S$Table_Day$PDem_FruitYear[i])
            S$Table_Day$PactRc_Fruit_Cohort[FruitingPeriod][is.nan(S$Table_Day$PactRc_Fruit_Cohort[FruitingPeriod])]= 0
            S$Table_Day$NPP_Fruit_Cohort[FruitingPeriod]=
                S$Parameters$epsilonFruit*S$Table_Day$PactRc_Fruit_Cohort[FruitingPeriod]
            S$Table_Day$CM_Fruit_Cohort[FruitingPeriod]= S$Table_Day$CM_Fruit_Cohort[FruitingPeriod]+
                S$Table_Day$NPP_Fruit_Cohort[FruitingPeriod]
            S$Table_Day$DM_Fruit_Cohort[FruitingPeriod]= S$Table_Day$CM_Fruit_Cohort[FruitingPeriod]/S$Parameters$CContent_Fruit
            # Overriped fruits that fall onto the ground (= to mass of the cohort that overripe) :
            S$Table_Day$Overriped_Fruit[i]= S$Table_Day$CM_Fruit_Cohort[max(min(FruitingPeriod)-1,1)]
            # S$Table_Day$Overriped_Fruit[i]= S$Table_Day$CM_Fruit_Cohort[min(FruitingPeriod)-1]*S$Parameters$epsilonFruit

            # Duration of the maturation of each cohort born in the ith day (in days):
            S$Table_Day$Maturation_duration_d[FruitingPeriod]=
                seq_along(FruitingPeriod)
            # Sucrose content of each cohort:
            S$Table_Day$Sucrose_Content[FruitingPeriod]=
                Sucrose_cont_perc(S$Table_Day$Maturation_duration_d[FruitingPeriod],
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
                S$Table_Day$CM_Fruit[i-1]= S$Table_Day$PactRc_Fruit[i]=
                    S$Table_Day$CM_Fruit_Cohort= S$Table_Day$Overriped_Fruit[i]= 0
                # RV: could harvest mature fruits only (To do).
            }else{
                S$Table_Day$Harvest_Fruit[i]= NA_real_
            }

            S$Table_Day$NPP_Fruit[i]= S$Parameters$epsilonFruit*S$Table_Day$PactRc_Fruit[i]
            S$Table_Day$Rc_Fruit[i]= (1-S$Parameters$epsilonFruit)*S$Table_Day$PactRc_Fruit[i]



            ############# Leaves ####
            #offer : what is left after deduction of the previous compartments
            #GlM: ce qui reste apres production de bois, grains et CepsRacine(=
            #offre)est repartis entre racine et feuilles selon la meme proportion
            #que la repartition de la NPP entre Racines et feuilles. Les lambdas ne
            #peuvent etre utilises que sur NPP totale
            S$Table_Day$Poff_Leaf[i]=
                S$Parameters$lambdaLeaf_remain*
                (S$Table_Day$OfferFunction[i]-S$Table_Day$PactRc_Fruit[i]-
                     S$Table_Day$PactRc_RsWood[i]-S$Table_Day$PactRc_StumpCoarseRoot[i])
            #Demand : the demand is actually S$Parameters$PDem_Leaf
            #Min(Demand, Offer)
            S$Table_Day$PactRc_Leaf[i]=max(0,min(S$Parameters$PDem_Leaf, S$Table_Day$Poff_Leaf[i]))
            #NPP
            S$Table_Day$NPP_Leaf[i]= S$Parameters$epsilonLeaf*S$Table_Day$PactRc_Leaf[i]
            #Rc
            S$Table_Day$Rc_Leaf[i]= (1-S$Parameters$epsilonLeaf)*S$Table_Day$PactRc_Leaf[i]
            #Excess into Reserves
            S$Table_Day$NPP_RE[i]= S$Table_Day$NPP_RE[i]+(S$Table_Day$Poff_Leaf[i]-S$Table_Day$PactRc_Leaf[i])

            #Mortality
            # By natural litterfall assuming no diseases
            S$Table_Day$Mnat_Leaf[i]=S$Table_Day$CM_Leaf[i-S$Zero_then_One[i]]/S$Parameters$lifespanLeaf
            # By American Leaf Spot # Litterfall by ALS is difference between 2 dates
            S$Table_Day$M_ALS[i]=
                S$Zero_then_One[i]*max(0,S$Table_Day$CM_Leaf[i-S$Zero_then_One[i]]*S$Table_Day$ALS[i])
            # Total Mortality. This is what we observe in litter-traps
            S$Table_Day$MnatALS_Leaf[i]=S$Table_Day$Mnat_Leaf[i]+S$Table_Day$M_ALS[i]
            #By pruning
            if(S$Table_Day$Plot_Age[i]>=S$Parameters$MeanAgePruning&S$Met_c$DOY[i]==S$Parameters$date_pruning){
                S$Table_Day$Mprun_Leaf[i]=S$Table_Day$CM_Leaf[i-S$Zero_then_One[i]]*
                    S$Parameters$LeafPruningRate}else{S$Table_Day$Mprun_Leaf[i]=0}
            #En fait, c'est une plus grande fraction que 1/(MeanAgePruning-1) = 1/5 qui part,
            # car ca touche les vieux rejets.
            #OR: Creer un parametre exprimant nbre feuilles dans la derniere classe d'age pour cela
            S$Table_Day$Mact_Leaf[i]= S$Table_Day$MnatALS_Leaf[i] + S$Table_Day$Mprun_Leaf[i]


            ############# Fine Roots #####
            #Demand : the demand is actually S$Parameters$PDem_Leaf
            #S$Table_Day$PDem_FineRoot[i]= S$Table_Day$PactRc_Leaf[i]
            S$Table_Day$PDem_FineRoot[i]= S$Parameters$PDem_Leaf
            # f(T,SW     D,...)#suppose egalite entre feuilles et racines fines pour demande
            #Offer
            S$Table_Day$Poff_FineRoot[i]=
                S$Parameters$lambdaFineRoot_remain*
                (S$Table_Day$OfferFunction[i]-S$Table_Day$PactRc_Fruit[i]-
                     S$Table_Day$PactRc_RsWood[i]-S$Table_Day$PactRc_StumpCoarseRoot[i])
            #min (Demand, Offer)
            S$Table_Day$PactRc_FineRoot[i]=max(0,min(S$Table_Day$PDem_FineRoot[i],S$Table_Day$Poff_FineRoot[i]))
            #NPP
            S$Table_Day$NPP_FineRoot[i]=S$Parameters$epsilonFineRoot*S$Table_Day$PactRc_FineRoot[i]
            #Rc
            S$Table_Day$Rc_FineRoot[i]=(1-S$Parameters$epsilonFineRoot)*S$Table_Day$PactRc_FineRoot[i]
            #Excess into reserves
            S$Table_Day$NPP_RE[i]= S$Table_Day$NPP_RE[i]+(S$Table_Day$Poff_FineRoot[i]-S$Table_Day$PactRc_FineRoot[i])

            #Mortality
            S$Table_Day$Mnat_FineRoot[i]=S$Table_Day$CM_FineRoot[i-S$Zero_then_One[i]]/S$Parameters$lifespanFineRoot
            S$Table_Day$Mprun_FineRoot[i]=S$Parameters$MortalityRateFineRootprun*S$Table_Day$Mprun_Leaf[i]
            S$Table_Day$Mact_FineRoot[i]=S$Table_Day$Mnat_FineRoot[i]+S$Table_Day$Mprun_FineRoot[i]


            ############# Update of Biomass & Rm, Rc, Ra & LAI & & Buds ####



            S$Table_Day$CM_Leaf[i]=S$Table_Day$CM_Leaf[i-S$Zero_then_One[i]]+
                S$Table_Day$NPP_Leaf[i]-S$Table_Day$Mact_Leaf[i]-
                S$Table_Day$Carbon_Lack_Mortality[i]*0.25
            S$Table_Day$CM_RsWood[i]= S$Table_Day$CM_RsWood[i-S$Zero_then_One[i]]+
                S$Table_Day$NPP_RsWood[i]-S$Table_Day$Mact_RsWood[i]-
                S$Table_Day$Carbon_Lack_Mortality[i]*0.25
            S$Table_Day$CM_Fruit[i]=S$Table_Day$CM_Fruit[i-S$Zero_then_One[i]]+
                S$Table_Day$NPP_Fruit[i]-S$Table_Day$Overriped_Fruit[i]
            # NB: S$Table_Day$Overriped_Fruit is negative (loss by falling if overriped)
            S$Table_Day$CM_StumpCoarseRoot[i]= S$Table_Day$CM_StumpCoarseRoot[i-S$Zero_then_One[i]]+
                S$Table_Day$NPP_StumpCoarseRoot[i]-S$Table_Day$Mact_StumpCoarseRoot[i]-
                S$Table_Day$Carbon_Lack_Mortality[i]*0.25
            S$Table_Day$CM_FineRoot[i]= S$Table_Day$CM_FineRoot[i-S$Zero_then_One[i]]+
                S$Table_Day$NPP_FineRoot[i]-S$Table_Day$Mact_FineRoot[i]-
                S$Table_Day$Carbon_Lack_Mortality[i]*0.25
            S$Table_Day$CM_RE[i]=S$Table_Day$CM_RE[i-S$Zero_then_One[i]]+S$Table_Day$NPP_RE[i]-
                S$Table_Day$Consumption_RE[i]
            S$Table_Day$Rc[i]= S$Table_Day$Rc_Fruit[i]+S$Table_Day$Rc_Leaf[i]+
                S$Table_Day$Rc_RsWood[i]+S$Table_Day$Rc_StumpCoarseRoot[i]+
                S$Table_Day$Rc_FineRoot[i]
            S$Table_Day$Ra[i]=S$Table_Day$Rm[i]+S$Table_Day$Rc[i]

            S$Table_Day$NPPtot[i]=S$Table_Day$NPP_RsWood[i]+S$Table_Day$NPP_StumpCoarseRoot[i]+
                S$Table_Day$NPP_Fruit[i]+S$Table_Day$NPP_Leaf[i]+S$Table_Day$NPP_FineRoot[i]

            #Daily C balance that should be null every day
            S$Table_Day$Cbalance[i]=S$Table_Day$OfferFunction[i]-
                S$Table_Day$NPPtot[i]-S$Table_Day$Rc[i]-S$Table_Day$NPP_RE[i]#-S$Table_Day$Consumption_RE[i])

            #Update LAI m2leaf m-2soil, caution, CM is in gC m-2soil, so use C content to transform in dry mass
            S$Table_Day$LAI[i]= S$Table_Day$CM_Leaf[i]*S$Parameters$SLA/1000/S$Parameters$CContent_Leaf

            # Dry mass computation:
            S$Table_Day$DM_Leaf[i]= S$Table_Day$CM_Leaf[i]/S$Parameters$CContent_Leaf
            S$Table_Day$DM_RsWood[i]= S$Table_Day$CM_RsWood[i]/S$Parameters$CContent_RsWood
            S$Table_Day$DM_Fruit[i]=S$Table_Day$CM_Fruit[i]/S$Parameters$CContent_Fruit
            S$Table_Day$DM_StumpCoarseRoot[i]= S$Table_Day$CM_StumpCoarseRoot[i]/
                S$Parameters$CContent_StumpCoarseRoot
            S$Table_Day$DM_FineRoot[i]= S$Table_Day$CM_FineRoot[i]/S$Parameters$CContent_FineRoots
            S$Table_Day$DM_RE[i]=S$Table_Day$CM_RE[i]/S$Parameters$CContent_StumpCoarseRoot

            ######################## Water balance (from BILJOU model) #############################

            # Soil Water potential, Campbell (1974) equation
            S$Table_Day$SoilWaterPot[i]=
                S$Parameters$PSIE*(((S$Table_Day$W1.mm[i-S$Zero_then_One[i]]+S$Table_Day$W2.mm[i-S$Zero_then_One[i]]+
                                       S$Table_Day$W3.mm[i-S$Zero_then_One[i]])/3750)/S$Parameters$PoreFrac)^(-S$Parameters$B)

            # Metamodel Coffee leaf water potential
            S$Table_Day$LeafWaterPotential_MPa[i]=
                0.040730 - 0.005074*S$Met_c$VPD[i] - 0.037518*PARcof + 2.676284*S$Table_Day$SoilWaterPot[i]

            # S$Table_Day$LeafWaterPotential_MPa[i]=
            #     -0.096845 - 0.080517*MetData$PARm2d1 +
            #     0.481117*(1-MetData$FDiff) - 0.001692*MetData$DaysWithoutRain

            # LAI plot is the sum of the LAI of the Tree + coffee LAI
            # (LAIplot[i] is first equal to 0, then added LAI of tree, and here adding LAI of coffee)
            S$Table_Day$LAIplot[i]= S$Table_Day$LAIplot[i]+S$Table_Day$LAI[i]

            #Rn or AE per layer (radiation reaching every layer, valid only during dailight hours,
            # not during night hours)
            # Rn understorey, source Shuttleworth & Wallace, 1985, eq. 21
            S$Table_Day$Rnu_MJm2d_OLD[i]=
                S$Met_c$Rn[i]*exp(-S$Parameters$k_Rn*S$Table_Day$LAIplot[i])
            # Available energy understorey source Shuttleworth & Wallace, 1985, eq. 2.
            # NB, For Tree and Coffee, AE=Rn=H+LE
            # S$Table_Day$AEu_MJm2d[i]= S$Table_Day$Rnu_MJm2d_OLD[i]*(1-S$Parameters$fG)
            S$Table_Day$AEu_MJm2d[i]= S$Table_Day$Rnu_MJm2d_OLD[i]
            # RV: soil heat storage is negligible at daily time-step (or will equilibrate soon),
            # removing it.
            # AE=Rn coffee : OR, after retrieving the fraction for the soil, I assumed it was partitionned according to LAI of coffee and tree

            #1/ Rainfall interception, source Gomez-Delgado et al.2011, Box A: IntercMax.mm=AX;
            # CanopyHumect.mm=At; IntercRevapor_mmd=RIn; ThSt_mmd=RTS
            #First step of computing, source OR
            S$Table_Day$IntercMax.mm[i]= S$Parameters$IntercSlope*S$Table_Day$LAIplot[i]
            # 0.133*3 = 0.4mm= Interception maximum capacity (mm) (from Siles et al. JH 2010),
            # with average LAI = 3

            #             S$Table_Day$CanopyHumect.mm[i]= max(0,S$Table_Day$CanopyHumect.mm[i-S$Zero_then_One[i]]+
            #                                                          S$Zero_then_One[i]*S$Met_c$Rain[i])
            # RV: Imbalance the first day if it was raining. Corrected here:
            S$Table_Day$CanopyHumect.mm[i]=
                max(0,S$Table_Day$CanopyHumect.mm[i-S$Zero_then_One[i]]+S$Met_c$Rain[i])
            #at this level it is the initial CanopyHumect.mm at the end of the day

            #Second step of computing, finally CanopyHumect.mm is stored at the end of the full day process,
            # i.e. after retrieving IntercRevapor_mmd
            Potential_LeafEvap=
                PENMON(Rn= S$Met_c$Rn[i], Wind= S$Met_c$WindSpeed[i], Tair = S$Met_c$Tair[i],
                       ZHT = S$Parameters$ZHT,TREEH = S$Table_Day$H_Tree_m[i], Pressure = S$Met_c$Pressure[i],
                       Gs = 1E09, VPD = S$Met_c$VPD[i])

            if(S$Table_Day$CanopyHumect.mm[i]<=S$Table_Day$IntercMax.mm[i]){
                S$Table_Day$ThSt_mmd[i]= 0
                S$Table_Day$IntercRevapor_mmd[i]= min(S$Table_Day$CanopyHumect.mm[i], Potential_LeafEvap)
                S$Table_Day$CanopyHumect.mm[i]= max(0,S$Table_Day$CanopyHumect.mm[i]-S$Table_Day$IntercRevapor_mmd[i])
            }else{
                S$Table_Day$ThSt_mmd[i]=S$Table_Day$CanopyHumect.mm[i]-S$Table_Day$IntercMax.mm[i]
                S$Table_Day$IntercRevapor_mmd[i]=min(S$Table_Day$IntercMax.mm[i],Potential_LeafEvap)
                S$Table_Day$CanopyHumect.mm[i]=max(0,S$Table_Day$IntercMax.mm[i]-S$Table_Day$IntercRevapor_mmd[i])
            }

            # 2/ SURFACE RUNOFF / INFILTRATION source Gomez-Delgado et al. 2011,
            # Box B:WSurfResMax_mm = BX; WSurfaceRes_mm=Bt;
            #ExcessRunoff_mmd=QB2; SuperficialRunoff1_mmd=QB1;  TotSuperficialRunoff_mmd=QB; Infiltration_mmd=i
            # 2.a Adding throughfall to superficial-box, calculation of surface runoff, updating of
            # stock in superficial-box (WSurfaceReserv_mm)
            #             S$Table_Day$WSurfaceRes_mm[i]=
            #                 S$Table_Day$WSurfaceRes_mm[i-S$Zero_then_One[i]] + S$Zero_then_One[i]*S$Table_Day$ThSt_mmd[i]
            # RV: Same as CanopyHumect.mm
            S$Table_Day$WSurfaceRes_mm[i]=
                S$Table_Day$WSurfaceRes_mm[i-S$Zero_then_One[i]] + S$Table_Day$ThSt_mmd[i]

            if(S$Table_Day$WSurfaceRes_mm[i] > S$Parameters$WSurfResMax_mm){
                S$Table_Day$ExcessRunoff_mmd[i] = S$Table_Day$WSurfaceRes_mm[i]-S$Parameters$WSurfResMax_mm
                S$Table_Day$WSurfaceRes_mm[i]= S$Parameters$WSurfResMax_mm # removing ExcessRunoff_mmd
                S$Table_Day$SuperficialRunoff1_mmd[i] = S$Parameters$kB * S$Table_Day$WSurfaceRes_mm[i]
                #Subsuperficial runoff from runoffbox
                S$Table_Day$TotSuperficialRunoff_mmd[i] =
                    S$Table_Day$ExcessRunoff_mmd[i] + S$Table_Day$SuperficialRunoff1_mmd[i]
                S$Table_Day$WSurfaceRes_mm[i] =
                    S$Table_Day$WSurfaceRes_mm[i] - S$Table_Day$SuperficialRunoff1_mmd[i]
            }else{
                #updating WSurfaceRes_mm, the ExcessRunoff_mmd has already been retrieved
                S$Table_Day$ExcessRunoff_mmd[i]=0
                S$Table_Day$SuperficialRunoff1_mmd[i] = S$Parameters$kB * S$Table_Day$WSurfaceRes_mm[i]
                S$Table_Day$TotSuperficialRunoff_mmd[i] = S$Table_Day$SuperficialRunoff1_mmd[i]
                S$Table_Day$WSurfaceRes_mm[i] = S$Table_Day$WSurfaceRes_mm[i] -
                    S$Table_Day$SuperficialRunoff1_mmd[i]}

            # 2.b Computing the infiltration capacity as a function of soil water content in W1
            S$Table_Day$W1.mm[i]= S$Table_Day$W1.mm[i-S$Zero_then_One[i]]

            if(S$Table_Day$W1.mm[i] <= S$Parameters$Wm1){
                S$Table_Day$InfilCapa_mmd[i]= S$Parameters$fo # InfilCapa_mmd: infiltration capacity
            }else{
                if(S$Table_Day$W1.mm[i]<= S$Parameters$Wf1){
                    S$Table_Day$InfilCapa_mmd[i]= S$Parameters$fo-(S$Table_Day$W1.mm[i]-S$Parameters$Wm1)*
                        (S$Parameters$fo - S$Parameters$fc) / (S$Parameters$Wf1 - S$Parameters$Wm1)
                }else{
                    S$Table_Day$InfilCapa_mmd[i]=S$Parameters$fc
                }
            }

            # 2.c Calculating infiltration from superficial-box to soil-boxes and updating stock in superficial-box
            if(S$Table_Day$InfilCapa_mmd[i]<= S$Table_Day$WSurfaceRes_mm[i]){
                S$Table_Day$Infiltration_mmd[i]= S$Table_Day$InfilCapa_mmd[i]   # infiltration (m?dt-1)
                S$Table_Day$WSurfaceRes_mm[i]=
                    S$Table_Day$WSurfaceRes_mm[i] - S$Table_Day$Infiltration_mmd[i]
            }else{
                S$Table_Day$Infiltration_mmd[i]= S$Table_Day$WSurfaceRes_mm[i]
                S$Table_Day$WSurfaceRes_mm[i]= 0
            }

            #3/ Adding Infiltration_mmd to soil water content of the previous day, computing drainage,
            # source Gomez-Delgado et al. 2010
            # RV: same as CanopyHumect.mm
            # S$Table_Day$W1.mm[i]= S$Table_Day$W1.mm[i-S$Zero_then_One[i]]+S$Zero_then_One[i]*S$Table_Day$Infiltration_mmd[i]
            S$Table_Day$W1.mm[i]= S$Table_Day$W1.mm[i-S$Zero_then_One[i]]+S$Table_Day$Infiltration_mmd[i]

            #Preventing W1.mm to be larger than the soil storage at field capacity:
            if(S$Table_Day$W1.mm[i] > S$Parameters$Wf1){
                S$Table_Day$Drain1.mm[i]= S$Table_Day$W1.mm[i] - S$Parameters$Wf1
                S$Table_Day$W1.mm[i] = S$Parameters$Wf1
            }else{S$Table_Day$Drain1.mm[i]= 0}     # Water excess in the root-box that drains (m)

            # RV: same as CanopyHumect.mm
            # S$Table_Day$W2.mm[i]= S$Table_Day$W2.mm[i-S$Zero_then_One[i]]+S$Zero_then_One[i]*S$Table_Day$Drain1.mm[i]
            S$Table_Day$W2.mm[i]= S$Table_Day$W2.mm[i-S$Zero_then_One[i]]+S$Table_Day$Drain1.mm[i]

            #Preventing W2.mm to be larger than the soil storage at field capacity:
            if(S$Table_Day$W2.mm[i] > S$Parameters$Wf2){
                S$Table_Day$Drain2.mm[i]= S$Table_Day$W2.mm[i] - S$Parameters$Wf2
                S$Table_Day$W2.mm[i] = S$Parameters$Wf2
            }else{S$Table_Day$Drain2.mm[i]= 0}     # Water excess in the root-box that drains (m)

            # RV: same as CanopyHumect.mm
            # S$Table_Day$W3.mm[i]= S$Table_Day$W3.mm[i-S$Zero_then_One[i]]+S$Zero_then_One[i]*S$Table_Day$Drain2.mm[i]
            S$Table_Day$W3.mm[i]= S$Table_Day$W3.mm[i-S$Zero_then_One[i]]+S$Table_Day$Drain2.mm[i]

            #Preventing W3.mm to be larger than the soil storage at field capacity:
            if(S$Table_Day$W3.mm[i] > S$Parameters$Wf3){
                S$Table_Day$Drain3.mm[i]= S$Table_Day$W3.mm[i] - S$Parameters$Wf3
                S$Table_Day$W3.mm[i] = S$Parameters$Wf3
            }else{S$Table_Day$Drain3.mm[i]= 0}     # Water excess in the root-box that drains (m)

            #3/First computing water per soil layer
            S$Table_Day$EW1.mm[i]= S$Table_Day$W1.mm[i]-S$Parameters$Wm1 # Extractable water (m)
            # Relative extractable water (dimensionless):
            S$Table_Day$REW1.mm[i]= S$Table_Day$EW1.mm[i]/(S$Parameters$Wf1-S$Parameters$Wm1)
            S$Table_Day$EW2.mm[i]= S$Table_Day$W2.mm[i]-S$Parameters$Wm2
            S$Table_Day$REW2.mm[i]= S$Table_Day$EW2.mm[i]/(S$Parameters$Wf2-S$Parameters$Wm2)
            S$Table_Day$EW3.mm[i]= S$Table_Day$W3.mm[i]-S$Parameters$Wm3
            S$Table_Day$REW3.mm[i]= S$Table_Day$EW3.mm[i]/(S$Parameters$Wf3-S$Parameters$Wm3)
            S$Table_Day$EWtot.mm[i]= S$Table_Day$EW1.mm[i]+S$Table_Day$EW2.mm[i]+S$Table_Day$EW3.mm[i]
            S$Table_Day$REWtot.mm[i]= S$Table_Day$EWtot.mm[i]/
                ((S$Parameters$Wf1-S$Parameters$Wm1)+(S$Parameters$Wf2-S$Parameters$Wm2)+
                     (S$Parameters$Wf3-S$Parameters$Wm3))

            #4/Evaporation of the Understorey, Eu (from W1 only)
            #Source "Modele evaporation soil.xls" with eucalyptus data, following the curve adjustement
            # proposed in Jarvis 1976, eq.4
            # S$Table_Day$Coeff.partitionHLE.understorey[i]=
            #     min(1,max(0,S$Parameters$PartHLEu.b1*S$Parameters$PartHLEu.b2*
            #                   (S$Table_Day$REW1.mm[i]-S$Parameters$PartHLEu.q)/
            #                   (S$Parameters$PartHLEu.b1+S$Parameters$PartHLEu.b2*
            #                        (S$Table_Day$REW1.mm[i]-S$Parameters$PartHLEu.q))))
            # S$Table_Day$Eu_mmd[i]= S$Table_Day$AEu_MJm2d[i]*
            #     S$Table_Day$Coeff.partitionHLE.understorey[i]/S$Parameters$lambda
            S$Table_Day$Eu_mmd[i]= S$Table_Day$AEu_MJm2d[i]*S$Parameters$Soil_H_LE_partitioning/S$Parameters$lambda
            # RV: computed by MAESPA

            #Avoiding depleting W1 below Wm1 and udating Wx after retrieving actual Eu
            if((S$Table_Day$W1.mm[i]-S$Table_Day$Eu_mmd[i])>=S$Parameters$Wm1){
                S$Table_Day$W1.mm[i]= S$Table_Day$W1.mm[i]-S$Table_Day$Eu_mmd[i]
            }else{S$Table_Day$Eu_mmd[i]= S$Table_Day$W1.mm[i]-S$Parameters$Wm1
            S$Table_Day$W1.mm[i]= S$Parameters$Wm1}

            #5/Transpiration T_mmd, source Granier et al., 1999
            #Transpiration T_Plot_ per layer, source Metamodels from MAESPA:
            # NB OR, I divided the values from metamodel

            # Metamodel Transpiration Coffee, and filter out for negative values
            S$Table_Day$T_Cof_mmd[i]=
                -0.42164 + 0.03467*S$Met_c$VPD[i] + 0.10559*S$Table_Day$LAI[i] +
                0.11510*PARcof
            S$Table_Day$T_Cof_mmd[i][S$Table_Day$T_Cof_mmd[i]<0]= 0
            #Plot transpiration
            S$Table_Day$T_Plot_mmd[i]= S$Table_Day$T_Tree_mmd[i]+S$Table_Day$T_Cof_mmd[i]

            #6/ Root Water Extraction by soil layer, source Granier et al., 1999
            S$Table_Day$RootWaterExtract1_mmd[i]= S$Table_Day$T_Plot_mmd[i]*S$Parameters$RootFraction1
            S$Table_Day$RootWaterExtract2_mmd[i]= S$Table_Day$T_Plot_mmd[i]*S$Parameters$RootFraction2
            S$Table_Day$RootWaterExtract3_mmd[i]= S$Table_Day$T_Plot_mmd[i]*S$Parameters$RootFraction3
            #Avoiding depleting Wx below Wmx, and udating Wx after retrieving actual RootWaterExtract
            if((S$Table_Day$W1.mm[i]-S$Table_Day$RootWaterExtract1_mmd[i])>=S$Parameters$Wm1){
                S$Table_Day$W1.mm[i]= S$Table_Day$W1.mm[i]-S$Table_Day$RootWaterExtract1_mmd[i]
            }else{S$Table_Day$RootWaterExtract1_mmd[i]=S$Table_Day$W1.mm[i]-S$Parameters$Wm1
            S$Table_Day$W1.mm[i]=S$Parameters$Wm1}

            if((S$Table_Day$W2.mm[i]-S$Table_Day$RootWaterExtract2_mmd[i])>=S$Parameters$Wm2){
                S$Table_Day$W2.mm[i]=S$Table_Day$W2.mm[i]-S$Table_Day$RootWaterExtract2_mmd[i]
            }else{S$Table_Day$RootWaterExtract2_mmd[i]=S$Table_Day$W2.mm[i]-S$Parameters$Wm2
            S$Table_Day$W2.mm[i]=S$Parameters$Wm2}

            if((S$Table_Day$W3.mm[i]-S$Table_Day$RootWaterExtract3_mmd[i])>=S$Parameters$Wm3){
                S$Table_Day$W3.mm[i]=S$Table_Day$W3.mm[i]-S$Table_Day$RootWaterExtract3_mmd[i]
            }else{S$Table_Day$RootWaterExtract3_mmd[i]=S$Table_Day$W3.mm[i]-S$Parameters$Wm3
            S$Table_Day$W3.mm[i]=S$Parameters$Wm3}


            #7/ Evapo-Transpiration ETR_Plot_mmd
            S$Table_Day$ETR_Plot_mmd[i]=
                S$Table_Day$T_Plot_mmd[i]+S$Table_Day$Eu_mmd[i]+S$Table_Day$IntercRevapor_mmd[i]

            #8/Second Updating water per soil layer
            S$Table_Day$Wtot.mm[i]= S$Table_Day$W1.mm[i]+S$Table_Day$W2.mm[i]+S$Table_Day$W3.mm[i]
            S$Table_Day$EW1.mm[i]= S$Table_Day$W1.mm[i]-S$Parameters$Wm1 # Extractable water (m)
            S$Table_Day$REW1.mm[i]= S$Table_Day$EW1.mm[i]/(S$Parameters$Wf1-S$Parameters$Wm1)
            # Relative extractable water (dimensionless)
            S$Table_Day$EW2.mm[i]= S$Table_Day$W2.mm[i]-S$Parameters$Wm2
            S$Table_Day$REW2.mm[i]= S$Table_Day$EW2.mm[i]/(S$Parameters$Wf2-S$Parameters$Wm2)
            S$Table_Day$EW3.mm[i]= S$Table_Day$W3.mm[i]-S$Parameters$Wm3
            S$Table_Day$REW3.mm[i]= S$Table_Day$EW3.mm[i]/(S$Parameters$Wf3-S$Parameters$Wm3)
            S$Table_Day$EWtot.mm[i]= S$Table_Day$EW1.mm[i]+S$Table_Day$EW2.mm[i]+S$Table_Day$EW3.mm[i]
            S$Table_Day$REWtot.mm[i]= S$Table_Day$EWtot.mm[i]/
                ((S$Parameters$Wf1-S$Parameters$Wm1)+(S$Parameters$Wf2-S$Parameters$Wm2)+
                     (S$Parameters$Wf3-S$Parameters$Wm3))

            #9/ Soil water deficit
            if(S$Parameters$REWc*S$Parameters$EWMtot-S$Table_Day$EWtot.mm[i]>0){
                S$Table_Day$SWDtot.mm[i]= S$Parameters$REWc*S$Parameters$EWMtot-S$Table_Day$EWtot.mm[i]
            }else{S$Table_Day$SWDtot.mm[i]= 0}

            #10/ Latent (LEmod) and Sensible (Hmod) heat fluxes, In kgH2O m-2 d-1 * MJ kgH2O-1 = MJ m-2 d-1
            S$Table_Day$LE_Plot_MJm2d[i]= S$Table_Day$ETR_Plot_mmd[i]*S$Parameters$lambda#kgH2O m-2 d-1 * MJ kgH2O-1
            # S$Table_Day$G_Plot_MJm2d[i]= S$Table_Day$Rnu_MJm2d[i]*(S$Parameters$fG)
            #NB, H is the result from radiative and energy balance, where LE depends on metamodels.
            # If Rn is too low as compared to LE, H wold be negative
            # S$Table_Day$H_Plot_MJm2d[i]= S$Met_c$Rn[i]-S$Table_Day$LE_Plot_MJm2d[i]-
            # S$Table_Day$G_Plot_MJm2d[i]
            S$Table_Day$LE_Coffee_MJm2d[i]=
                (S$Table_Day$T_Cof_mmd[i]+S$Table_Day$IntercRevapor_mmd[i]*
                     (S$Table_Day$LAI[i]/S$Table_Day$LAIplot[i]))*S$Parameters$lambda
            # S$Table_Day$H_Coffee_MJm2d[i]= S$Table_Day$Rncoffee_MJm2d[i]-S$Table_Day$LE_Coffee_MJm2d[i]

            # Metamodel for H :
            S$Table_Day$H_Coffee_MJm2d[i]=
                -1.80160 + 0.03139*S$Met_c$Tair[i] - 0.06046*S$Met_c$VPD[i]+
                1.93064*(1-S$Met_c$FDiff[i]) + 0.58368*PARcof+
                0.25838*S$Table_Day$LAI[i]


            S$Table_Day$Rncoffee_MJm2d[i]=
                S$Table_Day$H_Coffee_MJm2d[i] + S$Table_Day$LE_Coffee_MJm2d[i]

            S$Table_Day$LESoil_MJm2d[i]= S$Table_Day$Eu_mmd[i]*S$Parameters$lambda
            # S$Table_Day$Hu_MJm2d[i]=S$Table_Day$Rnu_MJm2d[i]-S$Table_Day$LEu_MJm2d[i]-S$Table_Day$G_Plot_MJm2d[i]
            # S$Table_Day$HSoil_MJm2d[i]= 3.27709+0.52850*S$Table_Day$PAR_Soil[i]-0.19023*
            #     S$Met_c$Tair[i]-0.03759*S$Met_c$VPD[i] -0.76927*(1-S$Met_c$FDiff[i])-
            #     0.02327*S$Table_Day$CrownH_Tree_m[i]
            S$Table_Day$HSoil_MJm2d[i]= S$Table_Day$AEu_MJm2d[i]*(1-S$Parameters$Soil_H_LE_partitioning)
            # RV: H soil is now computed as Rn soil - LE soil.
            # soil heat transport (QSoil):
            # S$Table_Day$QSoil_MJm2d[i]=
            #     -6.56909+0.29890*S$Table_Day$PAR_Soil[i]+0.27408*
            #     S$Met_c$Tair[i]+0.06291*S$Met_c$VPD[i]+
            #     (6.54980*(S$Table_Day$Wtot.mm[i]/S$Parameters$TotalDepth/1000))
            S$Table_Day$QSoil_MJm2d[i]= 0
            # RV: QSoil_MJm2d is negligible at yearly time-step, and equilibriate between several
            # days anyway.
            S$Table_Day$RnSoil_MJm2d[i]=
                S$Table_Day$HSoil_MJm2d[i] + S$Table_Day$LESoil_MJm2d[i] + S$Table_Day$QSoil_MJm2d[i]


            # Tree LE and Rn (can not compute them in the Tree function because we need IntercRevapor_mmd)

            S$Table_Day$LE_Tree_MJm2d[i]=
                (S$Table_Day$T_Tree_mmd[i]+S$Table_Day$IntercRevapor_mmd[i]*
                     (S$Table_Day$LAI_Tree[i]/S$Table_Day$LAIplot[i]))*S$Parameters$lambda
            S$Table_Day$RnTree_MJm2d[i]= S$Table_Day$H_Tree_MJm2d[i] + S$Table_Day$LE_Tree_MJm2d[i]

            # Total plot heat flux:
            S$Table_Day$H_tot_MJm2d[i]= S$Table_Day$H_Coffee_MJm2d[i]+S$Table_Day$H_Tree_MJm2d[i]+
                S$Table_Day$HSoil_MJm2d[i]
            # Total plot latent flux:
            S$Table_Day$LE_tot_MJm2d[i]=
                S$Table_Day$LE_Coffee_MJm2d[i]+S$Table_Day$LE_Tree_MJm2d[i]+
                S$Table_Day$LESoil_MJm2d[i]

            # Total plot net radiation:
            S$Table_Day$Rn_tot_MJm2d[i]= S$Table_Day$Rncoffee_MJm2d[i]+S$Table_Day$RnTree_MJm2d[i]+
                S$Table_Day$RnSoil_MJm2d[i]


            #11/ Tcanopy Coffee
            S$Table_Day$Ga[i]=
                bigleaf::aerodynamic.conductance(Tair = S$Met_c$Tair[i],
                                                 pressure = S$Met_c$Pressure[i]/10,
                                                 wind = S$Met_c$WindSpeed[i],
                                                 ustar = ustar[i],
                                                 H = S$Met_c$Tair[i],
                                                 zr = S$Parameters$MeasHeight,
                                                 zh = S$Parameters$MeasHeight,
                                                 d = S$Parameters$DisplacementHeight.coffee,
                                                 z0m = S$Parameters$z0.coffee,
                                                 wind_profile = T,
                                                 stab_correction = F,
                                                 Rb_model="Thom_1972")[,"Ga_h"] # m s-1
            # stab_correction could be activated whenever H is well simulated

            S$Table_Day$Diff_T[i]=
                (S$Table_Day$H_Coffee_MJm2d[i]*Parameters$MJ_to_W)/
                (S$Met_c$rho[i]*S$Parameters$Cp*S$Table_Day$Ga[i])

            S$Table_Day$Tcan_mod_Coffee_degC[i]= S$Met_c$Tair[i]+S$Table_Day$Diff_T[i]

        }
        list(Table_Day= S$Table_Day, PerCohortFruitDemand_c= S$PerCohortFruitDemand_c)
        }
    stopCluster(cl)
    # Reordering the lists to make the results as previous versions of the model:
    Table= data.frame(do.call(rbind, CycleList[c(1:NCycles)]))
    PerCohortFruitDemand= data.frame(do.call(cbind, CycleList[c((NCycles+1):NCycles*2)]))
    PerCohortFruitDemand=PerCohortFruitDemand[rowSums(PerCohortFruitDemand, na.rm = T)>0,]
    cat("\n", "Simulation completed successfully", "\n")
    FinalList= list(Table_Day= Table, PerCohortFruitDemand= PerCohortFruitDemand,
                    Met_c= Meteo, Location= S$Parameters$Location,
                    Tree_Species= S$Parameters$Tree_Species,
                    Parameters= Parameters)
    if(WriteIt){
        write.results(FinalList,output,...)
    }
    if(returnIt){
        return(FinalList)
    }
}
