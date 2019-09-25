#' Coffee model
#'
#' @description Computes the different components of the coffee growth and yield.
#'
#' @param S The simulation list
#' @param i The day index.
#'
#' @return Nothing, modify the list of simulation `S` in place. See [DynACof()] for more details.
#'
coffee_model= function(S,i){
  .= NULL
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

  # Metamodel Coffee leaf water potential
  S$Sim$LeafWaterPotential[i]=
    S$Sim$SoilWaterPot[previous_i(i,1)] -
    (S$Sim$T_Coffee[i] / S$Parameters$M_H20) / S$Parameters$KTOT

  # Energy balance ----------------------------------------------------------

  # Transpiration Coffee
  S$Sim$T_Coffee[i]= S$Parameters$T_Coffee(S,i)

  S$Sim$H_Coffee[i]= S$Parameters$H_Coffee(S,i)



  # Tcanopy Coffee : using bulk conductance if no trees, interlayer conductance if trees
  # Source: Van de Griend and Van Boxel 1989.
  if(S$Sim$Height_Tree[i]>S$Parameters$Height_Coffee){

    S$Sim$TairCanopy[i]=
      S$Sim$TairCanopy_Tree[i]+((S$Sim$H_Coffee[i]+S$Sim$H_Soil[i])*S$Parameters$MJ_to_W)/
      (bigleaf::air.density(S$Sim$TairCanopy_Tree[i],S$Met_c$Pressure[i]/10)*
         S$Parameters$Cp*
         G_interlay(Wind= S$Met_c$WindSpeed[i], ZHT = S$Parameters$ZHT,
                    LAI_top= S$Sim$LAI_Tree[i],
                    LAI_bot= S$Sim$LAI[i],
                    Z_top= S$Sim$Height_Tree[i],
                    extwind = S$Parameters$extwind))

    S$Sim$Tleaf_Coffee[i]=
      S$Sim$TairCanopy[i]+(S$Sim$H_Coffee[i]*S$Parameters$MJ_to_W)/
      (bigleaf::air.density(S$Sim$TairCanopy[i],S$Met_c$Pressure[i]/10)*
         S$Parameters$Cp*
         Gb_h(Wind = S$Met_c$WindSpeed[i], wleaf= S$Parameters$wleaf,
              LAI_lay=S$Sim$LAI[i],
              LAI_abv=S$Sim$LAI_Tree[i],
              ZHT = S$Parameters$ZHT,
              Z_top = S$Sim$Height_Tree[i],
              extwind= S$Parameters$extwind))

  }else{

    S$Sim$TairCanopy[i]=
      S$Met_c$Tair[i]+((S$Sim$H_Coffee[i]+S$Sim$H_Soil[i])*S$Parameters$MJ_to_W)/
      (bigleaf::air.density(S$Sim$TairCanopy_Tree[i],S$Met_c$Pressure[i]/10)*
         S$Parameters$Cp*
         G_bulk(Wind = S$Met_c$WindSpeed[i], ZHT = S$Parameters$ZHT,
                Z_top = S$Parameters$Height_Coffee,
                LAI = S$Sim$LAI[i],
                extwind = S$Parameters$extwind))

    S$Sim$Tleaf_Coffee[i]=
      S$Sim$TairCanopy[i]+(S$Sim$H_Coffee[i]*S$Parameters$MJ_to_W)/
      (bigleaf::air.density(S$Sim$TairCanopy[i],S$Met_c$Pressure[i]/10)*
         S$Parameters$Cp *
         Gb_h(Wind= S$Met_c$WindSpeed[i], wleaf= S$Parameters$wleaf,
              LAI_lay= S$Sim$LAI[i],
              LAI_abv= S$Sim$LAI_Tree[i],
              ZHT= S$Parameters$ZHT,
              Z_top= S$Parameters$Height_Coffee,
              extwind= S$Parameters$extwind))
  }
  # NB: if no trees, TairCanopy_Tree= Tair

  # Recomputing soil temperature knowing TairCanopy

  S$Sim$TSoil[i]=
    S$Sim$TairCanopy[i]+(S$Sim$H_Soil[i]*S$Parameters$MJ_to_W)/
    (bigleaf::air.density(S$Sim$TairCanopy[i],S$Met_c$Pressure[i]/10)*
       S$Parameters$Cp*
       G_soilcan(Wind= S$Met_c$WindSpeed[i], ZHT=S$Parameters$ZHT,
                 Z_top= max(S$Sim$Height_Tree[i],
                            S$Parameters$Height_Coffee, na.rm = T),
                 LAI = S$Sim$LAI_Tree[i] + S$Sim$LAI[i],
                 extwind= S$Parameters$extwind))

  S$Sim$DegreeDays_Tcan[i]=
    GDD(Tmean = S$Sim$TairCanopy[i],MinTT = S$Parameters$MinTT,
        MaxTT = S$Parameters$MaxTT)

  # Metamodel LUE coffee:
  S$Sim$lue[i]= S$Parameters$lue(S,i)

  #GPP Coffee
  S$Sim$GPP[i]= S$Sim$lue[i]*S$Sim$APAR[i]

  # Maintenance respiration -------------------------------------------------

  # Rm is computed at the beginning of the day on the drymass of the previous day.
  # This is considered as the highest priority for the plant (to maintain its dry mass)

  # Resprout (branches) wood:
  S$Sim$Rm_Shoot[i]=
    after(i,2)*
    (S$Parameters$pa_Shoot*S$Sim$DM_Shoot[previous_i(i,1)]*
       S$Parameters$NC_Shoot*S$Parameters$MRN*
       S$Parameters$Q10_Shoot^((S$Sim$TairCanopy[i]-S$Parameters$TMR)/10))

  # Stump and Coarse roots (perennial wood):
  S$Sim$Rm_SCR[i]=
    after(i,2)*
    (S$Parameters$pa_SCR*
       S$Sim$DM_SCR[previous_i(i,1)]*
       S$Parameters$NC_SCR*S$Parameters$MRN*
       S$Parameters$Q10_SCR^(
         (S$Sim$TairCanopy[i]-S$Parameters$TMR)/10))

  # Fruits:
  S$Sim$Rm_Fruit[i]=
    after(i,2)*
    (S$Parameters$pa_Fruit*S$Sim$DM_Fruit[previous_i(i,1)]*
       S$Parameters$NC_Fruit*S$Parameters$MRN*
       S$Parameters$Q10_Fruit^((S$Sim$TairCanopy[i]-S$Parameters$TMR)/10))

  # Leaves:
  S$Sim$Rm_Leaf[i]=
    after(i,2)*
    (S$Parameters$pa_Leaf*S$Sim$DM_Leaf[previous_i(i,1)]*
       S$Parameters$NC_Leaf*S$Parameters$MRN*
       S$Parameters$Q10_Leaf^((S$Sim$TairCanopy[i]-S$Parameters$TMR)/10))

  # Fine roots:
  S$Sim$Rm_FRoot[i]=
    after(i,2)*
    (S$Parameters$pa_FRoot*S$Sim$DM_FRoot[previous_i(i,1)]*
       S$Parameters$NC_FRoot*S$Parameters$MRN*
       S$Parameters$Q10_FRoot^((S$Sim$TairCanopy[i]-S$Parameters$TMR)/10))

  # Total plant maintenance respiration
  S$Sim$Rm[i]=
    S$Sim$Rm_Fruit[i]+S$Sim$Rm_Leaf[i]+
    S$Sim$Rm_Shoot[i]+S$Sim$Rm_SCR[i]+
    S$Sim$Rm_FRoot[i]



  # Coffee Allocation -------------------------------------------------------

  # Potential use of reserves:
  S$Sim$Consumption_RE[i]=
    S$Parameters$kres*S$Sim$CM_RE[previous_i(i,1)]

  # Supply function
  S$Sim$Supply[i]=
    max(S$Sim$GPP[i]-S$Sim$Rm[i]+S$Sim$Consumption_RE[i],0)

  # If the respiration is greater than the GPP + reserves use, then take this carbon
  # from mortality of each compartments' biomass equally (not for fruits or reserves):
  S$Sim$Carbon_Lack_Mortality[i]=
    -min(0,S$Sim$GPP[i]-S$Sim$Rm[i]+S$Sim$Consumption_RE[i])

  # 1-Resprout wood ---------------------------------------------------------
  # Allocation priority 1, see Charbonnier 2012.
  S$Sim$Alloc_Shoot[i]= S$Parameters$lambda_Shoot*S$Sim$Supply[i]
  S$Sim$NPP_Shoot[i]= S$Sim$Alloc_Shoot[i]/S$Parameters$epsilon_Shoot
  S$Sim$Rg_Shoot[i]= S$Sim$Alloc_Shoot[i]-S$Sim$NPP_Shoot[i]
  S$Sim$Mnat_Shoot[i]=
    S$Sim$CM_Shoot[previous_i(i,1)]/S$Parameters$lifespan_Shoot
  # Pruning
  if(S$Sim$Plot_Age[i]>=S$Parameters$MeanAgePruning&
     S$Met_c$DOY[i]==S$Parameters$D_pruning){
    S$Sim$Mprun_Shoot[i]=
      S$Sim$CM_Shoot[previous_i(i,1)]*S$Parameters$WoodPruningRate
  }
  S$Sim$Mortality_Shoot[i]=
    min((S$Sim$Mnat_Shoot[i]+S$Sim$Mprun_Shoot[i]),
        S$Sim$CM_Shoot[previous_i(i,1)])

  # 2-Stump and coarse roots (perennial wood) ------------------------------
  S$Sim$Alloc_SCR[i]= S$Parameters$lambda_SCR*S$Sim$Supply[i]
  S$Sim$NPP_SCR[i]= S$Sim$Alloc_SCR[i]/S$Parameters$epsilon_SCR
  S$Sim$Rg_SCR[i]= S$Sim$Alloc_SCR[i]-S$Sim$NPP_SCR[i]
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
  if(S$Met_c$DOY[i]==S$Parameters$DVG2){
    S$Sim$ratioNodestoLAI[S$Met_c$year>=S$Met_c$year[i]]=
      mean(S$Sim$TairCanopy[S$Met_c$year==S$Met_c$year[i]&
                              S$Met_c$DOY>=S$Parameters$DVG1&
                              S$Met_c$DOY <= S$Parameters$DVG2])%>%
      {S$Parameters$RNL_base*CN(.)}
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
  dd_i= round(cumsum(S$Sim$DegreeDays_Tcan[i:previous_i(i,1000)]),2)

  # (3) Find the window where buds are under dormancy (find the dormant cohorts)
  # Bud develops during F_buds1 (840) degree days after initiation, so they cannot
  # be dormant less than F_buds1 before i. But they can stay under dormancy until
  # F_buds2 dd maximum, so they cannot be older than F_buds2 dd before i.
  OldestDormancy= i - (max(which(dd_i<S$Parameters$F_buds2))-1)
  YoungestDormancy= i - (max(which(dd_i<S$Parameters$F_buds1))-1)
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
  S$Sim$pbreak[i]= 1/(1+exp(S$Parameters$a_p+S$Parameters$b_p*S$Sim$LeafWaterPotential[i]))
  # (8) Compute the number of buds that effectively break dormancy in each cohort:
  S$Sim$BudBreak_cohort[DormancyBreakPeriod]=
    pmin(S$Sim$Bud_available[DormancyBreakPeriod],
         S$Sim$Budinit[DormancyBreakPeriod]*S$Sim$pbreak[i]*
           S$Sim$Temp_cor_Bud[DormancyBreakPeriod])
  # NB 1: cannot exceed the number of buds of each cohort
  # NB 2: using Budinit and not Bud_available because pbreak is fitted on
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
  FruitingPeriod= i-which(dd_i<(S$Parameters$F_over))+1
  # NB : Fruits that are older than the FruitingPeriod are overripped

  # Demand from each fruits cohort present on the coffee tree (not overriped),
  # same as Demand_Fruit but keeping each value :
  Demand_Fruit_Cohort_Period=
    S$Sim$BudBreak[FruitingPeriod]*S$Parameters$DE_opt*
    logistic_deriv(dd_i[1:length(FruitingPeriod)],
                   S$Parameters$u_log,S$Parameters$s_log)
  Demand_Fruit_Cohort_Period[is.na(Demand_Fruit_Cohort_Period)]= 0
  # Total C demand of the fruits :
  S$Sim$Demand_Fruit[i]= sum(Demand_Fruit_Cohort_Period)
  # C supply to Fruits (i.e. what is left from Supply after removing the consumption
  # by previous compartments and Rm):
  S$Sim$Supply_Fruit[i]=
    S$Sim$Supply[i]-S$Sim$Alloc_Shoot[i]-
    S$Sim$Alloc_SCR[i]

  # Total C allocation to all fruits on day i :
  S$Sim$Alloc_Fruit[i]= min(S$Sim$Demand_Fruit[i],S$Sim$Supply_Fruit[i])
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
  # Using CM_Fruit_Cohort_remain to keep track of the fruit mass that is created, but it is updated by removing the overriped
  # fruits then, so the overriped fruits can only be removed once.
  S$Sim$CM_Fruit_Cohort_remain[FruitingPeriod]=
    S$Sim$CM_Fruit_Cohort_remain[FruitingPeriod] + S$Sim$NPP_Fruit_Cohort[FruitingPeriod]
  # Overriped fruits that fall onto the ground (= to mass of the cohort that overripe) :
  overriped_day= max(min(FruitingPeriod)-1,1)
  S$Sim$Overriped_Fruit[i]= sum(S$Sim$CM_Fruit_Cohort_remain[previous_i(overriped_day,0:10)])
  S$Sim$CM_Fruit_Cohort_remain[previous_i(overriped_day,0:10)]= 0.0

  # Duration of the maturation of each cohort born in the ith day (in days):
  S$Sim$Maturation_duration[FruitingPeriod]=
    seq_along(FruitingPeriod)
  # Sucrose content of each cohort:
  S$Sim$SC[FruitingPeriod]=
    Sucrose_cont_perc(S$Sim$Maturation_duration[FruitingPeriod],
                      a= S$Parameters$S_a, b= S$Parameters$S_b,
                      x0= S$Parameters$S_x0, y0=S$Parameters$S_y0)
  # Sucrose mass of each cohort
  S$Sim$SM[FruitingPeriod]=
    S$Sim$DM_Fruit_Cohort[FruitingPeriod]*S$Sim$SC[FruitingPeriod]
  # Harvest maturity:
  S$Sim$Harvest_Maturity_Pot[i]=
    round(sum(S$Sim$SM[FruitingPeriod])/
            sum(S$Sim$DM_Fruit_Cohort[FruitingPeriod]*
                  ((S$Parameters$S_y0 + S$Parameters$S_a)/100)),3)
  S$Sim$Harvest_Maturity_Pot[i][is.nan(S$Sim$Harvest_Maturity_Pot[i])]= 0

  # NB : here harvest maturity is computed as the average maturity of the cohorts, because
  # all cohorts present in the Coffea are within the 'FruitingPeriod' window.
  # It could be computed as the percentage of cohorts that are fully mature (Pezzopane
  # et al. 2012 say at 221 days after flowering)
  # Optimal sucrose concentration around 8.8% of the dry mass

  S$Sim$NPP_Fruit[i]= S$Sim$Alloc_Fruit[i]/S$Parameters$epsilon_Fruit
  S$Sim$Rg_Fruit[i]= S$Sim$Alloc_Fruit[i]-S$Sim$NPP_Fruit[i]

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
  }else if(S$Parameters$harvest=="quality"){
    is_harvest=
      S$Sim$Plot_Age[i]>=S$Parameters$ageMaturity &
      mean(S$Sim$Harvest_Maturity_Pot[previous_i(i,0:9)])<
      mean(S$Sim$Harvest_Maturity_Pot[previous_i(i,10:19)])
    # Made as soon as the overall fruit maturation is optimal (all fruits are mature)
  }else{
    is_harvest= FALSE
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

  S$Sim$Supply_Leaf[i]=
    S$Parameters$lambda_Leaf_remain*
    (S$Sim$Supply[i]-S$Sim$Alloc_Fruit[i]-
       S$Sim$Alloc_Shoot[i]-S$Sim$Alloc_SCR[i])

  S$Sim$Alloc_Leaf[i]=
    min(S$Parameters$DELM*(S$Parameters$Stocking_Coffee/10000)*
          ((S$Parameters$LAI_max-S$Sim$LAI[i])/
             (S$Sim$LAI[i]+S$Parameters$LAI_max)),
        S$Sim$Supply_Leaf[i])


  S$Sim$NPP_Leaf[i]= S$Sim$Alloc_Leaf[i]/S$Parameters$epsilon_Leaf
  S$Sim$Rg_Leaf[i]= S$Sim$Alloc_Leaf[i]-S$Sim$NPP_Leaf[i]
  S$Sim$Mnat_Leaf[i]=S$Sim$CM_Leaf[previous_i(i,1)]/S$Parameters$lifespan_Leaf

  S$Sim$NPP_RE[i]= S$Sim$NPP_RE[i]+(S$Sim$Supply_Leaf[i]-S$Sim$Alloc_Leaf[i])

  S$Sim$M_ALS[i]=
    after(i,2)*max(0,S$Sim$CM_Leaf[previous_i(i,1)]*S$Sim$ALS[i])

  if(S$Sim$Plot_Age[i]>=
     S$Parameters$MeanAgePruning&S$Met_c$DOY[i]==S$Parameters$D_pruning){
    S$Sim$Mprun_Leaf[i]= S$Sim$CM_Leaf[previous_i(i,1)]*S$Parameters$LeafPruningRate
  }else{
    S$Sim$Mprun_Leaf[i]= 0
  }

  S$Sim$Mortality_Leaf[i]= S$Sim$Mnat_Leaf[i] + S$Sim$Mprun_Leaf[i] + S$Sim$M_ALS[i]

  # Fine Roots --------------------------------------------------------------

  S$Sim$Supply_FRoot[i]=
    S$Parameters$lambda_FRoot_remain*
    (S$Sim$Supply[i]-S$Sim$Alloc_Fruit[i]-
       S$Sim$Alloc_Shoot[i]-S$Sim$Alloc_SCR[i])

  S$Sim$Alloc_FRoot[i]=max(0,min(S$Sim$Alloc_Leaf[i],S$Sim$Supply_FRoot[i]))

  S$Sim$NPP_FRoot[i]= S$Sim$Alloc_FRoot[i]/S$Parameters$epsilon_FRoot

  S$Sim$Rg_FRoot[i]= S$Sim$Alloc_FRoot[i]-S$Sim$NPP_FRoot[i]

  S$Sim$NPP_RE[i]= S$Sim$NPP_RE[i]+(S$Sim$Supply_FRoot[i]-S$Sim$Alloc_FRoot[i])

  S$Sim$Mnat_FRoot[i]= S$Sim$CM_FRoot[previous_i(i,1)]/S$Parameters$lifespan_FRoot
  S$Sim$Mprun_FRoot[i]= S$Parameters$m_FRoot*S$Sim$Mprun_Leaf[i]
  S$Sim$Mortality_FRoot[i]= S$Sim$Mnat_FRoot[i]+S$Sim$Mprun_FRoot[i]


  # Biomass -----------------------------------------------------------------

  CM_tot=
    S$Sim$CM_Leaf[previous_i(i,1)] + S$Sim$CM_Shoot[previous_i(i,1)] +
    S$Sim$CM_SCR[previous_i(i,1)] + S$Sim$CM_FRoot[previous_i(i,1)]

  S$Sim$CM_Leaf[i]= S$Sim$CM_Leaf[previous_i(i,1)]+
    S$Sim$NPP_Leaf[i]-S$Sim$Mortality_Leaf[i]-
    S$Sim$Carbon_Lack_Mortality[i]*S$Sim$CM_Leaf[previous_i(i,1)]/CM_tot
  S$Sim$CM_Shoot[i]= S$Sim$CM_Shoot[previous_i(i,1)]+
    S$Sim$NPP_Shoot[i]-S$Sim$Mortality_Shoot[i]-
    S$Sim$Carbon_Lack_Mortality[i]*S$Sim$CM_Shoot[previous_i(i,1)]/CM_tot
  S$Sim$CM_Fruit[i]= S$Sim$CM_Fruit[previous_i(i,1)]+
    S$Sim$NPP_Fruit[i]-S$Sim$Overriped_Fruit[i]
  S$Sim$CM_SCR[i]= S$Sim$CM_SCR[previous_i(i,1)]+
    S$Sim$NPP_SCR[i]-S$Sim$Mortality_SCR[i]-
    S$Sim$Carbon_Lack_Mortality[i]*S$Sim$CM_SCR[previous_i(i,1)]/CM_tot
  S$Sim$CM_FRoot[i]= S$Sim$CM_FRoot[previous_i(i,1)]+
    S$Sim$NPP_FRoot[i]-S$Sim$Mortality_FRoot[i]-
    S$Sim$Carbon_Lack_Mortality[i]*S$Sim$CM_FRoot[previous_i(i,1)]/CM_tot
  S$Sim$CM_RE[i]=S$Sim$CM_RE[previous_i(i,1)]+S$Sim$NPP_RE[i]-
    S$Sim$Consumption_RE[i]

  S$Sim$DM_Leaf[i]= S$Sim$CM_Leaf[i]/S$Parameters$CC_Leaf
  S$Sim$DM_Shoot[i]= S$Sim$CM_Shoot[i]/S$Parameters$CC_Shoot
  S$Sim$DM_Fruit[i]=S$Sim$CM_Fruit[i]/S$Parameters$CC_Fruit
  S$Sim$DM_SCR[i]= S$Sim$CM_SCR[i]/
    S$Parameters$CC_SCR
  S$Sim$DM_FRoot[i]= S$Sim$CM_FRoot[i]/S$Parameters$CC_FRoots
  S$Sim$DM_RE[i]=S$Sim$CM_RE[i]/S$Parameters$CC_SCR

  # Total Respiration and NPP -----------------------------------------------

  S$Sim$Rg[i]= S$Sim$Rg_Fruit[i]+S$Sim$Rg_Leaf[i]+
    S$Sim$Rg_Shoot[i]+S$Sim$Rg_SCR[i]+
    S$Sim$Rg_FRoot[i]
  S$Sim$Ra[i]=S$Sim$Rm[i]+S$Sim$Rg[i]
  S$Sim$NPP[i]=S$Sim$NPP_Shoot[i]+S$Sim$NPP_SCR[i]+
    S$Sim$NPP_Fruit[i]+S$Sim$NPP_Leaf[i]+S$Sim$NPP_FRoot[i]
}
