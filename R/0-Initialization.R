#' Generator for class Simulation
#'
#' @description Set the reference class for the simulation object, which hold the Meteorology, the simulation outputs and the
#'              parameters.
#'
#' @details This function is internal and shouldn't be called by users.
#'
#' @return The generator for the simulation class.
#'
#' @examples
#' # Create an object of class "Simulation":
#' S= SimulationClass$new()
#' # Fill in the parameters:
#' S$Parameters= Parameters
#' @keywords internal
#'
#' @export
SimulationClass <- setRefClass("Simulation",
                               fields = list(Table_Day = "list",
                                             Met_c= "list",
                                             Parameters= "list",
                                             Zero_then_One="vector"))

#' Initialise model variables.
#'
#' @description Initialise all model variables before start
#'
#' @param S Model internal working list (= to output list at start)
#'
#' @aliases No_Shade.init Tree.init
#'
#' @details User should not use this function
#'
#' @keywords internal
#'
#' @export
Init_Table_Day= function(S){
  S$Table_Day$LAI= rep_len(0.1,length(S$Table_Day$Cycle))
  S$Table_Day$LAIplot= rep_len(0,length(S$Table_Day$Cycle))
  #Leaf Area per Plant location, to convert per ha using density,cannot be zero at beginning,
  # otherwise, GPP does not start and nothing grows
  S$Table_Day$CM_RE=
  S$Table_Day$Rm=
  S$Table_Day$CM_SCR=
  S$Table_Day$Demand_Fruit=
  S$Table_Day$CM_Fruit=
  S$Table_Day$Sucrose_Mass=
  S$Table_Day$Harvest_Maturity=
    S$Table_Day$CM_FRoot=
  S$Table_Day$CM_RsWood= rep_len(0,length(S$Table_Day$Cycle))
  S$Table_Day$CM_Leaf= rep_len(1,length(S$Table_Day$Cycle))

  S$Table_Day$DM_Leaf=
    S$Table_Day$DM_FRoot=
    S$Table_Day$DM_RsWood=
    S$Table_Day$DM_Fruit=
    S$Table_Day$DM_SCR=
    S$Table_Day$DM_Fruit_Cohort=
    S$Table_Day$DM_RE=
  S$Table_Day$Mact_SCR=
    S$Table_Day$Mnat_SCR=
  S$Table_Day$Mprun_RsWood=
  S$Table_Day$DegreeDays_Tcan=
    S$Table_Day$Budinit=
    S$Table_Day$BudBreak=
    S$Table_Day$Bud_available=
    S$Table_Day$BudBreak_cohort=
    S$Table_Day$Alloc_Fruit_Cohort=
    S$Table_Day$NPP_Fruit_Cohort=
    S$Table_Day$CM_Fruit_Cohort=
    S$Table_Day$Maturation_duration=
    S$Table_Day$Sucrose_Content= rep_len(0,length(S$Table_Day$Cycle))
  S$Table_Day$Temp_cor_Bud= rep_len(1,length(S$Table_Day$Cycle))


  S$Table_Day$Tcan_Diurnal_Cof_deg=
    S$Table_Day$NPP_RE=
    S$Table_Day$lue=
    S$Table_Day$GPP=
    S$Table_Day$K_Dif=
    S$Table_Day$K_Dir=
    S$Table_Day$Consumption_RE=
    S$Table_Day$Offer=
    S$Table_Day$Carbon_Lack_Mortality=
    S$Table_Day$Alloc_RsWood=
    S$Table_Day$NPP_RsWood=
    S$Table_Day$Rc_RsWood=
    S$Table_Day$Mnat_RsWood=
    S$Table_Day$Mortality_RsWood=
    S$Table_Day$Rm_RsWood=
    S$Table_Day$lambdaSCRage=
    S$Table_Day$Alloc_SCR=
    S$Table_Day$NPP_SCR=
    S$Table_Day$Rc_SCR=
    S$Table_Day$Rm_SCR=
    S$Table_Day$Mortality_SCR=
    S$Table_Day$Harvest_Maturity_Pot=
    S$Table_Day$ratioNodestoLAI=
    S$Table_Day$Offer_Fruit=
    S$Table_Day$Alloc_Fruit=
    S$Table_Day$Overriped_Fruit=
    S$Table_Day$NPP_Fruit=
    S$Table_Day$Rc_Fruit=
    S$Table_Day$Harvest_Fruit=
    S$Table_Day$Rm_Fruit=
    S$Table_Day$Offer_Leaf=
    S$Table_Day$Alloc_Leaf=
    S$Table_Day$NPP_Leaf=
    S$Table_Day$Rc_Leaf=
    S$Table_Day$Mnat_Leaf=
    S$Table_Day$M_ALS=
    S$Table_Day$MnatALS_Leaf=
    S$Table_Day$Mprun_Leaf=
    S$Table_Day$Mortality_Leaf=
    S$Table_Day$Rm_Leaf=
    S$Table_Day$Demand_FRoot=
    S$Table_Day$Offer_FRoot=
    S$Table_Day$Alloc_FRoot=
    S$Table_Day$NPP_FRoot=
    S$Table_Day$Rc_FRoot=
    S$Table_Day$Mnat_FRoot=
    S$Table_Day$Mprun_FRoot=
    S$Table_Day$Mortality_FRoot=
    S$Table_Day$Rm_FRoot=
    S$Table_Day$Rc=
    S$Table_Day$Ra=
    S$Table_Day$NPP=
    S$Table_Day$Cbalance=rep_len(0,length(S$Table_Day$Cycle))

  S$Table_Day$Rn_tot=
  S$Table_Day$Date_harvest= rep_len(NA_real_,length(S$Table_Day$Cycle))

  S$Table_Day$Throughfall=
    S$Table_Day$IntercRevapor=
    S$Table_Day$ExcessRunoff=
    S$Table_Day$SuperficialRunoff=
    S$Table_Day$TotSuperficialRunoff=
    S$Table_Day$InfilCapa=
    S$Table_Day$Infiltration=
    S$Table_Day$Drain_1=
    S$Table_Day$Drain_2=
    S$Table_Day$Drain_3=
    S$Table_Day$EW_1=
    S$Table_Day$REW_1=
    S$Table_Day$EW_2=
    S$Table_Day$REW_2=
    S$Table_Day$EW_3=
    S$Table_Day$REW_3=
    S$Table_Day$EW_tot=
    S$Table_Day$REW_tot=
    S$Table_Day$E_Soil=
    S$Table_Day$LE_Plot=
    S$Table_Day$LE_Soil=
    S$Table_Day$H_Soil=
    S$Table_Day$Q_Soil=
    S$Table_Day$Rn_Soil=
    S$Table_Day$LE_Tree=
    S$Table_Day$H_tot=
    S$Table_Day$LE_tot=
    S$Table_Day$Diff_T=
    S$Table_Day$Tleaf_Coffee=
    S$Table_Day$WindSpeed_Coffee=
    S$Table_Day$TairCanopy=
    S$Table_Day$APAR_Dif=
    S$Table_Day$APAR=
    S$Table_Day$PAR_Soil=
    S$Table_Day$Tcan_MAESPA_Coffee=
    S$Table_Day$SoilWaterPot=
    S$Table_Day$LeafWaterPotential=
    S$Table_Day$AEu=
    S$Table_Day$IntercMax=
    S$Table_Day$T_Cof=
    S$Table_Day$T_tot=
    S$Table_Day$RootWaterExtract_1=
    S$Table_Day$RootWaterExtract_2=
    S$Table_Day$RootWaterExtract_3=
    S$Table_Day$ETR=
    S$Table_Day$SWD=
    S$Table_Day$H_Coffee=
    S$Table_Day$Rn_Coffee=
    S$Table_Day$LE_Coffee= rep_len(0,length(S$Table_Day$Cycle))

  S$Table_Day$W_1= rep_len(290,length(S$Table_Day$Cycle))
  S$Table_Day$W_2= rep_len(66,length(S$Table_Day$Cycle))
  S$Table_Day$W_3= rep_len(69,length(S$Table_Day$Cycle))
  S$Table_Day$W_tot= S$Table_Day$W_1+S$Table_Day$W_2+S$Table_Day$W_3

  S$Table_Day$CanopyHumect= rep_len(0,length(S$Table_Day$Cycle))
  S$Table_Day$WSurfaceRes= rep_len(0,length(S$Table_Day$Cycle))

  if(S$Parameters$Tree_Species=="No_Shade"){
    No_Shade.init(S)
  }else{
    Tree.init(S)
  }
}

#' @rdname Init_Table_Day
#' @export
No_Shade.init= function(S){
  # NB: if Tree_Species is NULL (i.e. no shade trees), then do not add
  # any trees related variables to the main table, except for the few ones
  # needed in-code (e.g. Tree Height for GBCANMS):
  # Shade tree layer computations (common for all species)
  # Should output at least APAR_Tree, LAI_Tree, T_Tree, Rn_Tree, H_Tree,
  # LE_Tree (sum of transpiration + leaf evap)
  # And via allometries: Height_Tree for canopy boundary layer conductance
  S$Table_Day$LAI_Tree=
    S$Table_Day$APAR_Tree=
    S$Table_Day$T_Tree=
    S$Table_Day$Rn_Tree=
    S$Table_Day$H_Tree=
    S$Table_Day$LE_Tree=
    S$Table_Day$Height_Tree=
    rep_len(0,length(S$Table_Day$Cycle))
  S$Table_Day$WindSpeed_Tree= S$Met_c$WindSpeed
  S$Table_Day$TairCanopy_Tree= S$Met_c$Tair
}

#' @rdname Init_Table_Day
#' @export
Tree.init= function(S){
  # Initialisation of Shade tree variables:
  S$Table_Day$CM_Leaf_Tree=
    S$Table_Day$CM_Stem_Tree=
    S$Table_Day$CM_Branch_Tree=
    S$Table_Day$CM_FRoot_Tree=
    S$Table_Day$CM_CR_Tree= rep_len(0.01,length(S$Table_Day$Cycle))
  S$Table_Day$CM_Reserves_Tree= rep_len(0.15,length(S$Table_Day$Cycle))

  S$Table_Day$LAI_Tree=
    S$Table_Day$CM_Leaf_Tree*(S$Parameters$SLA_Tree/1000)/
    S$Parameters$CContent_Leaf_Tree

  S$Table_Day$Trunk_H_Tree=
    S$Table_Day$Crown_H_Tree=
    S$Table_Day$LA_Tree=
    S$Table_Day$DM_Leaf_Tree=
    S$Table_Day$DM_Branch_Tree=
    S$Table_Day$DM_Stem_Tree=
    S$Table_Day$DM_CR_Tree=
    S$Table_Day$DM_FRoot_Tree=
    S$Table_Day$DM_Stem_FGM_Tree=
    S$Table_Day$DM_RE_Tree=
    S$Table_Day$Mprun_Branch_Tree=
    S$Table_Day$Mprun_FRoot_Tree=
    S$Table_Day$Mprun_Leaf_Tree=
    S$Table_Day$Mact_Stem_Tree=
    S$Table_Day$Mact_CR_Tree=
    S$Table_Day$Rm_Tree=
    S$Table_Day$DBH_Tree=
    S$Table_Day$Crown_H_Tree=
    S$Table_Day$CrownProj_Tree=
    S$Table_Day$LAD_Tree=
    S$Table_Day$K_Dif_Tree=
    S$Table_Day$K_Dir_Tree=
    S$Table_Day$APAR_Dif_Tree=
    S$Table_Day$APAR_Dir_Tree=
    S$Table_Day$APAR_Tree=
    S$Table_Day$Transmittance_Tree=
    S$Table_Day$lue_Tree=
    S$Table_Day$T_Tree=
    S$Table_Day$H_Tree=
    S$Table_Day$Tleaf_Tree=
    S$Table_Day$GPP_Tree=
    S$Table_Day$Rm_Leaf_Tree=
    S$Table_Day$Rm_CR_Tree=
    S$Table_Day$Rm_Branch_Tree=
    S$Table_Day$Rm_Stem_Tree=
    S$Table_Day$Rm_FRoot_Tree=
    S$Table_Day$Offer_Total_Tree=
    S$Table_Day$Alloc_Stem_Tree=
    S$Table_Day$NPP_Stem_Tree=
    S$Table_Day$Rc_Stem_Tree=
    S$Table_Day$Alloc_CR_Tree=
    S$Table_Day$NPP_CR_Tree=
    S$Table_Day$Rc_CR_Tree=
    S$Table_Day$Alloc_Branch_Tree=
    S$Table_Day$NPP_Branch_Tree=
    S$Table_Day$Rc_Branch_Tree=
    S$Table_Day$Mact_Branch_Tree=
    S$Table_Day$Alloc_Leaf_Tree=
    S$Table_Day$NPP_Leaf_Tree=
    S$Table_Day$Rc_Leaf_Tree=
    S$Table_Day$Mact_Leaf_Tree=
    S$Table_Day$Alloc_FRoot_Tree=
    S$Table_Day$NPP_FRoot_Tree=
    S$Table_Day$Rc_FRoot_Tree=
    S$Table_Day$Mact_FRoot_Tree=
    S$Table_Day$Alloc_Reserves_Tree=
    S$Table_Day$Rc_Reserves_Tree=
    S$Table_Day$Rc_Tree=
    S$Table_Day$Ra_Tree=
    S$Table_Day$DeltaCM__Tree=
    S$Table_Day$NPP_Tree=
    S$Table_Day$Cbalance_Tree=
    S$Table_Day$Height_Tree=
    S$Table_Day$CrownRad_Tree=
    S$Table_Day$NPP_Reserves_Tree=
    S$Table_Day$Consumption_RE_Tree=
    S$Table_Day$TimetoThin_Tree=
    S$Table_Day$MThinning_Stem_Tree=
    S$Table_Day$MThinning_CR_Tree=
    S$Table_Day$MThinning_Branch_Tree=
    S$Table_Day$MThinning_Leaf_Tree=
    S$Table_Day$MThinning_FRoot_Tree=
    S$Table_Day$Rn_Tree=
    S$Table_Day$H_Tree=
    S$Table_Day$Rn_tot=
    S$Table_Day$Rn_Tree=
    rep_len(0,length(S$Table_Day$Cycle))


  S$Table_Day$Stocking_Tree= S$Parameters$StockingTree_treeha1/10000


  S$Table_Day$WindSpeed_Tree= S$Met_c$WindSpeed
  S$Table_Day$TairCanopy_Tree= S$Met_c$Tair
}
