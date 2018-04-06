#' Generator for class Simulation (simulation object)
#' @field Sim            A list, this is the simulation output list
#' @field Met_c          A list, this is the input meteorology
#' @field Parameters     A list, this is the parameters list
#' @field Zero_then_One  A vector, this is a helper vector (will be depreciated soon)
SimulationClass= setRefClass("Simulation",
                               fields = list(Sim = "list",
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
Init_Sim= function(S){
  S$Sim$LAI= rep_len(0.1,length(S$Sim$Cycle))
  S$Sim$LAIplot= rep_len(0,length(S$Sim$Cycle))
  #Leaf Area per Plant location, to convert per ha using density,cannot be zero at beginning,
  # otherwise, GPP does not start and nothing grows
  S$Sim$CM_RE=
  S$Sim$Rm=
  S$Sim$CM_SCR=
  S$Sim$Demand_Fruit=
  S$Sim$CM_Fruit=
  S$Sim$Sucrose_Mass=
  S$Sim$Harvest_Maturity=
    S$Sim$CM_FRoot=
  S$Sim$CM_RsWood= rep_len(0,length(S$Sim$Cycle))
  S$Sim$CM_Leaf= rep_len(1,length(S$Sim$Cycle))

  S$Sim$DM_Leaf=
    S$Sim$DM_FRoot=
    S$Sim$DM_RsWood=
    S$Sim$DM_Fruit=
    S$Sim$DM_SCR=
    S$Sim$DM_Fruit_Cohort=
    S$Sim$DM_RE=
  S$Sim$Mact_SCR=
    S$Sim$Mnat_SCR=
  S$Sim$Mprun_RsWood=
  S$Sim$DegreeDays_Tcan=
    S$Sim$p_budbreakperday=
    S$Sim$Budinit=
    S$Sim$BudBreak=
    S$Sim$Bud_available=
    S$Sim$BudBreak_cohort=
    S$Sim$Alloc_Fruit_Cohort=
    S$Sim$NPP_Fruit_Cohort=
    S$Sim$CM_Fruit_Cohort=
    S$Sim$Maturation_duration=
    S$Sim$Sucrose_Content= rep_len(0,length(S$Sim$Cycle))
  S$Sim$Temp_cor_Bud= rep_len(1,length(S$Sim$Cycle))


  S$Sim$Tcan_Diurnal_Cof_deg=
    S$Sim$NPP_RE=
    S$Sim$lue=
    S$Sim$GPP=
    S$Sim$K_Dif=
    S$Sim$K_Dir=
    S$Sim$Consumption_RE=
    S$Sim$Offer=
    S$Sim$Carbon_Lack_Mortality=
    S$Sim$Alloc_RsWood=
    S$Sim$NPP_RsWood=
    S$Sim$Rc_RsWood=
    S$Sim$Mnat_RsWood=
    S$Sim$Mortality_RsWood=
    S$Sim$Rm_RsWood=
    S$Sim$lambdaSCRage=
    S$Sim$Alloc_SCR=
    S$Sim$NPP_SCR=
    S$Sim$Rc_SCR=
    S$Sim$Rm_SCR=
    S$Sim$Mortality_SCR=
    S$Sim$Harvest_Maturity_Pot=
    S$Sim$ratioNodestoLAI=
    S$Sim$Offer_Fruit=
    S$Sim$Alloc_Fruit=
    S$Sim$Overriped_Fruit=
    S$Sim$NPP_Fruit=
    S$Sim$Rc_Fruit=
    S$Sim$Harvest_Fruit=
    S$Sim$Rm_Fruit=
    S$Sim$Offer_Leaf=
    S$Sim$Alloc_Leaf=
    S$Sim$NPP_Leaf=
    S$Sim$Rc_Leaf=
    S$Sim$Mnat_Leaf=
    S$Sim$M_ALS=
    S$Sim$MnatALS_Leaf=
    S$Sim$Mprun_Leaf=
    S$Sim$Mortality_Leaf=
    S$Sim$Rm_Leaf=
    S$Sim$Demand_FRoot=
    S$Sim$Offer_FRoot=
    S$Sim$Alloc_FRoot=
    S$Sim$NPP_FRoot=
    S$Sim$Rc_FRoot=
    S$Sim$Mnat_FRoot=
    S$Sim$Mprun_FRoot=
    S$Sim$Mortality_FRoot=
    S$Sim$Rm_FRoot=
    S$Sim$Rc=
    S$Sim$Ra=
    S$Sim$NPP=
    S$Sim$Cbalance=rep_len(0,length(S$Sim$Cycle))
  S$Sim$BudInitPeriod= rep(FALSE,length(S$Sim$Cycle))
  S$Sim$Rn_tot=
  S$Sim$Date_harvest= rep_len(NA_real_,length(S$Sim$Cycle))

  S$Sim$Throughfall=
    S$Sim$IntercRevapor=
    S$Sim$ExcessRunoff=
    S$Sim$SuperficialRunoff=
    S$Sim$TotSuperficialRunoff=
    S$Sim$InfilCapa=
    S$Sim$Infiltration=
    S$Sim$Drain_1=
    S$Sim$Drain_2=
    S$Sim$Drain_3=
    S$Sim$EW_1=
    S$Sim$REW_1=
    S$Sim$EW_2=
    S$Sim$REW_2=
    S$Sim$EW_3=
    S$Sim$REW_3=
    S$Sim$EW_tot=
    S$Sim$REW_tot=
    S$Sim$E_Soil=
    S$Sim$LE_Plot=
    S$Sim$LE_Soil=
    S$Sim$H_Soil=
    S$Sim$Q_Soil=
    S$Sim$Rn_Soil=
    S$Sim$LE_Tree=
    S$Sim$H_tot=
    S$Sim$LE_tot=
    S$Sim$Diff_T=
    S$Sim$Tleaf_Coffee=
    S$Sim$WindSpeed_Coffee=
    S$Sim$TairCanopy=
    S$Sim$APAR_Dif=
    S$Sim$APAR=
    S$Sim$PAR_Soil=
    S$Sim$Tcan_MAESPA_Coffee=
    S$Sim$SoilWaterPot=
    S$Sim$LeafWaterPotential=
    S$Sim$AEu=
    S$Sim$IntercMax=
    S$Sim$T_Cof=
    S$Sim$T_tot=
    S$Sim$RootWaterExtract_1=
    S$Sim$RootWaterExtract_2=
    S$Sim$RootWaterExtract_3=
    S$Sim$ETR=
    S$Sim$SWD=
    S$Sim$H_Coffee=
    S$Sim$Rn_Coffee=
    S$Sim$LE_Coffee= rep_len(0,length(S$Sim$Cycle))

  S$Sim$W_1= rep_len(290,length(S$Sim$Cycle))
  S$Sim$W_2= rep_len(66,length(S$Sim$Cycle))
  S$Sim$W_3= rep_len(69,length(S$Sim$Cycle))
  S$Sim$W_tot= S$Sim$W_1+S$Sim$W_2+S$Sim$W_3

  S$Sim$CanopyHumect= rep_len(0,length(S$Sim$Cycle))
  S$Sim$WSurfaceRes= rep_len(0,length(S$Sim$Cycle))

  if(S$Parameters$Tree_Species=="No_Shade"){
    No_Shade.init(S)
  }else{
    Tree.init(S)
  }
}

#' @rdname Init_Sim
#' @export
No_Shade.init= function(S){
  # NB: if Tree_Species is NULL (i.e. no shade trees), then do not add
  # any trees related variables to the main table, except for the few ones
  # needed in-code (e.g. Tree Height for GBCANMS):
  # Shade tree layer computations (common for all species)
  # Should output at least APAR_Tree, LAI_Tree, T_Tree, Rn_Tree, H_Tree,
  # LE_Tree (sum of transpiration + leaf evap)
  # And via allometries: Height_Tree for canopy boundary layer conductance
  S$Sim$LAI_Tree=
    S$Sim$APAR_Tree=
    S$Sim$T_Tree=
    S$Sim$Rn_Tree=
    S$Sim$H_Tree=
    S$Sim$LE_Tree=
    S$Sim$Height_Tree=
    rep_len(0,length(S$Sim$Cycle))
  S$Sim$WindSpeed_Tree= S$Met_c$WindSpeed
  S$Sim$TairCanopy_Tree= S$Met_c$Tair
}

#' @rdname Init_Sim
#' @export
Tree.init= function(S){
  # Initialisation of Shade tree variables:
  S$Sim$CM_Leaf_Tree=
    S$Sim$CM_Stem_Tree=
    S$Sim$CM_Branch_Tree=
    S$Sim$CM_FRoot_Tree=
    S$Sim$CM_CR_Tree= rep_len(0.01,length(S$Sim$Cycle))
  S$Sim$CM_RE_Tree= rep_len(0.15,length(S$Sim$Cycle))

  S$Sim$LAI_Tree=
    S$Sim$CM_Leaf_Tree*(S$Parameters$SLA_Tree/1000)/
    S$Parameters$CContent_Leaf_Tree

  S$Sim$Trunk_H_Tree=
    S$Sim$Crown_H_Tree=
    S$Sim$LA_Tree=
    S$Sim$DM_Leaf_Tree=
    S$Sim$DM_Branch_Tree=
    S$Sim$DM_Stem_Tree=
    S$Sim$DM_CR_Tree=
    S$Sim$DM_FRoot_Tree=
    S$Sim$DM_Stem_FGM_Tree=
    S$Sim$DM_RE_Tree=
    S$Sim$Mprun_Branch_Tree=
    S$Sim$Mprun_FRoot_Tree=
    S$Sim$Mprun_Leaf_Tree=
    S$Sim$Mact_Stem_Tree=
    S$Sim$Mact_CR_Tree=
    S$Sim$Rm_Tree=
    S$Sim$DBH_Tree=
    S$Sim$Crown_H_Tree=
    S$Sim$CrownProj_Tree=
    S$Sim$LAD_Tree=
    S$Sim$K_Dif_Tree=
    S$Sim$K_Dir_Tree=
    S$Sim$APAR_Dif_Tree=
    S$Sim$APAR_Dir_Tree=
    S$Sim$APAR_Tree=
    S$Sim$Transmittance_Tree=
    S$Sim$lue_Tree=
    S$Sim$T_Tree=
    S$Sim$H_Tree=
    S$Sim$Tleaf_Tree=
    S$Sim$GPP_Tree=
    S$Sim$Rm_Leaf_Tree=
    S$Sim$Rm_CR_Tree=
    S$Sim$Rm_Branch_Tree=
    S$Sim$Rm_Stem_Tree=
    S$Sim$Rm_FRoot_Tree=
    S$Sim$Offer_Total_Tree=
    S$Sim$Alloc_Stem_Tree=
    S$Sim$NPP_Stem_Tree=
    S$Sim$Rc_Stem_Tree=
    S$Sim$Alloc_CR_Tree=
    S$Sim$NPP_CR_Tree=
    S$Sim$Rc_CR_Tree=
    S$Sim$Alloc_Branch_Tree=
    S$Sim$NPP_Branch_Tree=
    S$Sim$Rc_Branch_Tree=
    S$Sim$Mact_Branch_Tree=
    S$Sim$Alloc_Leaf_Tree=
    S$Sim$NPP_Leaf_Tree=
    S$Sim$Rc_Leaf_Tree=
    S$Sim$Mact_Leaf_Tree=
    S$Sim$Alloc_FRoot_Tree=
    S$Sim$NPP_FRoot_Tree=
    S$Sim$Rc_FRoot_Tree=
    S$Sim$Mact_FRoot_Tree=
    S$Sim$Alloc_Reserves_Tree=
    S$Sim$Rc_Reserves_Tree=
    S$Sim$M_Rm_Stem_Tree=
    S$Sim$M_Rm_CR_Tree=
    S$Sim$M_Rm_Branch_Tree=
    S$Sim$M_Rm_Leaf_Tree=
    S$Sim$M_Rm_FRoot_Tree=
    S$Sim$M_Rm_Reserves_Tree=
    S$Sim$Mortality_Leaf_Tree=
    S$Sim$Mortality_Branch_Tree=
    S$Sim$Mortality_Stem_Tree=
    S$Sim$Mortality_CR_Tree=
    S$Sim$Mortality_FRoot_Tree=
    S$Sim$Rc_Tree=
    S$Sim$Ra_Tree=
    S$Sim$DeltaCM__Tree=
    S$Sim$NPP_Tree=
    S$Sim$Cbalance_Tree=
    S$Sim$Height_Tree=
    S$Sim$CrownRad_Tree=
    S$Sim$NPP_Reserves_Tree=
    S$Sim$Consumption_RE_Tree=
    S$Sim$TimetoThin_Tree=
    S$Sim$MThinning_Stem_Tree=
    S$Sim$MThinning_CR_Tree=
    S$Sim$MThinning_Branch_Tree=
    S$Sim$MThinning_Leaf_Tree=
    S$Sim$MThinning_FRoot_Tree=
    S$Sim$Rn_Tree=
    S$Sim$H_Tree=
    S$Sim$Rn_tot=
    S$Sim$Rn_Tree=
    rep_len(0,length(S$Sim$Cycle))

  S$Sim$Stocking_Tree= rep_len(S$Parameters$StockingTree_treeha1/10000,
                                     length(S$Sim$Cycle))

  S$Sim$WindSpeed_Tree= S$Met_c$WindSpeed
  S$Sim$TairCanopy_Tree= S$Met_c$Tair
}
