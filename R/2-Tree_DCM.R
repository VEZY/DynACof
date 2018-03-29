
#' Shade Tree module
#'
#' @description This make all computations for shade trees (similar to coffee, but no fruits) for the ith
#'              day by modifying the \code{S} list in place.
#'
#' @param S The main simulation list to make the computation on and to modify.
#' @param i The index of the day since the first day of the simulation.
#'
#' @return Nothing, modify the list of simulation \code{S} in place. See \code{\link{DynACof}} for
#'         more details.
#'
#' @note This function shouldn't be called by the user. It is made as a module so it is easier for
#'       advanced users to modify the code.
#'       \code{No_Shade()} is used as an empty function that is called when there are no shade trees.
#'
#' @aliases No_Shade
#'
#' @keywords internal
#'
#' @seealso \code{\link{DynACof}}
#'
#' @export
Shade.Tree= function(S,i){
  # Shade tree layer computations (common for all species)
  # Should output at least APAR_Tree, LAI_Tree, T_Tree, Rn_Tree, H_Tree,
  # LE_Tree (sum of transpiration + leaf evap)
  # And via allometries: Height_Tree for canopy boundary layer conductance


  # Metamodel for kdif and kdir
  S$Parameters$k(S,i)

  #For stocking Cordia=50, without thinning, with metamodel
  S$Table_Day$APAR_Dif_Tree[i]=
    (S$Met_c$PAR[i]*S$Met_c$FDiff[i])*
    (1-exp(-S$Table_Day$K_Dif_Tree[i]*S$Table_Day$LAI_Tree[i-S$Zero_then_One[i]]))#MJ m-2 d-1
  S$Table_Day$APAR_Dir_Tree[i]= (S$Met_c$PAR[i]*(1-S$Met_c$FDiff[i]))*
    (1-exp(-S$Table_Day$K_Dir_Tree[i]*S$Table_Day$LAI_Tree[i-S$Zero_then_One[i]]))#MJ m-2 d-1
  S$Table_Day$APAR_Tree[i]= max(0,S$Table_Day$APAR_Dir_Tree[i]+S$Table_Day$APAR_Dif_Tree[i])
  # S$Table_Day$APAR_Tree[i]= S$Met_c$PAR[i]*(1-exp(-S$Table_Day$k_Tree[i]*
  # S$Table_Day$LAI_Tree[i-S$Zero_then_One[i]]))
  S$Table_Day$Transmittance_Tree[i]=
    1-(S$Table_Day$APAR_Tree[i]/S$Met_c$PAR[i])
  S$Table_Day$Transmittance_Tree[i][is.nan(S$Table_Day$Transmittance_Tree[i])]=0
  # Calling the metamodels for LUE, Transpiration and sensible heat flux :
  S$Parameters$Metamodels(S,i)
  # Computing the air temperature in the shade tree layer:


  S$Table_Day$TairCanopy_Tree[i]=
    S$Met_c$Tair[i]+(S$Table_Day$H_Tree[i]*S$Parameters$MJ_to_W)/
    (bigleaf::air.density(S$Met_c$Tair[i],S$Met_c$Pressure[i]/10)*
       S$Parameters$Cp*
       G_bulk(Wind= S$Met_c$WindSpeed[i], ZHT= S$Parameters$ZHT,
              LAI= S$Table_Day$LAI_Tree[i-S$Zero_then_One[i]],
              extwind= S$Parameters$extwind,
              Z_top= S$Table_Day$Height_Tree[i-S$Zero_then_One[i]]))
  # NB : using WindSpeed and not WindSpeed_Tree because wind extinction is already
  # computed in G_bulk (until top of canopy).

  S$Table_Day$Tleaf_Tree[i]=
    S$Met_c$Tair[i]+(S$Table_Day$H_Tree[i]*S$Parameters$MJ_to_W)/
    (bigleaf::air.density(S$Met_c$Tair[i],S$Met_c$Pressure[i]/10)*
       S$Parameters$Cp*
       1/(1/G_bulk(Wind= S$Met_c$WindSpeed[i], ZHT= S$Parameters$ZHT,
                   LAI= S$Table_Day$LAI_Tree[i-S$Zero_then_One[i]],
                   extwind= S$Parameters$extwind,
                   Z_top= S$Table_Day$Height_Tree[i-S$Zero_then_One[i]])+
            1/Gb_h(Wind = S$Met_c$WindSpeed[i], wleaf= S$Parameters$wleaf_Tree,
                   LAI_lay= S$Table_Day$LAI_Tree[i-S$Zero_then_One[i]],
                   LAI_abv= 0,ZHT = S$Parameters$ZHT,
                   Z_top = S$Table_Day$Height_Tree[i-S$Zero_then_One[i]],
                   extwind= S$Parameters$extwind)
       ))



  #GPP
  S$Table_Day$GPP_Tree[i]= S$Table_Day$lue_Tree[i]*S$Table_Day$APAR_Tree[i]
  #Tree Thinning threshold when Transmittance <=S$Parameters$ThinThresh, then
  if(S$Table_Day$Transmittance_Tree[i]<S$Parameters$ThinThresh){
    S$Table_Day$TimetoThin_Tree[i]=1
  }

  # Maintenance respiration -------------------------------------------------

  # Rm is computed at the beginning of the day on the drymass of the previous day.
  S$Table_Day$Rm_Leaf_Tree[i]=
    S$Parameters$PaliveLeaf_Tree*S$Table_Day$DM_Leaf_Tree[i-S$Zero_then_One[i]]*
    S$Parameters$MRN_Tree*S$Parameters$NContentLeaf_Tree*S$Parameters$Q10Leaf_Tree^
    ((S$Met_c$Tair[i]-S$Parameters$TMR)/10)

  S$Table_Day$Rm_CR_Tree[i]=
    S$Parameters$PaliveCR_Tree*
    S$Table_Day$DM_CR_Tree[i-S$Zero_then_One[i]]*
    S$Parameters$MRN_Tree*S$Parameters$NContentCR_Tree*
    S$Parameters$Q10CR_Tree^(
      (S$Met_c$Tair[i]-S$Parameters$TMR)/10)

  S$Table_Day$Rm_Branch_Tree[i]=
    S$Parameters$PaliveBranch_Tree*
    S$Table_Day$DM_Branch_Tree[i-S$Zero_then_One[i]]*
    S$Parameters$MRN_Tree*S$Parameters$NContentBranch_Tree*
    S$Parameters$Q10Branch_Tree^(
      (S$Met_c$Tair[i]-S$Parameters$TMR)/10)

  S$Table_Day$Rm_Stem_Tree[i]=
    S$Parameters$PaliveStem_Tree[S$Table_Day$Plot_Age[i],2]*
    S$Table_Day$DM_Stem_Tree[i-S$Zero_then_One[i]]*
    S$Parameters$MRN_Tree*S$Parameters$NContentStem_Tree*
    S$Parameters$Q10Stem_Tree^(
      (S$Met_c$Tair[i]-S$Parameters$TMR)/10)

  S$Table_Day$Rm_FRoot_Tree[i]=
    S$Parameters$PaliveFRoot_Tree*
    S$Table_Day$DM_FRoot_Tree[i-S$Zero_then_One[i]]*
    S$Parameters$MRN*S$Parameters$NContentFRoot_Tree*
    S$Parameters$Q10FRoot_Tree^(
      (S$Met_c$Tair[i]-S$Parameters$TMR)/10)

  S$Table_Day$Rm_Tree[i]=
    S$Table_Day$Rm_Leaf_Tree[i]+ S$Table_Day$Rm_CR_Tree[i]+
    S$Table_Day$Rm_Branch_Tree[i]+S$Table_Day$Rm_Stem_Tree[i]+
    S$Table_Day$Rm_FRoot_Tree[i]

  ############################----- Shade Tree Allocation ----############################

  ########## Potential use of reserves####
  # Reserves are used only if GPP doesn't meet the maintenance respiration + 10% need.
  # Thus, if GPP is < to Rm*1.1, then we take the needed C to meet the Rm (Rm*1.1-GPP), but not more than
  # there is C in the reserves
  if(S$Table_Day$GPP_Tree[i]<(1.2*S$Table_Day$Rm_Tree[i])){
    S$Table_Day$Consumption_RE_Tree[i]=
      max(0,min(S$Table_Day$CM_Reserves_Tree[previous_i(i,1)],S$Parameters$kres_max_Tree*S$Table_Day$Rm_Tree[i]))
  }

  ### Offer Function: NB, Rm is used from the previous i, assumed not very different but could
  # also be computed first, actually
  S$Table_Day$Offer_Total_Tree[i]=
    S$Table_Day$GPP_Tree[i]-S$Table_Day$Rm_Tree[i]+S$Table_Day$Consumption_RE_Tree[i]
  # If the offer is negative, there is mortality

  #### Stem ####
  # Offer: NB, Rm is used from the previous i, assumed not very different but could also be computed first, actually
  S$Table_Day$Alloc_Stem_Tree[i]=
    S$Parameters$lambda_Stem_Tree*S$Table_Day$Offer_Total_Tree[i]
  #NPP = Offer * growth cost coefficient:
  S$Table_Day$NPP_Stem_Tree[i]=
    S$Parameters$epsilon_Stem_Tree*S$Table_Day$Alloc_Stem_Tree[i]
  # Growth respiration = Offer * (1-growth cost coefficient):
  S$Table_Day$Rc_Stem_Tree[i]=
    (1-S$Parameters$epsilon_Stem_Tree)*S$Table_Day$Alloc_Stem_Tree[i]
  # Mortality: No mortality yet for this compartment.
  # If stem mortality has to be set, write it here.


  #### Coarse Roots ####
  # Offer:
  S$Table_Day$Alloc_CR_Tree[i]=
    S$Parameters$lambda_CR_Tree*S$Table_Day$Offer_Total_Tree[i]
  #NPP
  S$Table_Day$NPP_CR_Tree[i]= S$Parameters$epsilon_CR_Tree*
    S$Table_Day$Alloc_CR_Tree[i]
  # Growth respiration:
  S$Table_Day$Rc_CR_Tree[i]= (1-S$Parameters$epsilon_CR_Tree)*
    S$Table_Day$Alloc_CR_Tree[i]
  # Natural mortality
  S$Table_Day$Mact_CR_Tree[i]=
    S$Table_Day$CM_CR_Tree[i-S$Zero_then_One[i]]/S$Parameters$lifespanCR_Tree


  #### Branches ####
  # NB: Served first as Erythrina must regrow branches in priority after pruning
  S$Table_Day$Alloc_Branch_Tree[i]=
    S$Parameters$lambda_BranchWood_Tree*S$Table_Day$Offer_Total_Tree[i]
  #NPP
  S$Table_Day$NPP_Branch_Tree[i]=
    S$Parameters$epsilon_Branch_Tree*S$Table_Day$Alloc_Branch_Tree[i]
  # Growth respiration:
  S$Table_Day$Rc_Branch_Tree[i]=
    (1-S$Parameters$epsilon_Branch_Tree)*S$Table_Day$Alloc_Branch_Tree[i]
  # Natural mortality:
  S$Table_Day$Mact_Branch_Tree[i]=
    S$Table_Day$CM_Branch_Tree[i-S$Zero_then_One[i]]/S$Parameters$lifespanBranch_Tree


  #### Leaves ####
  # Offer:
  S$Table_Day$Alloc_Leaf_Tree[i]=
    S$Parameters$lambda_Leaf_Tree*S$Table_Day$Offer_Total_Tree[i]
  #NPP
  S$Table_Day$NPP_Leaf_Tree[i]=
    S$Parameters$epsilon_Leaf_Tree*S$Table_Day$Alloc_Leaf_Tree[i]
  # Growth respiration:
  S$Table_Day$Rc_Leaf_Tree[i]=
    (1-S$Parameters$epsilon_Leaf_Tree)*S$Table_Day$Alloc_Leaf_Tree[i]

  # Leaf Fall ---------------------------------------------------------------

  if(S$Met_c$DOY[i]%in%S$Parameters$Fall_Period_Tree&S$Table_Day$Plot_Age[i]>1){
    # Phenology (leaf mortality increases in this period) if Leaf_Fall_Tree is TRUE
    S$Table_Day$Mact_Leaf_Tree[i]=
      S$Table_Day$CM_Leaf_Tree[i-S$Zero_then_One[i]]*S$Parameters$Leaf_fall_rate_Tree
  }else{
    # Or just natural litterfall assuming no diseases
    S$Table_Day$Mact_Leaf_Tree[i]=
      S$Table_Day$CM_Leaf_Tree[i-S$Zero_then_One[i]]/S$Parameters$lifespanLeaf_Tree
  }

  #### Fine roots ####
  # Offer
  S$Table_Day$Alloc_FRoot_Tree[i]=
    S$Parameters$lambda_FRoot_Tree*S$Table_Day$Offer_Total_Tree[i]
  # NPP
  S$Table_Day$NPP_FRoot_Tree[i]=
    S$Parameters$epsilon_FRoot_Tree*S$Table_Day$Alloc_FRoot_Tree[i]
  # Growth respiration
  S$Table_Day$Rc_FRoot_Tree[i]=
    (1-S$Parameters$epsilon_FRoot_Tree)*S$Table_Day$Alloc_FRoot_Tree[i]
  # Natural mortality
  S$Table_Day$Mact_FRoot_Tree[i]=
    S$Table_Day$CM_FRoot_Tree[i-S$Zero_then_One[i]]/S$Parameters$lifespanFRoot_Tree


  #### Reserves ####

  # Offer
  S$Table_Day$Alloc_Reserves_Tree[i]=
    S$Parameters$lambda_Reserves_Tree*S$Table_Day$Offer_Total_Tree[i]
  # Allocation
  S$Table_Day$NPP_Reserves_Tree[i]=
    S$Parameters$epsilon_Reserves_Tree*S$Table_Day$Alloc_Reserves_Tree[i]
  # Cost of allocating to reserves
  S$Table_Day$Rc_Reserves_Tree[i]=
    (1-S$Parameters$epsilon_Reserves_Tree)*S$Table_Day$Alloc_Reserves_Tree[i]



  # Pruning -----------------------------------------------------------------

  # NB: several dates of pruning are allowed
  if(S$Table_Day$Plot_Age[i]%in%S$Parameters$Pruning_Age_Tree&&S$Met_c$DOY[i]%in%S$Parameters$date_pruning_Tree){
    # Leaves pruning :
    S$Table_Day$Mprun_Leaf_Tree[i]=
      S$Table_Day$CM_Leaf_Tree[i-S$Zero_then_One[i]]*S$Parameters$pruningIntensity_Tree
    # Total mortality (cannot exceed total leaf dry mass):
    S$Table_Day$Mact_Leaf_Tree[i]=
      min(S$Table_Day$Mact_Leaf_Tree[i] + S$Table_Day$Mprun_Leaf_Tree[i],
          S$Table_Day$CM_Leaf_Tree[i-S$Zero_then_One[i]])

    # Branch pruning:
    S$Table_Day$Mprun_Branch_Tree[i]=
      S$Table_Day$CM_Branch_Tree[i-S$Zero_then_One[i]]*S$Parameters$pruningIntensity_Tree
    S$Table_Day$Mact_Branch_Tree[i]=
      min((S$Table_Day$Mact_Branch_Tree[i]+S$Table_Day$Mprun_Branch_Tree[i]),
          S$Table_Day$CM_Branch_Tree[i-S$Zero_then_One[i]])

    # Effect of pruning on fine roots  (assumed half the leaves mortality, may be wrong):
    # S$Table_Day$Mprun_FRoot_Tree[i]= (S$Table_Day$Mprun_Leaf_Tree[i]*0.5)
    S$Table_Day$Mprun_FRoot_Tree[i]=
      S$Table_Day$CM_FRoot_Tree[i-S$Zero_then_One[i]]*S$Parameters$pruningIntensity_Tree
    S$Table_Day$Mact_FRoot_Tree[i]=
      min(S$Table_Day$Mact_FRoot_Tree[i]+S$Table_Day$Mprun_FRoot_Tree[i],
          S$Table_Day$CM_FRoot_Tree[i-S$Zero_then_One[i]])
  }


  # Thinning ----------------------------------------------------------------

  if(S$Table_Day$TimetoThin_Tree[i]==1){
    # First, reduce stocking by the predefined rate of thining:
    S$Table_Day$Stocking_Tree[i:nrow(S$Table_Day)]=
      S$Table_Day$Stocking_Tree[i-1]*(1-S$Parameters$RateThinning_Tree)
    # Then add mortality (removing) due to thining :
    S$Table_Day$MThinning_Stem_Tree[i]=
      S$Table_Day$CM_Stem_Tree[i-S$Zero_then_One[i]]*S$Parameters$RateThinning_Tree
    S$Table_Day$MThinning_CR_Tree[i]=
      S$Table_Day$CM_CR_Tree[i-S$Zero_then_One[i]]*S$Parameters$RateThinning_Tree
    S$Table_Day$MThinning_Branch_Tree[i]=
      S$Table_Day$CM_Branch_Tree[i-S$Zero_then_One[i]]*S$Parameters$RateThinning_Tree
    S$Table_Day$MThinning_Leaf_Tree[i]=
      S$Table_Day$CM_Leaf_Tree[i-S$Zero_then_One[i]]*S$Parameters$RateThinning_Tree
    S$Table_Day$MThinning_FRoot_Tree[i]=
      S$Table_Day$CM_FRoot_Tree[i-S$Zero_then_One[i]]*S$Parameters$RateThinning_Tree
  }

  # Dry Mass update ---------------------------------------------------------

  S$Table_Day$CM_Leaf_Tree[i]=
    S$Table_Day$CM_Leaf_Tree[i-S$Zero_then_One[i]]+S$Table_Day$NPP_Leaf_Tree[i]-
    S$Table_Day$Mact_Leaf_Tree[i]-S$Table_Day$MThinning_Leaf_Tree[i]

  S$Table_Day$CM_Branch_Tree[i]=
    S$Table_Day$CM_Branch_Tree[i-S$Zero_then_One[i]]+S$Table_Day$NPP_Branch_Tree[i]-
    S$Table_Day$Mact_Branch_Tree[i]-S$Table_Day$MThinning_Branch_Tree[i]

  S$Table_Day$CM_Stem_Tree[i]=
    S$Table_Day$CM_Stem_Tree[i-S$Zero_then_One[i]]+S$Table_Day$NPP_Stem_Tree[i]-
    S$Table_Day$Mact_Stem_Tree[i]-S$Table_Day$MThinning_Stem_Tree[i]

  S$Table_Day$CM_CR_Tree[i]=
    S$Table_Day$CM_CR_Tree[i-S$Zero_then_One[i]]+
    S$Table_Day$NPP_CR_Tree[i]- S$Table_Day$Mact_CR_Tree[i]-
    S$Table_Day$MThinning_CR_Tree[i]

  S$Table_Day$CM_FRoot_Tree[i]=
    S$Table_Day$CM_FRoot_Tree[i-S$Zero_then_One[i]]+
    S$Table_Day$NPP_FRoot_Tree[i]-S$Table_Day$Mact_FRoot_Tree[i]-
    S$Table_Day$MThinning_FRoot_Tree[i]

  S$Table_Day$CM_Reserves_Tree[i]=
    S$Table_Day$CM_Reserves_Tree[i-S$Zero_then_One[i]]+
    S$Table_Day$NPP_Reserves_Tree[i]-S$Table_Day$Consumption_RE_Tree[i]

  ##########################################
  S$Table_Day$DM_Leaf_Tree[i]=
    S$Table_Day$CM_Leaf_Tree[i]/S$Parameters$CContent_Leaf_Tree
  S$Table_Day$DM_Branch_Tree[i]=
    S$Table_Day$CM_Branch_Tree[i]/S$Parameters$CContent_wood_Tree
  S$Table_Day$DM_Stem_Tree[i]=
    S$Table_Day$CM_Stem_Tree[i]/S$Parameters$CContent_wood_Tree
  S$Table_Day$DM_CR_Tree[i]=
    S$Table_Day$CM_CR_Tree[i]/S$Parameters$CContent_wood_Tree
  S$Table_Day$DM_FRoot_Tree[i]=
    S$Table_Day$CM_FRoot_Tree[i]/S$Parameters$CContent_wood_Tree

  S$Table_Day$Rc_Tree[i]=
    S$Table_Day$Rc_CR_Tree[i]+S$Table_Day$Rc_Leaf_Tree[i]+
    S$Table_Day$Rc_Branch_Tree[i]+S$Table_Day$Rc_Stem_Tree[i]+
    S$Table_Day$Rc_FRoot_Tree[i]+S$Table_Day$Rc_Reserves_Tree[i]

  S$Table_Day$Ra_Tree[i]=
    S$Table_Day$Rm_Tree[i]+S$Table_Day$Rc_Tree[i]

  # NPP_Tree
  S$Table_Day$NPP_Tree[i]=
    S$Table_Day$NPP_Stem_Tree[i]+S$Table_Day$NPP_Branch_Tree[i]+
    S$Table_Day$NPP_Leaf_Tree[i]+S$Table_Day$NPP_CR_Tree[i]+
    S$Table_Day$NPP_FRoot_Tree[i]+S$Table_Day$NPP_Reserves_Tree[i]

  # Daily C balance that should be nil every day:
  S$Table_Day$Cbalance_Tree[i]=
    S$Table_Day$Offer_Total_Tree[i]-(S$Table_Day$NPP_Tree[i]+S$Table_Day$Rc_Tree[i])

  S$Table_Day$LAI_Tree[i]= S$Table_Day$DM_Leaf_Tree[i]*(S$Parameters$SLA_Tree/1000)
  # Allometries ------------------------------------------------------------
  S$Parameters$Allometries(S,i)

  S$Table_Day$LAIplot[i]= S$Table_Day$LAIplot[i] + S$Table_Day$LAI_Tree[i]
}

#' @rdname Shade.Tree
#' @export
No_Shade= function(...){

}
