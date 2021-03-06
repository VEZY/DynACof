
#' Shade Tree subroutine
#'
#' @description Make all computations for shade trees (similar to coffee, but no fruits) for the ith
#'              day by modifying the `S` list in place.
#'
#' @param S The simulation list of class "Simulation".
#' @param i The index of the day since the first day of the simulation.
#'
#' @return Nothing, modify the list of simulation `S` in place. See [DynACof()] for
#'         more details.
#'
#' @note This function shouldn't be called by the user. It is made as a subroutine so it is easier for
#'       advanced users to modify the code.
#'       `No_Shade()` is used as an empty function that is called when there are no shade trees.
#'
#' @aliases No_Shade
#'
#' @keywords internal
#'
#' @importFrom utils tail
#'
#' @seealso [DynACof()]
#'
#' @export
tree_model= function(S,i){
  # Shade tree layer computations (common for all species)
  # Should output at least APAR_Tree, LAI_Tree, T_Tree, Rn_Tree, H_Tree,
  # LE_Tree (sum of transpiration + leaf evap)
  # And via allometries: Height_Tree for canopy boundary layer conductance
  n_i= length(S$Sim$LAI)

  S$Sim$lue_Tree[i]= S$Parameters$lue_Tree(S,i)

  S$Sim$GPP_Tree[i]= S$Sim$lue_Tree[i]*S$Sim$APAR_Tree[i]

  #Tree Thinning threshold when Transmittance <=S$Parameters$ThinThresh:
  S$Sim$TimetoThin_Tree[i][
    S$Sim$Transmittance_Tree[i]<S$Parameters$ThinThresh]= TRUE

  # Maintenance respiration -------------------------------------------------

  # Rm is computed at the beginning of the day on the drymass of the previous day.
  S$Sim$Rm_Leaf_Tree[i]=
    S$Parameters$pa_Leaf_Tree*S$Sim$DM_Leaf_Tree[previous_i(i,1)]*
    S$Parameters$MRN_Tree*S$Parameters$NC_Leaf_Tree*S$Parameters$Q10Leaf_Tree^
    ((S$Sim$TairCanopy_Tree[i]-S$Parameters$TMR)/10)

  S$Sim$Rm_CR_Tree[i]=
    S$Parameters$pa_CR_Tree*
    S$Sim$DM_CR_Tree[previous_i(i,1)]*
    S$Parameters$MRN_Tree*S$Parameters$NC_CR_Tree*
    S$Parameters$Q10CR_Tree^(
      (S$Sim$TairCanopy_Tree[i]-S$Parameters$TMR)/10)

  S$Sim$Rm_Branch_Tree[i]=
    S$Parameters$pa_Branch_Tree[S$Sim$Plot_Age[i],2]*
    S$Sim$DM_Branch_Tree[previous_i(i,1)]*
    S$Parameters$MRN_Tree*S$Parameters$NC_Branch_Tree*
    S$Parameters$Q10Branch_Tree^(
      (S$Sim$TairCanopy_Tree[i]-S$Parameters$TMR)/10)

  S$Sim$Rm_Stem_Tree[i]=
    S$Parameters$pa_Stem_Tree[S$Sim$Plot_Age[i],2]*
    S$Sim$DM_Stem_Tree[previous_i(i,1)]*
    S$Parameters$MRN_Tree*S$Parameters$NC_Stem_Tree*
    S$Parameters$Q10Stem_Tree^(
      (S$Sim$TairCanopy_Tree[i]-S$Parameters$TMR)/10)

  S$Sim$Rm_FRoot_Tree[i]=
    S$Parameters$pa_FRoot_Tree*
    S$Sim$DM_FRoot_Tree[previous_i(i,1)]*
    S$Parameters$MRN*S$Parameters$NC_FRoot_Tree*
    S$Parameters$Q10FRoot_Tree^(
      (S$Sim$TSoil[i]-S$Parameters$TMR)/10)

  S$Sim$Rm_Tree[i]=
    S$Sim$Rm_Leaf_Tree[i]+ S$Sim$Rm_CR_Tree[i]+
    S$Sim$Rm_Branch_Tree[i]+S$Sim$Rm_Stem_Tree[i]+
    S$Sim$Rm_FRoot_Tree[i]


  # Shade Tree Allocation ---------------------------------------------------

  # Potential use of reserves:
  # Reserves are used only if GPP doesn't meet the condition:
  # maintenance respiration * kres_max_Tree.
  # Thus, if GPP is < to Rm*kres_max_Tree, the model take the needed C to meet the Rm,
  # but not more than there is C in the reserves. If the reserve mass are high enough
  # (>Res_max_Tree gC m-2), the model use it whatever the need.
  if(S$Sim$GPP_Tree[i]<(S$Parameters$kres_max_Tree*S$Sim$Rm_Tree[i])|
     S$Sim$CM_RE_Tree[previous_i(i,1)]>S$Parameters$Res_max_Tree){
    S$Sim$Consumption_RE_Tree[i]=
      max(0,min(S$Sim$CM_RE_Tree[previous_i(i,1)],S$Parameters$kres_max_Tree*S$Sim$Rm_Tree[i]))
  }

  S$Sim$Supply_Total_Tree[i]=
    S$Sim$GPP_Tree[i]-S$Sim$Rm_Tree[i]+S$Sim$Consumption_RE_Tree[i]
  # If the supply is negative (Rm>GPP+RE), there is mortality. This mortality is shared between
  # the organs according to their potential carbon allocation (it is a deficit in carbon
  # allocation)

  if(S$Sim$Supply_Total_Tree[i]<0){
    # Make it positive to cumulate in mortality:
    S$Sim$Supply_Total_Tree[i]= -S$Sim$Supply_Total_Tree[i]
    # Allocate carbon deficit to each organ:
    S$Sim$M_Rm_Stem_Tree[i]=
      S$Parameters$lambda_Stem_Tree*S$Sim$Supply_Total_Tree[i]
    S$Sim$M_Rm_CR_Tree[i]=
      S$Parameters$lambda_CR_Tree*S$Sim$Supply_Total_Tree[i]
    S$Sim$M_Rm_Branch_Tree[i]=
      S$Parameters$lambda_Branch_Tree*S$Sim$Supply_Total_Tree[i]
    S$Sim$M_Rm_Leaf_Tree[i]=
      min(S$Parameters$DELM_Tree*S$Sim$Stocking_Tree[i]*
            ((S$Parameters$LAI_max_Tree-S$Sim$LAI_Tree[i])/
               (S$Sim$LAI_Tree[i]+S$Parameters$LAI_max_Tree)),
          S$Parameters$lambda_Leaf_Tree*S$Sim$Supply_Total_Tree[i])
    S$Sim$M_Rm_FRoot_Tree[i]=
      S$Parameters$lambda_FRoot_Tree*S$Sim$Supply_Total_Tree[i]
    S$Sim$M_Rm_RE_Tree[i]=
      S$Sim$Supply_Total_Tree[i]-
      (S$Sim$M_Rm_FRoot_Tree[i]+S$Sim$M_Rm_Leaf_Tree[i]+
         S$Sim$M_Rm_Branch_Tree[i]+S$Sim$M_Rm_CR_Tree[i]+
         S$Sim$M_Rm_Stem_Tree[i])

    if(S$Sim$M_Rm_RE_Tree[i]>(S$Sim$CM_RE_Tree[previous_i(i,1)]-
                              S$Sim$Consumption_RE_Tree[i])){
      # If reserves cannot provide the C deficit, take it from wood mortality:
      C_overdeficit_RE=
        S$Sim$M_Rm_RE_Tree[i]-(S$Sim$CM_RE_Tree[previous_i(i,1)]-
                                 S$Sim$Consumption_RE_Tree[i])
      S$Sim$M_Rm_CR_Tree[i]=
        S$Sim$M_Rm_CR_Tree[i]+
        C_overdeficit_RE*(S$Parameters$lambda_CR_Tree/S$Parameters$Wood_alloc)
      S$Sim$M_Rm_Branch_Tree[i]=
        S$Sim$M_Rm_Branch_Tree[i]+
        C_overdeficit_RE*(S$Parameters$lambda_Branch_Tree/S$Parameters$Wood_alloc)
      S$Sim$M_Rm_Stem_Tree[i]=
        S$Sim$M_Rm_Stem_Tree[i]+
        C_overdeficit_RE*(S$Parameters$lambda_Stem_Tree/S$Parameters$Wood_alloc)
      S$Sim$M_Rm_RE_Tree[i]= S$Sim$M_Rm_RE_Tree[i]-C_overdeficit_RE
    }
    # NB : M_Rm_RE_Tree is regarded as an extra reserve consumption as supply is not met.
    S$Sim$Supply_Total_Tree[i]= 0
  }

  # Allocation to each compartment :
  S$Sim$Alloc_Stem_Tree[i]=
    S$Parameters$lambda_Stem_Tree*S$Sim$Supply_Total_Tree[i]
  S$Sim$Alloc_CR_Tree[i]=
    S$Parameters$lambda_CR_Tree*S$Sim$Supply_Total_Tree[i]
  S$Sim$Alloc_Branch_Tree[i]=
    S$Parameters$lambda_Branch_Tree*S$Sim$Supply_Total_Tree[i]
  S$Sim$Alloc_Leaf_Tree[i]=
    min(S$Parameters$DELM_Tree*S$Sim$Stocking_Tree[i]*
          ((S$Parameters$LAI_max_Tree-S$Sim$LAI_Tree[i])/
             (S$Sim$LAI_Tree[i]+S$Parameters$LAI_max_Tree)),
        S$Parameters$lambda_Leaf_Tree*S$Sim$Supply_Total_Tree[i])
  S$Sim$Alloc_FRoot_Tree[i]=
    S$Parameters$lambda_FRoot_Tree*S$Sim$Supply_Total_Tree[i]
  # Allocation to reserves (Supply - all other allocations):
  S$Sim$Alloc_RE_Tree[i]=
    S$Sim$Supply_Total_Tree[i]-
    (S$Sim$Alloc_FRoot_Tree[i]+S$Sim$Alloc_Leaf_Tree[i]+
       S$Sim$Alloc_Branch_Tree[i]+S$Sim$Alloc_CR_Tree[i]+
       S$Sim$Alloc_Stem_Tree[i])

  # Stem:
  S$Sim$NPP_Stem_Tree[i]= S$Sim$Alloc_Stem_Tree[i]/S$Parameters$epsilon_Stem_Tree
  S$Sim$Rg_Stem_Tree[i]=
    S$Sim$Alloc_Stem_Tree[i]-S$Sim$NPP_Stem_Tree[i]
  # Mortality: No mortality yet for this compartment.
  # If stem mortality has to be set, write it here.

  # Coarse Roots:
  S$Sim$NPP_CR_Tree[i]= S$Sim$Alloc_CR_Tree[i]/S$Parameters$epsilon_CR_Tree
  S$Sim$Rg_CR_Tree[i]= S$Sim$Alloc_CR_Tree[i]-S$Sim$NPP_CR_Tree[i]
  S$Sim$Mact_CR_Tree[i]=
    S$Sim$CM_CR_Tree[previous_i(i,1)]/S$Parameters$lifespan_CR_Tree

  # Branches:
  S$Sim$NPP_Branch_Tree[i]=
    S$Sim$Alloc_Branch_Tree[i]/S$Parameters$epsilon_Branch_Tree
  S$Sim$Rg_Branch_Tree[i]=
    S$Sim$Alloc_Branch_Tree[i]-S$Sim$NPP_Branch_Tree[i]
  S$Sim$Mact_Branch_Tree[i]=
    S$Sim$CM_Branch_Tree[previous_i(i,1)]/S$Parameters$lifespan_Branch_Tree

  # Leaves:
  S$Sim$NPP_Leaf_Tree[i]=
    S$Sim$Alloc_Leaf_Tree[i]/S$Parameters$epsilon_Leaf_Tree
  S$Sim$Rg_Leaf_Tree[i]=
    S$Sim$Alloc_Leaf_Tree[i]-S$Sim$Rg_Leaf_Tree[i]

  # Leaf Fall ---------------------------------------------------------------

  if(S$Sim$TimetoFall_Tree[i]){
    # Phenology (leaf mortality increases in this period) if Leaf_Fall_Tree is TRUE
    S$Sim$Mact_Leaf_Tree[i]=
      S$Sim$CM_Leaf_Tree[previous_i(i,1)]*
      S$Parameters$Leaf_fall_rate_Tree[[
        which(lapply(S$Parameters$Fall_Period_Tree,
                     function(x){match(S$Met_c$DOY[i],x,nomatch = 0)})>0)]]
  }else{
    # Or just natural litterfall assuming no diseases
    S$Sim$Mact_Leaf_Tree[i]=
      S$Sim$CM_Leaf_Tree[previous_i(i,1)]/S$Parameters$lifespan_Leaf_Tree
  }

  # Fine roots
  S$Sim$NPP_FRoot_Tree[i]=
    S$Sim$Alloc_FRoot_Tree[i]/S$Parameters$epsilon_FRoot_Tree
  S$Sim$Rg_FRoot_Tree[i]=
    S$Sim$Alloc_FRoot_Tree[i]-S$Sim$NPP_FRoot_Tree[i]
  S$Sim$Mact_FRoot_Tree[i]=
    S$Sim$CM_FRoot_Tree[previous_i(i,1)]/S$Parameters$lifespan_FRoot_Tree

  # Reserves:
  S$Sim$NPP_RE_Tree[i]=
    S$Sim$Alloc_RE_Tree[i]/S$Parameters$epsilon_RE_Tree
  # Cost of allocating to reserves
  S$Sim$Rg_RE_Tree[i]=
    S$Sim$Alloc_RE_Tree[i]-S$Sim$NPP_RE_Tree[i]

  # Pruning -----------------------------------------------------------------

  # NB: several dates of pruning are allowed
  if(S$Sim$TimetoPrun_Tree[i]){
    # Leaves pruning :
    S$Sim$Mprun_Leaf_Tree[i]=
      S$Sim$CM_Leaf_Tree[previous_i(i,1)]*S$Parameters$pruningIntensity_Tree
    # Total mortality (cannot exceed total leaf dry mass):
    S$Sim$Mact_Leaf_Tree[i]=
      max(0,min(S$Sim$Mact_Leaf_Tree[i] + S$Sim$Mprun_Leaf_Tree[i],
                S$Sim$CM_Leaf_Tree[previous_i(i,1)]))

    # Branch pruning:
    S$Sim$Mprun_Branch_Tree[i]=
      S$Sim$CM_Branch_Tree[previous_i(i,1)]*S$Parameters$pruningIntensity_Tree
    S$Sim$Mact_Branch_Tree[i]=
      max(0,min((S$Sim$Mact_Branch_Tree[i]+S$Sim$Mprun_Branch_Tree[i]),
                S$Sim$CM_Branch_Tree[previous_i(i,1)]))
    S$Sim$Mprun_FRoot_Tree[i]=
      S$Parameters$m_FRoot_Tree*S$Sim$Mprun_Leaf_Tree[i]
    S$Sim$Mact_FRoot_Tree[i]=
      max(0,min(S$Sim$Mact_FRoot_Tree[i]+S$Sim$Mprun_FRoot_Tree[i],
                S$Sim$CM_FRoot_Tree[previous_i(i,1)]))
  }

  # Thinning ----------------------------------------------------------------

  if(S$Sim$TimetoThin_Tree[i]){
    # First, reduce stocking by the predefined rate of thining:
    S$Sim$Stocking_Tree[i:n_i]=
      S$Sim$Stocking_Tree[i-1]*(1-S$Parameters$RateThinning_Tree)
    # Then add mortality (removing) due to thining :
    S$Sim$MThinning_Stem_Tree[i]=
      S$Sim$CM_Stem_Tree[previous_i(i,1)]*S$Parameters$RateThinning_Tree
    S$Sim$MThinning_CR_Tree[i]=
      S$Sim$CM_CR_Tree[previous_i(i,1)]*S$Parameters$RateThinning_Tree
    S$Sim$MThinning_Branch_Tree[i]=
      S$Sim$CM_Branch_Tree[previous_i(i,1)]*S$Parameters$RateThinning_Tree
    S$Sim$MThinning_Leaf_Tree[i]=
      S$Sim$CM_Leaf_Tree[previous_i(i,1)]*S$Parameters$RateThinning_Tree
    S$Sim$MThinning_FRoot_Tree[i]=
      S$Sim$CM_FRoot_Tree[previous_i(i,1)]*S$Parameters$RateThinning_Tree
  }

  # Mortality update --------------------------------------------------------

  S$Sim$Mortality_Leaf_Tree[i]=
    S$Sim$M_Rm_Leaf_Tree[i]+S$Sim$Mact_Leaf_Tree[i]+S$Sim$MThinning_Leaf_Tree[i]
  S$Sim$Mortality_Branch_Tree[i]=
    S$Sim$M_Rm_Branch_Tree[i]+S$Sim$Mact_Branch_Tree[i]+S$Sim$MThinning_Branch_Tree[i]
  S$Sim$Mortality_Stem_Tree[i]=
    S$Sim$M_Rm_Stem_Tree[i]+S$Sim$Mact_Stem_Tree[i]+S$Sim$MThinning_Stem_Tree[i]
  S$Sim$Mortality_CR_Tree[i]=
    S$Sim$M_Rm_CR_Tree[i]+S$Sim$Mact_CR_Tree[i]+S$Sim$MThinning_CR_Tree[i]
  S$Sim$Mortality_FRoot_Tree[i]=
    S$Sim$M_Rm_FRoot_Tree[i]+S$Sim$Mact_FRoot_Tree[i]+
    S$Sim$MThinning_FRoot_Tree[i]

  # C mass update -----------------------------------------------------------

  S$Sim$CM_Leaf_Tree[i]=
    max(0,S$Sim$CM_Leaf_Tree[previous_i(i,1)]+S$Sim$NPP_Leaf_Tree[i]-S$Sim$Mortality_Leaf_Tree[i])

  S$Sim$CM_Branch_Tree[i]=
    max(0,S$Sim$CM_Branch_Tree[previous_i(i,1)]+S$Sim$NPP_Branch_Tree[i]-
          S$Sim$Mortality_Branch_Tree[i])

  S$Sim$CM_Stem_Tree[i]=
    max(0,S$Sim$CM_Stem_Tree[previous_i(i,1)]+S$Sim$NPP_Stem_Tree[i]-
          S$Sim$Mortality_Stem_Tree[i])

  S$Sim$CM_CR_Tree[i]=
    max(0,S$Sim$CM_CR_Tree[previous_i(i,1)]+
          S$Sim$NPP_CR_Tree[i]-S$Sim$Mortality_CR_Tree[i])

  S$Sim$CM_FRoot_Tree[i]=
    max(0,S$Sim$CM_FRoot_Tree[previous_i(i,1)]+
          S$Sim$NPP_FRoot_Tree[i]-S$Sim$Mortality_FRoot_Tree[i])

  S$Sim$CM_RE_Tree[i]=
    max(0,S$Sim$CM_RE_Tree[previous_i(i,1)]+
          S$Sim$NPP_RE_Tree[i]-S$Sim$Consumption_RE_Tree[i]-S$Sim$M_Rm_RE_Tree[i])

  # Dry Mass update ---------------------------------------------------------

  S$Sim$DM_Leaf_Tree[i]=
    S$Sim$CM_Leaf_Tree[i]/S$Parameters$CC_Leaf_Tree
  S$Sim$DM_Branch_Tree[i]=
    S$Sim$CM_Branch_Tree[i]/S$Parameters$CC_wood_Tree
  S$Sim$DM_Stem_Tree[i]=
    S$Sim$CM_Stem_Tree[i]/S$Parameters$CC_wood_Tree
  S$Sim$DM_CR_Tree[i]=
    S$Sim$CM_CR_Tree[i]/S$Parameters$CC_wood_Tree
  S$Sim$DM_FRoot_Tree[i]=
    S$Sim$CM_FRoot_Tree[i]/S$Parameters$CC_wood_Tree

  # Respiration -------------------------------------------------------------

  S$Sim$Rg_Tree[i]=
    S$Sim$Rg_CR_Tree[i]+S$Sim$Rg_Leaf_Tree[i]+
    S$Sim$Rg_Branch_Tree[i]+S$Sim$Rg_Stem_Tree[i]+
    S$Sim$Rg_FRoot_Tree[i]+S$Sim$Rg_RE_Tree[i]

  S$Sim$Ra_Tree[i]=
    S$Sim$Rm_Tree[i]+S$Sim$Rg_Tree[i]

  # Total NPP ---------------------------------------------------------------

  S$Sim$NPP_Tree[i]=
    S$Sim$NPP_Stem_Tree[i]+S$Sim$NPP_Branch_Tree[i]+
    S$Sim$NPP_Leaf_Tree[i]+S$Sim$NPP_CR_Tree[i]+
    S$Sim$NPP_FRoot_Tree[i]+S$Sim$NPP_RE_Tree[i]

  # Daily C balance that should be nil every day:
  S$Sim$Cbalance_Tree[i]=
    S$Sim$Supply_Total_Tree[i]-(S$Sim$NPP_Tree[i]+S$Sim$Rg_Tree[i])

  # Allometries ------------------------------------------------------------
  S$Parameters$Allometries(S,i)

  S$Sim$LAI_Tree[min(i+1,n_i)]= S$Sim$DM_Leaf_Tree[i]*(S$Parameters$SLA_Tree/1000)
  S$Sim$LAIplot[min(i+1,n_i)]= S$Sim$LAIplot[min(i+1,n_i)] + S$Sim$LAI_Tree[min(i+1,n_i)]
  S$Sim$Height_Canopy[min(i+1,n_i)]= max(S$Sim$Height_Tree[i], S$Parameters$Height_Coffee)
}
