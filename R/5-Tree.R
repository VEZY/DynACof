#' @rdname site
#' @export
Tree= function(){
  list(
    Tree_Species         = "Erythrina poeppigiana",   # Names of the shade Tree species
    Species_ID           = "Erythrina_Aquiares",      # Optionnal species ID
    StockingTree_treeha1 = 7.38,                      # density at planting (trees ha-1). Source: Taugourdeau et al. (2014)
    SLA_Tree             = 17.4,                      # Specific leaf area (m2 kg-1). Source: Van Oijen et al. (2010, I)
    Leaf_fall_rate_Tree  = 0.09,                      # Mortality during leaf fall (fraction of the leaf mass)
    Fall_Period_Tree     = c(1:30),                   # Time period were leaves fall at high rate (DOY)
    ThinThresh           = 0,                         # Low transmittance threshold under wich thinning is triggered (0-1)
    RateThinning_Tree    = 0,                         # How many trees are thinned per thinning event
    date_pruning_Tree    = 213,                       # Date(s) of pruning each year (DOY)
    pruningIntensity_Tree= 0.8,                       # Pruning intensity (% dry mass)
    Pruning_Age_Tree     = 1:20,                      # Ages at which pruning is made (age). Set to NULL if no prunning.
    # k_Dif_Tree           = 0.305,                     # Light extionction coefficient for diffuse light. Now computed by metamodels
    # k_Dir_Tree           = 0.304,                     # Light extionction coefficient for direct light. Now computed by metamodels
    # lue_Tree             = 1.1375,                    # Light-use efficiency (gc MJ-1). Now computed by metamodels
    WoodDensity_kgDMm3   = 250,                       # Wood density. Source: Van Oijen et al (2010, I) + Nygren (1995)
    lambda_Stem_Tree     = 0.18,                      # Allocation coefficient to the stem. Source: Litton (2007)
    lambda_BranchWood_Tree= 0.25,                     # Allocation coefficient to the branches wood. Source: Litton (2007)
    lambda_CR_Tree       = 0.05,                      # Allocation coefficient to the coarse roots. Source: Litton (2007)
    lambda_Leaf_Tree     = 0.24,                      # Allocation coefficient to the Leaves. Source: Litton (2007)
    lambda_FRoot_Tree    = 0.08,                      # Allocation coefficient to the fine roots. Source: Litton (2007)
    lambda_Reserves_Tree = 0.20,                      # Allocation coefficient to the reserves
    kres_max_Tree        = 1.2,                       # Maximum carbon extracted from reserves compared to maintenance respiration (fraction of Rm)
    CContent_Leaf_Tree   = 0.47,                      # Leaf carbon content in gC gDM-1. Source: Masera et al. (2003)
    CContent_wood_Tree   = 0.47,                      # Wood carbon content in gC gDM-1. Source: Masera et al. (2003)
    epsilon_Branch_Tree  = 0.75,                      # Branch growth cost coefficient (gC.gC-1). Source: Litton et al. (2007)
    epsilon_Stem_Tree    = 0.75,                      # Stem growth cost coefficient (gC.gC-1). Source: Litton et al. (2007)
    epsilon_CR_Tree      = 0.75,                      # Coarse root growth cost coefficient (gC.gC-1). Source: Litton et al. (2007)
    epsilon_Leaf_Tree    = 1/1.392,                   # Leaf growth cost coefficient (gC.gC-1). Source: Erythrina excelsa Villar and Merino (2001),
    epsilon_FRoot_Tree   = 1/1.392,                   # Leaf growth cost coefficient (gC.gC-1). Considered = to leaves
    epsilon_Reserves_Tree= 1,                         # Reserves growth cost coefficient (gC.gC-1). No cost, unknown.
    lifespanBranch_Tree  = 7300,                      # Branch lifespan, natural mortality (d)
    lifespanLeaf_Tree    = 90,                        # Leaf lifespan (d). Source: Van Oijen et al. (2010,I);
    lifespanFRoot_Tree   = 90,                        # Fine roots lifespan (d).
    lifespanCR_Tree      = 7300,                      # Coarse roots lifespan (d). Source: Van Oijen et al. (2010,I)
    Kh                   = 0.7,                       # Allometries, source: CAF2007, Van Oijen et al. (2010). Adjusted to fit our observations.
    KhExp                = 0.5,                       # Allometries, source: CAF2007, Van Oijen et al. (2010). Adjusted to fit our observations.
    Kc                   = 8,                         # Allometries, source: CAF2007, Van Oijen et al. (2010). Adjusted to fit our observations.
    KcExp                = 0.45,                      # Allometries, source: CAF2007, Van Oijen et al. (2010). Adjusted to fit our observations.
    MRN_Tree             = 0.20,                      # Base maintenance respiration (gC.gN.day-1)
    NContentBranch_Tree  = 0.0084,                    # Branch nitrogen content (gN.gDM-1). Source: Van Oijen et al. (2010,I)
    NContentStem_Tree    = 0.0084,                    # Stem nitrogen content (gN.gDM-1). Source: Van Oijen et al. (2010,I)
    NContentCR_Tree      = 0.0084,                    # Coarse roots nitrogen content (gN.gDM-1). Source: Van Oijen et al. (2010,I)
    NContentLeaf_Tree    = 0.0359,                    # Leaf nitrogen content (gN.gDM-1). Source: average 3.35 to 3.82%, Van Oijen et al. (2010,I)
    NContentFRoot_Tree   = 0.0084,                    # Fine root nitrogen content (gN.gDM-1). Taken = to leaves
    Q10Branch_Tree       = 2.1,                       # Branch Q10 (-)
    Q10Stem_Tree         = 1.7,                       # Stem Q10 (-)
    Q10CR_Tree           = 2.1,                       # Coarse root Q10 (-)
    Q10Leaf_Tree         = 1.896,                     # Leaf Q10 (-), see 1-DATA/Erythrina/Respiration.docx
    Q10FRoot_Tree        = 1.4,                       # Fine root Q10 (-). Source: Van Oijen et al (2010,I)
    PaliveBranch_Tree    = 1/3,                       # Branch living tissue (fraction)
    PaliveStem_0_Tree    = 1,                         # Stem living tissue at age 0 (fraction)
    PaliveStem_End_Tree  = 0.05,                      # Stem living tissue at end of rotation (fraction)
    PaliveStem_rate_Tree = 5,                         # Rate of decreasing stem living tissue
    PaliveStem_AgeMax_Tree= 40,                       # Age at which PaliveStem_End_Tree refers (age)
    PaliveStem_Tree      =                            # Computation of living tissue at each age (do not modify)
      data.frame(Age= 1:PaliveStem_AgeMax_Tree,
                 PaliveStem_Tree=
                   PaliveStem_End_Tree+
                   ((PaliveStem_0_Tree-PaliveStem_End_Tree)*
                      exp(seq(0,-PaliveStem_rate_Tree,
                              length.out = PaliveStem_AgeMax_Tree)))),
    PaliveCR_Tree        = 0.21,                       # Coarse roots living tissue (fraction)
    PaliveLeaf_Tree      = 1,                          # Leaf living tissue (fraction)
    PaliveFRoot_Tree     = 1,                          # Fine root living tissue (fraction)

    # Computation of the light extinction coefficient using MAESPA metamodels:
    k= function(S,i){
      # See MAESPA_Validation project, script 4-Aquiares_Metamodels.R
      # Source for non-constant k: Sinoquet et al. 2007
      # DOI: 10.1111/j.1469-8137.2007.02088.x
      S$Table_Day$K_Dif_Tree[i]= 0.6161 - 0.5354*S$Table_Day$LAD_Tree[i-S$Zero_then_One[i]]
      S$Table_Day$K_Dir_Tree[i]= 0.4721 - 0.3973*S$Table_Day$LAD_Tree[i-S$Zero_then_One[i]]
    },

    # Computation of the lue, transpiration and sensible heat flux using MAESPA metamodels:
    Metamodels= function(S,i){
      S$Table_Day$lue_Tree[i]= 2.59906 + 0.10707*S$Met_c$AirTemp_C[i] -
        0.02552*S$Met_c$VPD_hPa[i] + 3.86372*(1-S$Met_c$DiffuseFrSpitters[i]) -
        0.34895*S$Met_c$PAR_MJ[i]

      S$Table_Day$T_Tree_mmd[i]=
        0.021820*S$Met_c$VPD_hPa[i] - 0.016112*S$Met_c$AirTemp_C[i] + 0.942021*S$Table_Day$APAR_Tree[i]-
        1.397349*(1-S$Met_c$DiffuseFrSpitters[i]) + 0.004328*S$Table_Day$LAI_Tree[i]
      S$Table_Day$T_Tree_mmd[i][S$Table_Day$T_Tree_mmd[i]<0]= 0 #to discard negative values

      S$Table_Day$H_Tree[i]=
        0.34975 + 0.81448*S$Table_Day$APAR_Dir_Tree[i] + 0.29321*S$Table_Day$APAR_Dif_Tree[i]-
        0.75987*S$Table_Day$LAI_Tree[i] - 0.55724*S$Table_Day$T_Tree_mmd[i] -
        0.02898*S$Met_c$VPD_hPa[i]
    },

    # Allometries equations (any kind of variable can be added here). Called by Shade.Tree() function.
    # Should output at least DBH_Tree (for LUE), CrownProj_Tree, Crown_H_Tree_m
    Allometries= function(S,i){
      S$Table_Day$DBH_Tree[i]=
        ((S$Table_Day$DM_Stem_Tree[i]/
            (S$Parameters$CContent_wood_Tree*1000*S$Table_Day$Stocking_Tree[i])/0.5)^0.625)/100
      # Source: Rojas-García et al. (2015) DOI: 10.1007/s13595-015-0456-y
      # /!\ DBH is an average DBH among trees.
      #Tree Height. Source:  CAF2007 used in Van Oijen et al. (2011). With no pruning :
      S$Table_Day$Height_Tree[i]=
        round(S$Parameters$Kh*(((S$Table_Day$DM_Stem_Tree[i]/1000)/
                                  S$Table_Day$Stocking_Tree[i])^S$Parameters$KhExp),2)

      # Crown projected area:
      S$Table_Day$CrownProj_Tree[i]=
        round(S$Parameters$Kc*(((S$Table_Day$DM_Branch_Tree[i]/1000)/
                                  S$Table_Day$Stocking_Tree[i])^S$Parameters$KcExp),2)
      # Source: Van Oijen et al. (2010, I).
      S$Table_Day$CrownRad_Tree[i]= sqrt(S$Table_Day$CrownProj_Tree[i]/pi)
      S$Table_Day$Crown_H_Tree[i]= S$Table_Day$CrownRad_Tree[i] # See Charbonnier et al. 2013, Table 2.
      S$Table_Day$Trunk_H_Tree[i]= S$Table_Day$Height_Tree[i]-S$Table_Day$Crown_H_Tree[i]

      # If there is a pruning management, change the allometries (mostly derived from Vezy et al. 2018) :
      if(S$Table_Day$Plot_Age[i]%in%S$Parameters$Pruning_Age_Tree){
        # Pruning : trunk height does not depend on trunk dry mass anymore (pruning effect)
        S$Table_Day$Trunk_H_Tree[i]= round(3*(1-exp(-0.2-S$Table_Day$Plot_Age_num[i])),2)
        S$Table_Day$Height_Tree[i]= S$Table_Day$Crown_H_Tree[i]+S$Table_Day$Trunk_H_Tree[i]
        # The equation make it grow fast at the early stages and reach a plateau at the
        # maximum height after ca. few months.
      }else if(any(S$Table_Day$Plot_Age[i]>S$Parameters$Pruning_Age_Tree)){
        # if there were any pruning before, add the trunk
        Lastheight_Trunk=
          round(3*(1-exp(-0.2-S$Parameters$Pruning_Age_Tree[
            tail(which(S$Table_Day$Plot_Age[i]>S$Parameters$Pruning_Age_Tree),1)]+1)),2)
        S$Table_Day$Height_Tree[i]=
          S$Parameters$Kh*(((S$Table_Day$DM_Stem_Tree[i]/1000)/
                              S$Table_Day$Stocking_Tree[i])^S$Parameters$KhExp)+Lastheight_Trunk
        S$Table_Day$Trunk_H_Tree[i]= S$Table_Day$Height_Tree[i]-S$Table_Day$Crown_H_Tree[i]
      }
      S$Table_Day$LA_Tree[i]= S$Table_Day$LAI_Tree[i]/S$Table_Day$Stocking_Tree[i]
      S$Table_Day$LAD_Tree[i]=
        S$Table_Day$LA_Tree[i]/((S$Table_Day$CrownRad_Tree[i]^2)*
                                  (0.5*S$Table_Day$Crown_H_Tree[i])*pi*(4/3))
    }
  )
}
