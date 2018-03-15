#' @rdname site
#' @export
Tree= function(){
  list(
    Tree_Species         = "Erythrina poeppigiana",
    Species_ID           = "Erythrina_Aquiares",
    #Stocking_Tree (trees per hectare):
    StockingTree_treeha1 = 7.38,   # Taugourdeau et al. (2014), Table 3
    # StockingTree_treeha1= 5.2      # Charbonnier et al. (2017)

    SLA_Tree             = 17.4, # 14.8 m2.kg-1, 17.4 in Van Oijen et al. (2010, I)

    # Leaf fall (increase mortality during this period) :
    Leaf_fall_rate_Tree  = 0.09,  # leaf C mass loss per day
    Fall_Period_Tree     = c(1:30),  # period were leaves fall at high rate

    # Thinning (no thinning) :
    ThinThresh           = 0,           # Thinning is triggered whenever Tree transmittance is less than 0.7
    RateThinning_Tree    = 0,    # 40% of the trees are thinned and exported

    # Date(s) of pruning within the year (in day of year)
    # date_pruning_Tree= c(42,213)
    date_pruning_Tree    = 213,

    # Pruning intensity (in % of actual dry mass):
    pruningIntensity_Tree= 0.8,

    # Ages at which pruning is made (set to NULL if no prunning):
    Pruning_Age_Tree     = 1:20,

    # Leaf area dynamic along the year (% of the maximum LA for each DOY)
    # LA_perc_pruned= fread("1-DATA/Erythrina/LA_CATIE_2015.csv", data.table = F)$LA_perc
    # LA_perc_natural= fread("1-DATA/Erythrina/LA_Aquiares_Meas.csv", data.table = F)$LA_perc

    # # Light extinction coefficients (computed from MAESPA):
    # k_Dif_Tree= 0.305 # for diffuse light, to adapt to Eryhtrina
    # k_Dir_Tree= 0.304 # for direct light, to adapt to Eryhtrina

    # lue_Tree= 1.1375 # Light-use efficiency (LUE)
    # Or in Van Oijen et al. (2010): 0.56–0.92 in g C MJ-1 PAR

    #Wood density
    WoodDensity_kgDMm3   = 250, # Van Oijen et al (2010, I) + Nygren (1995)

    # Allocation NPP for each organ:
    # See Van Oijen et al. (2010, I):  Typical coffee AFS in Latin America: shade trees of 15 m: 500 kg DM
    # tree-1: 75% wood, 20% roots, 5% leaves.
    lambdaStem_Tree= 0.18,          # Mean from Litton 2007 Fig.1, ANPPwood #0.13*0.7/2#0.1885417NB OR : I divide by 2 considering half in branches and in stems
    lambdaBranchWood_Tree= 0.25,    # Mean from Litton 2007 Fig.1, ANPPwood #0.13*0.7/2#0.091 NB OR : I divide by 2 considering half in branches and in stems
    lambdaCoarseRoot_Tree= 0.05,    # Mean from Litton 2007 Fig.1, TBCF #(0.106+0.055+0.02)/8*3/2.8#0.2
    lambdaLeaf_Tree= 0.24,          # Mean from Litton 2007 Fig.1, ANPPfoliage#, 0.295*0.90#0.18#0.1
    lambdaFineRoot_Tree= 0.08,      # Mean from Litton 2007 Fig.1, ANPPfoliage# 0.126/8.5*0.45#
    lambdaReserves_Tree= 0.20,

    # Reserves use :
    kres_max_Tree= 1.2,   # maximum gC used by day compared to maintenance respiration (fraction of Rm)

    # Carbon content in gC gDM-1
    CContent_Leaf_Tree= 0.47,   # Masera et al. (2003)
    CContent_wood_Tree= 0.47,  # Masera et al. (2003)
    # CContent_Leaf_Tree= 0.42 # Grace et al. 1987 (found in NYGREN et al. 1994), taken from Pinus radiata...
    # CContent_wood_Tree= 0.438  # Charbonnier et al. (2017), Table S1 (supplementary)
    # CContent_Leaf_Tree= 56.2 # Charbonnier et al. (2017), Table S1 (supplementary)
    # NB : E.poeppigiana carbon contents from Charbonnier et al. (2017) are wrong (from Oelbermann et al. (2005),table 1),
    # but the information was misread (% of C in leaves compared to branches)

    # Growth cost coefficient (gC.gC-1)
    epsilonBranch_Tree= 0.75,       # Litton et al. (2007)
    epsilonStem_Tree= 0.75,         # Litton et al. (2007)
    epsilonCoarseRoot_Tree= 0.75,   # Litton et al. (2007)
    epsilonLeaf_Tree= 1/1.392,      # from Erythrina excelsa Villar and Merino (2001),
    # DOI: 10.1046/j.1469-8137.2001.00147.x
    epsilonFineRoot_Tree= 1/1.392,  # considered = to leaves
    epsilonReserves_Tree= 1,        # no cost (unknown)

    # Lifespan parameters (natural mortality), in days:
    lifespanBranch_Tree= 7300,      # taken as Cordia
    lifespanLeaf_Tree= 90,        # 81 +/- 15, Van Oijen et al. (2010,I);
    lifespanFineRoot_Tree= 90,
    lifespanCoarseRoot_Tree= 7300, # Van Oijen et al. (2010, I), mean life span = 10 y,

    # Allometries, source: CAF2007, Van Oijen et al. (2010)
    Kh= 0.7,          # Modified to fit our observations
    KhExp= 0.5,     # Modified to fit our observations
    # Kc= 15.8
    # KcExp= 0.55
    Kc= 8,
    KcExp= 0.45,


    # Global maintenance respiration:
    MRN_Tree= 0.20,             # gC.gN.day-1, see 1-DATA/Erythrina/Respiration.docx

    # Branch maintenance respiration:
    PaliveBranch_Tree= 1/3,         # % of living cells in organ
    NContentBranch_Tree= 0.0084,    # gN.gDM-1 see Van Oijen et al. (2010,I)
    Q10Branch_Tree= 2.1,         # Unitless

    # Stem maintenance respiration:
    PaliveStem_0_Tree= 1,       # % of living cells at age 0
    PaliveStem_End_Tree= 0.05,  # % of living cells at maximum age
    PaliveStem_rate_Tree= 5,
    PaliveStem_AgeMax_Tree= 40, # Age at which PaliveStem_End_Tree refers
    PaliveStem_Tree=
      data.frame(Age= 1:PaliveStem_AgeMax_Tree,
                 PaliveStem_Tree=
                   PaliveStem_End_Tree+
                   ((PaliveStem_0_Tree-PaliveStem_End_Tree)*
                      exp(seq(0,-PaliveStem_rate_Tree,
                              length.out = PaliveStem_AgeMax_Tree)))),

    NContentStem_Tree= 0.0084,  # gN.gDM-1 see Van Oijen et al. (2010,I)
    Q10Stem_Tree= 1.7,          # Unitless

    # Coarse roots maintenance respiration:
    PaliveCoarseRoot_Tree= 0.21,     # %
    NContentCoarseRoot_Tree= 0.0084, # Van Oijen et al. (2010,I)
    Q10CoarseRoot_Tree= 2.1,            # Unitless

    # Leaves maintenance respiration :
    PaliveLeaf_Tree= 1,         # %
    NContentLeaf_Tree= 0.0359,  # gN.gDM-1, average 3.35 to 3.82% Van Oijen et al. (2010,I)
    Q10Leaf_Tree= 1.896,        # see 1-DATA/Erythrina/Respiration.docx

    # Fine roots maintenance respiration:
    PaliveFineRoot_Tree= 1,         # %
    NContentFineRoot_Tree= 0.0084,  # = to leaves
    Q10FineRoot_Tree= 1.4,          # Van Oijen et al (2010,I)

    # Metamodels:

    k= function(S,i){
      # Metamodels for K.
      # See MAESPA_Validation project, script 4-Aquiares_Metamodels.R
      # Source for non-constant k: Sinoquet et al. 2007
      # DOI: 10.1111/j.1469-8137.2007.02088.x
      S$Table_Day$K_Dif_Tree[i]= 0.6161 - 0.5354*S$Table_Day$LAD_Tree[i-S$Zero_then_One[i]]
      S$Table_Day$K_Dir_Tree[i]= 0.4721 - 0.3973*S$Table_Day$LAD_Tree[i-S$Zero_then_One[i]]
      # S$Table_Day$K_Dif_Tree[i]= max(0,min(0.6161 - 0.5354*S$Table_Day$LAD_Tree[i-S$Zero_then_One[i]],0.54))
      # S$Table_Day$K_Dir_Tree[i]= max(0,min(0.4721 - 0.3973*S$Table_Day$LAD_Tree[i-S$Zero_then_One[i]],0.63))
    },
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

    # Allometries function:

    Allometries= function(S,i){
      # Different allometries used in the model. We can add any variable here.
      # Called in Shade.Tree() function.
      # Should output at least DBH_Tree (for LUE), CrownProj_Tree, Crown_H_Tree_m

      ########## Derived variables for Erythrina
      # DBH:
      S$Table_Day$DBH_Tree[i]=
        ((S$Table_Day$DM_Stem_Tree[i]/
            (S$Parameters$CContent_wood_Tree*1000*S$Table_Day$Stocking_Tree[i])/0.5)^0.625)/100
      # Source: Rojas-García et al. (2015) DOI: 10.1007/s13595-015-0456-y
      # /!\ DBH is an average DBH among trees.

      #Tree Height
      # Source:  CAF2007 used in Van Oijen et al. (2011). With no pruning :
      S$Table_Day$Height_Tree[i]=
        round(S$Parameters$Kh*(((S$Table_Day$DM_Stem_Tree[i]/1000)/
                                  S$Table_Day$Stocking_Tree[i])^S$Parameters$KhExp),2)

      #Crown projected area:
      S$Table_Day$CrownProj_Tree[i]=
        round(S$Parameters$Kc*(((S$Table_Day$DM_Branch_Tree[i]/1000)/
                                  S$Table_Day$Stocking_Tree[i])^S$Parameters$KcExp),2)
      # Source: Van Oijen et al. (2010, I).
      S$Table_Day$CrownRad_Tree[i]= sqrt(S$Table_Day$CrownProj_Tree[i]/pi)
      S$Table_Day$Crown_H_Tree[i]= S$Table_Day$CrownRad_Tree[i] # See Charbonnier et al. 2013, Table 2.
      S$Table_Day$Trunk_H_Tree[i]= S$Table_Day$Height_Tree[i]-S$Table_Day$Crown_H_Tree[i]

      # If there is a pruning management, modify the allometries (mostly derived from Vezy et al. 2018) :
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
