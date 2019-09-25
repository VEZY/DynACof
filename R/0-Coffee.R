#' @rdname site
#' @export
coffee= function(){
  list(
    Stocking_Coffee   = 5580,       # Coffee density at planting (plant ha-1)
    AgeCoffeeMin      = 1,          # minimum coffee stand age
    AgeCoffeeMax      = 40,         # maximum coffee stand age (start a new rotation after)
    SLA               = 10.97,      # Specific Leaf Area (m-2 kg-1 dry mass)
    wleaf             = 0.068,      # Leaf width (m)
    DELM              = 7,          # Max leaf carbon demand (gC plant-1 d-1)
    LAI_max           = 6,          # Max measured LAI to compute leaf demand. (measured= 5.56)
    Height_Coffee     = 2,          # Average coffee canopy height (m), used for aerodynamic conductance.
    D_pruning         = 74,         # day of year of pruning
    MeanAgePruning    = 5,          # Age of first pruning (year)
    LeafPruningRate   = 0.6,        # how much leaves are pruned (ratio)
    WoodPruningRate   = 1/3,        # how much branches wood are pruned (ratio)
    k_Dif             = 0.4289,     # Light extinction coefficient for diffuse light (-), computed from MAESPA
    k_Dir             = 0.3579,     # Light extinction coefficient for direct light (-), computed from MAESPA
    kres              = 0.08,       # Maximum carbon proportion extracted from reserves mass per day
    DVG1              = 105,        # Day of year for the beginning of the Vegetative Growing Season
    DVG2              = 244,        # Day of year for the end of the Vegetative Growing Season
    MinTT             = 10,         # Minimum temperature threshold (deg C) for degree days computation
    MaxTT             = 40,         # Maximum temperature threshold (deg C) for degree days computation (if any)
    RNL_base          = 91.2,       # Nodes per LAI unit at the reference 20 Celsius degrees following Drinnan & Menzel (1995)
    VF_Flowering      = 5500,       # Very first flowering (dd), source: Rodriguez et al. (2001)
    F_buds1           = 840,        # Bud development stage 1 (2), source: PhD Louise Meylan p.58.
    F_buds2           = 2562,       # Bud development stage 2 (dd)
    a_bud             = 0.004,      # Parameter for bud initiation from Eq. 12 in Rodriguez et al. (2001)
    b_bud             = -0.0000041, # Parameter for bud initiation from Eq. 12 in Rodriguez et al. (2001)
    F_Tffb            = 4000,       # Time of first floral buds (Rodriguez et al., 2001).
    a_p               = 5.78,       # Parameter for bud dormancy break from Rodriguez et al. (2011)
    b_p               = 1.90,       # Parameter for bud dormancy break from Rodriguez et al. (2011)
    F_rain            = 40,         # Amount of cumulative rainfall to break bud dormancy (mm). Source: 20 mm Zacharias et al. (2008)
    Max_Bud_Break     = 12,         # Max number of nodes that can break dormancy daily (buds node-1). Source : Rodriguez et al. (2011)
    ageMaturity       = 3,          # Coffee maturity age (Years)
    BudInitEnd        = 100,        # End of bud initiation period relative to first potential bud break of the year (dd).
    # FruitMaturation   = 2836,       # Fruit maturation duration until stage 5, ripe (dd). Source: Rodriguez et al. (2011) Table 1.
    F_over            = 3304,       # Duration until fruit stage 5, overripe, in  the  soil (dd). Source: Rodriguez 2011 Table 1
    u_log             = 1418,       # Parameters for the logistic fruit growth pattern (FruitMaturation/2)
    s_log             = 300,        # Idem
    S_a               = 5.3207,     # Sucrose concentration in berries throught time (dd) parameter. Source : Pezzopane et al. (2011).
    S_b               = -28.5561,   # Sucrose concentration in berries throught time parameter
    S_x0              = 190.9721,   # Sucrose concentration in berries throught time parameter, adapt. to Aquiares (95% maturity ~ at 195 dd)
    S_y0              = 3.4980,     # Sucrose concentration in berries throught time parameter
    Optimum_Berry_DM  = 0.246,      # Optimum berry dry mass, without carbohydrate limitation (g dry mass berry-1). Source: Wintgens book + Vaast et al. (2005)
    kscale_Fruit      = 0.05,       # Empirical coefficient for the exponential fruit growth
    harvest           = "quantity", # Harvest condition: "quality"  -> harvest when most fruits are mature is reached (optimize fruit quality)
    #                    "quantity" -> harvest when fruit dry mass is at maximum.
    # NB: "quality" requires a well-set maturation module.
    Min_Fruit_CM      = 20,         # Minimum fruit carbon mass below which harvest cannot be triggered
    FtS               = 0.63,       # Fruit to seed ratio (g g-1). Source: Wintgens
    lambda_Shoot      = 0.14,       # Allocation coefficient to resprout wood
    lambda_SCR        = 0.075,      # Allocation coefficient to stump and coarse roots.
    lambda_Leaf_remain= 0.85,       # Allocation coefficient to allocate the remaining carbon to leaves and fine roots
    lambda_FRoot_remain= 0.15,      # Idem, remain carbon: (1-lambda_Shoot-lambda_SCR-Fruit_Allocation)
    lifespan_Leaf     = 265,        # Leaf life span. Source: Charbonnier et al. (2017)
    lifespan_Shoot    = 7300,       # Resprout wood life span. Source: Van Oijen et al (2010 I)
    lifespan_SCR      = 7300,       # Stump and coarse roots life span. Source: Charbonnier et al. (2017)
    lifespan_FRoot    = 365,        # Fine roots life span. Source: Van Oijen et al (2010 I)
    m_FRoot           = 0.05,       # Fine root percentage that die at pruning
    CC_Fruit          = 0.4857,     # Fruit carbon content (gC g-1 dry mass)
    CC_Leaf           = 0.463,      # Leaf carbon content (gC g-1 dry mass)
    CC_Shoot          = 0.463,      # Resprout wood carbon content (gC g-1 dry mass)
    CC_SCR            = 0.475,      # Stump and coarse root carbon content (gC g-1 dry mass)
    CC_FRoots         = 0.463,      # Fine root carbon content (gC g-1 dry mass)
    epsilon_Fruit     = 1.6,        # Fruit growth respiration coefficient (g g-1), computed using : http://www.science.poorter.eu/1994_Poorter_C&Nrelations.pdf :
    epsilon_Leaf      = 1.279,      # Leaf growth respiration coefficient (g g-1)
    epsilon_Shoot     = 1.20,       # Resprout wood growth respiration coefficient (g g-1). Source: Dufrêne et al. (2005)
    epsilon_SCR       = 1.31,       # Stump and coarse root growth respiration coefficient (g g-1).
    epsilon_FRoot     = 1.279,      # Fine root growth respiration coefficient (g g-1).
    NC_Fruit          = 0.011,      # Fruit nitrogen content (gN gDM-1). Source: Van Oijen et al. (2010) (1.1% of DM)
    NC_Leaf           = 0.0296,     # Leaf nitrogen content (gN gDM-1). Source: Ghini et al. (2015), 28.2 to 30.9 g kg−1 DW
    NC_Shoot          = 0.0041,     # Resprout wood nitrogen content (gN gDM-1). Source: Ghini et al. (2015), 28.2 to 30.9 g kg−1 DW
    NC_SCR            = 0.005,      # Stump and coarse root nitrogen content (gN gDM-1).
    NC_FRoot          = 0.018,      # Fine root nitrogen content (gN gDM-1).
    Q10_Fruit         = 2.4,        # Fruit Q10, computed from whole plant chamber measurements (Charbonnier 2013), (-)
    Q10_Leaf          = 2.4,        # Leaf Q10 (-)
    Q10_Shoot         = 2.4,        # Resprout wood Q10 (-)
    Q10_SCR           = 1.65,       # Stump and coarse root Q10 (-). Source: Van Oijen et al. (2010)
    Q10_FRoot         = 1.65,       # Fine root Q10 (-). Source: Van Oijen et al. (2010)
    TMR               = 15,         # Base temperature for maintenance respiration (deg C)
    MRN               =             # Base maintenance respiration (gC gN-1 d-1). Computed from Ryan (1991)
      ((0.00055*12*12)+
         (0.00055*0.6*12*12))/2,
    # MRN: transformed in gDM gN-1 d-1 in the model using CC of each organ.
    # Accounting for 40% reduction during daytime (*1+ during night, *0.6 during daylight)
    pa_Fruit          = 1,          # Fruit living tissue (fraction)
    pa_Leaf           = 1,          # Leaf living tissue (fraction)
    pa_Shoot          = 0.37,       # Resprout wood living tissue (fraction)
    pa_SCR            = 0.21,       # Stump and coarse root living tissue (fraction)
    pa_FRoot          = 1,          # Fine root living tissue (fraction)
    DE_opt            = 0.164,      # optimum demand in total carbon for each berry (including growth respiration)
    # = Optimum_Berry_DM*CC_Fruit+Optimum_Berry_DM*CC_Fruit*(1-epsilonFruit),
    Bud_T_correction= CB, # must be a function to predict the temperature-dependent coefficient giving the mean T in input
    # Parameters for American Leaf Spot
    SlopeAzimut       = 180,        # site slope azimuth (deg)
    Slope             = 5,          # Percentage slope (%)
    RowDistance       = 1.5,        # Coffee inter-row distance
    Shade             = 0.25,       # Shade percentage see in Anna Deffner
    Fertilization     = 3,          # Number of fertilizations per year
    ShadeType         = 1,          # Shade type:
    # 1 Legume only; 2	bananas and legume only;3	bananas and other plants;
    # 4	fruit and forest tree only; 5	no shade
    CoffeePruning= "tree",           # Coffee pruning management type:
    # tree ; row ; 3 by block ; 4 NULL (no pruning)
    KTOT         = 800.0,      # soil to leaf hydrolic conducance (mol m-2 s-1 MPa-1)
    # Metamodels (or subroutines):

    # Leaf Water Potential:
    LeafWaterPotential= function(S,i){
      0.040730 - 0.005074*S$Met_c$VPD[i] - 0.037518*S$Sim$PAR_Trans_Tree[i] +
        2.676284*S$Sim$SoilWaterPot[previous_i(i,1)]
    },
    # Transpiration:
    T_Coffee= function(S,i){
      T_Coffee=
        -0.72080 + 0.07319*S$Met_c$VPD[i] -0.76984*(1-S$Met_c$FDiff[i]) +
        0.13646*S$Sim$LAI[i] + 0.12910*S$Sim$PAR_Trans_Tree[i]
      T_Coffee[T_Coffee<0]= 0
      T_Coffee
    },
    # Sensible heat flux:
    H_Coffee= function(S,i){
      1.2560 - 0.2886*S$Met_c$VPD[i] - 3.6280*S$Met_c$FDiff[i] +
        2.6480*S$Sim$T_Coffee[i] + 0.4389*S$Sim$PAR_Trans_Tree[i]
    },
    # Light use efficiency:
    lue= function(S,i){
      2.784288 + 0.009667*S$Met_c$Tair[i] + 0.010561*S$Met_c$VPD[i] -
        0.710361*sqrt(S$Sim$PAR_Trans_Tree[i])
    }
  )
}
