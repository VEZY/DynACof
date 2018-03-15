#' @rdname site
#' @export
coffee= function(){
  list(
    AgeCoffeeMin      = 1,          # minimum coffee stand age
    AgeCoffeeMax      = 41,         # maximum coffee stand age (start a new rotation after)
    date_pruning      = 74,         # day of year of pruning
    MeanAgePruning    = 5,          # Age of first pruning (year)
    SLA               = 10.97,      # Specific Leaf Area (m-2 kg-1 dry mass)
    PDem_Leaf         = 1.2,        # Leaf carbon demand (gC m-2 d-1)
    LeafPruningRate   = 0.26,       # how much leaves are pruned (ratio)
    k_Dif             = 0.4289,     # Light extinction coefficient for diffuse light (-), computed from MAESPA
    k_Dir             = 0.3579,     # Light extinction coefficient for direct light (-), computed from MAESPA
    kres              = 0.33,       # Maximum fraction of the reserve used daily
    VGS_Start         = 105,        # Day of year for the beginning of the Vegetative Growing Season
    VGS_Stop          = 244,        # Day of year for the end of the Vegetative Growing Season
    MinTT             = 10,         # Minimum temperature threshold (deg C) for degree days computation
    MaxTT             = 40,         # Maximum temperature threshold (deg C) for degree days computation (if any)
    RNL_base          = 91.2,       # Nodes per LAI unit at the reference 20 Celsius degrees following Drinnan & Menzel (1995)
    VF_Flowering      = 5500,       # Very first flowering (dd), source: Rodriguez et al. (2001)
    Budstage1         = 840,        # Bud development stage 1 (2), source: PhD Louise Meylan p.58.
    Budstage2         = 2562,       # Bud development stage 2 (dd)
    a_Budinit         = 0.00287,    # Parameter for bud initiation from Eq. 12 in Rodriguez et al. (2001)
    b_Budinit         = -0.0000041, # Parameter for bud initiation from Eq. 12 in Rodriguez et al. (2001)
    Tffb              = 4000,       # Time of first floral buds (Rodriguez et al., 2001).
    a_p               = 5.78,       # Parameter for bud dormancy break from Rodriguez et al. (2011)
    b_p               = 1.90,       # Parameter for bud dormancy break from Rodriguez et al. (2011)
    RainForBudBreak   = 10,         # Amount of cumulative rainfall to break bud dormancy (mm). Source: Zacharias et al. (2008)
    Max_Bud_Break     = 12,         # Max number of nodes that can break dormancy daily (buds node-1). Source : Rodriguez et al. (2011)
    ageMaturity       = 3,          # Coffee maturity age (Years)
    FruitMaturation   = 2836,       # Fruit maturation duration until stage 5, ripe (dd). Source: Rodriguez et al. (2011) Table 1.
    FruitOverripe     = 3304,       # Duration until fruit stage 5, overripe, in  the  soil (dd). Source: Rodriguez 2011 Table 1
    u_log             = FruitMaturation/2, # Parameters for the logistic fruit growth pattern
    s_log             = 300,        # Idem
    S_a               = 5.3207,     # Sucrose concentration in berries throught time (dd) parameter. Source : Pezzopane et al. (2011).
    S_b               = -28.5561,   # Sucrose concentration in berries throught time parameter
    S_x0              = 190.9721,   # Sucrose concentration in berries throught time parameter, adapt. to Aquiares (95% maturity ~ at 195 dd)
    S_y0              = 3.4980,     # Sucrose concentration in berries throught time parameter
    Optimum_Berry_DM  = 0.246,      # Optimum berry dry mass, without carbohydrate limitation (g dry mass berry-1). Source: Wintgens book + Vaast et al. (2005)
    kscale_Fruit      = 0.05,       # Empiricall coefficient for the exponential fruit growth
    FtS               = 0.63,       # Fruit to seed ratio (g g-1). Source: Wintgens
    lambdaRsWood      = 0.08,       # Allocation coefficient to resprout wood
    lambdaSCR0        = 0.02,       # Allocation coefficient to stump and coarse roots at age 0.
    lambdaSCR40       = 0.045,      # Allocation coefficient to stump and coarse roots at age 40.
    lambdaLeaf_remain = 0.94,       # Allocation coefficient to allocate the remaining carbon to leaves and fine roots
    lambdaFRoot_remain= 0.06,       # Idem, remain carbon: (1-lambdaRsWood-lambdaSCR-Fruit_Allocation)
    lifespanLeaf      = 265,        # Leaf life span. Source: Charbonnier et al. (2017)
    lifespanRsWood    = 8000,       # Resprout wood life span. Source: Charbonnier et al. (2017)
    lifespanSCR       = 14600,      # Stump and coarse roots life span. Source: Charbonnier et al. (2017)
    lifespanFRoot     = 365,        # Fine roots life span. Source: Charbonnier et al. (2017)
    M_RateFRootprun   = 0.001,      # Fine root percentage that die at pruning
    CContent_Fruit    = 0.4857,     # Fruit carbon content (gC g-1 dry mass)
    CContent_Leaf     = 0.463,      # Leaf carbon content (gC g-1 dry mass)
    CContent_RsWood   = 0.463,      # Resprout wood carbon content (gC g-1 dry mass)
    CContent_SCR      = 0.475,      # Stump and coarse root carbon content (gC g-1 dry mass)
    CContent_FRoots   = 0.463,      # Fine root carbon content (gC g-1 dry mass)
    epsilonFruit      = 0.625,      # Fruit growth respiration coefficient (g g-1), computed using : http://www.science.poorter.eu/1994_Poorter_C&Nrelations.pdf :
    epsilonLeaf       = 0.782,      # Leaf growth respiration coefficient (g g-1)
    epsilonRsWood     = 0.83,       # Resprout wood growth respiration coefficient (g g-1). Source: Dufrêne et al. (2005)
    epsilonSCR        = 0.762,      # Stump and coarse root growth respiration coefficient (g g-1).
    epsilonFRoot      = 0.782,      # Stump and coarse root growth respiration coefficient (g g-1).
    NContentFruit     = 0.011,      # Fruit nitrogen content (gN gDM-1). Source: Van Oijen et al. (2010) (1.1% of DM)
    NContentLeaf      = 0.0296,     # Leaf nitrogen content (gN gDM-1). Source: Ghini et al. (2015), 28.2 to 30.9 g kg−1 DW
    NContentRsWood    = 0.0041,     # Resprout wood nitrogen content (gN gDM-1). Source: Ghini et al. (2015), 28.2 to 30.9 g kg−1 DW
    NContentSCR       = 0.005,      # Stump and coarse root nitrogen content (gN gDM-1).
    NContentFRoot     = 0.018,      # Fine root nitrogen content (gN gDM-1).
    Q10Fruit          = 2.4,        # Fruit Q10, computed from whole plant chamber measurements (Charbonnier 2013), (-)
    Q10Leaf           = 2.4,        # Leaf Q10 (-)
    Q10RsWood         = 2.4,        # Resprout wood Q10 (-)
    Q10SCR            = 1.65,       # Stump and coarse root Q10 (-). Source: Van Oijen et al. (2010)
    Q10FRoot          = 1.65,       # Fine root Q10 (-). Source: Van Oijen et al. (2010)
    TMR               = 15,         # Base temperature for maintenance respiration (deg C)
    MRN               =             # Base maintenance respiration (gC gN-1 d-1). Source: Ryan (1991)
      ((0.00055*12*12)+
         (0.00055*0.6*12*12))/2,
    # MRN: transformed in gDM gN-1 d-1 in the model using Ccontent of each organ.
    # Accounting for 40% reduction during daytime (*1+ during night, *0.6 during daylight)
    PaliveFruit       = 1,          # Fruit living tissue (fraction)
    PaliveLeaf        = 1,          # Leaf living tissue (fraction)
    PaliveRsWood      = 0.37,       # Resprout wood living tissue (fraction)
    PaliveSCR         = 0.21,       # Stump and coarse root living tissue (fraction)
    PaliveFRoot       = 1,          # Fine root living tissue (fraction)
    Opti_C_DemandFruit= Optimum_Berry_DM*CContent_Fruit+Optimum_Berry_DM*CContent_Fruit*(1-epsilonFruit),
    # Opti_C_DemandFruit stands for the optimum demand in total carbon for each berry (including growth respiration)

    # Parameters for American Leaf Spot
    SlopeAzimut       = 180,        # site slope azimuth (deg)
    Slope             = 5,          # Percentage slope (%)
    RowDistance       = 1.5,        # Coffee inter-row distance
    Shade             = 0.25,       # Shade percentage see in Anna Deffner
    Fertilization     = 3,          # Number of fertilizations per year
    ShadeType         = 1,          # Shade type:
    # 1 Legume only; 2	bananas and legume only;3	bananas and other plants;
    # 4	fruit and forest tree only; 5	no shade
    CoffeePruning= "tree"           # Coffee pruning management type:
    # tree ; row ; 3 by block ; 4 NULL (no pruning)
  )
}
