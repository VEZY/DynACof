Parameters_f= function(Tree_Species=NULL){
   
    ########## Aquiares Coffee PARAMETERS ####
   
    Parameters= list()
    ###Key Parameters for Management ####
    #Defining the duration of one cycle = rotation
    Parameters$AgeCoffeeMin= 1
    Parameters$AgeCoffeeMax= 41    
    #productivity, 3-4 years after planting (coffee cvs. Tipica and Bourbon) (source: Roskoski et al. 1982 )
    Parameters$date_pruning= 74    #Aquiares date pruning (15 mars).
    Parameters$MeanAgePruning= 1825/365#Aquiares = 5 yrs = age in years of first pruning and of 
    # pruning interval,PhD Louise Melyan tab. 4.4 p.84
    ####Key Parameters for Leaf module ####
    #WARNING !!! Model LAI very sensitive to SLA
    Parameters$SLA_FSun_m2kg= 11.4 #11.4 m2leaf kg-1, SE  =0.4
    Parameters$SLA_Sh_m2kg= 12 # 12 m2leaf kg-1, SE = 0.4
    Parameters$SLA<- 0.95*((1-Parameters$ShadetreeCanopyProj)*Parameters$SLA_FSun_m2kg + 
                               Parameters$ShadetreeCanopyProj*Parameters$SLA_Sh_m2kg)
    # = 10.97
    #11.55 m2leaf kgDM-1 ; Charbonnier et al., NPP paper Tab. 2
    #Van Oijen 2010 SLW = 107-105 gDM m-2 in Ryan et al. 2001 : correponding to SLA = 9.34-9.52 m2 kgDM-1
    #Van Oijen 2010 SLW = 170-207 gDM m-2 (cv. Catuai, 80% shade-fullsun)Herrero et al. 2000 : 
    # correponding to SLA = 5.88-4.83 m2 kgDM-1
    #Cavatte et al., 2012: from 14 in low light to 11 in high light, small reduction also with drought
    #Check for Aquiares
    
    # Parameters$PDem_Leaf= 0.8*1.15*1.05*Parameters$StockingRatio_to_Aquiares
    Parameters$PDem_Leaf= 1.2
    #changes the final average; leaf Demand, NPPmax; sensitive between 0 to 3, then no more sensitive;
    # original value 0.8
    #NB, we have to make it a function of f(T,SW     D,...)# Il faudrait repartir la NPPmax annuelle
    # selon 2 ou 3 flushes
    Parameters$LeafPruningRate= 1/3.85 # the oldest resprouts, i.e. the ones with largest LAI are pruned,
    # thus pruning rate should be quite > to 1/(MeanAgePruning)

    ####Key Parameters for APAR####
    
    #Without metamodel, Thinning tree, adjusment of kcoffee
    #Parameters$k<-0.29*Parameters$StockingRatio_to_Aquiares*2#0.34#0.487 extinction coefficient 
    #With metamodel KCordia, Thinning tree, adjusment of kcoffee
    #Parameters$k<-0.29*Parameters$StockingRatio_to_Aquiares*1.2#0.34#0.487 extinction coefficient, 
    # source : "Manip calibration LAI2000 25 caf?s vu ORbisRyu.xlsx"
    #Je l'ai abaiss? pour coller ? APAR_MAESPA et pour prendre les trous entre caf?iers
    # defined for Aquiares in Script Functions OR 7.R:
    # Parameters$LUE_can_coffeeFs_gCMJ= 1.05 # 1.05 gC MJaPAR-1, Charbonnier et al., GPP paper
    # Parameters$LUE_can_coffeeSh_gCMJ= 1.4#1.4 gC MJaPAR-1, Charbonnier et al., GPP paper
    # Parameters$lue= (1-Parameters$ShadetreeCanopyProj)*Parameters$LUE_can_coffeeFs_gCMJ+
    #     Parameters$ShadetreeCanopyProj*Parameters$LUE_can_coffeeSh_gCMJ
    #LUE  = 1.05 gC MJAPAR-1 in canopy full sun, 1.4 in shade
    #defined by GPP/APAR Charbonnier et al. GPP paper, fig.10
    
    # Light extinction coefficients (computed from MAESPA):
    Parameters$k_Dif= 0.4289 # for diffuse light
    Parameters$k_Dir= 0.3579 # for direct light
    
    
    ####Key Parameters for Reserves####
    Parameters$kres<-1/365*120#reserve coefficient OR taux de reserves utilisable (1# jour-1) OR C'est trop par
    # jour non Pourquoi pas 1/365 ? GlM: Je pense qu'on a mis ca comme un "frein" pour ne pas vider
    # les reserves d'un seul coup quand une des demandes est forte... on a un pourcentage des reserves
    # "utilisables". C'est effectivement >1/365. Mais certains jours il faut avoir acces ? plus de 1/365
    # probablement...
    ####Parameters for Vegetative Growing season (VGS: Summer+Autumn)####
    Parameters$date_VegetGrowSeason_Start<-105# Assuming VGS is between flowering and Bud initiation. Drinnan & Menzel (1995)
    Parameters$date_VegetGrowSeason_Stop<-244# 1rst of September, Assuming VGS is between flowering and Bud initiation.
    # Drinnan & Menzel (1995)
    Parameters$MinTT= 10    # minimum temperature threshold for degree days computation
    # Parameters$MinTT= 11.25    # minimum temperature threshold for degree days computation
    # NB : Minimum at 11.25 in Gutierrez et al. (1998)
    # Parameters$MaxTT= 32    # maximum temperature threshold for degree days computation (if any)
    Parameters$MaxTT= 40    # maximum temperature threshold for degree days computation (if any)
    
    ####Key Parameters for Flower buds####
    Parameters$MaxCohorts= 800 # Maximum cohorts allowed (not important, default value is robust)
    Parameters$ratioNodestoLAI_REF20deg<- 91.2 #upscaled from branch to plant LAI from Fig. 4e in Rodriguez et al.,
    # 2001, standardized at 20?C following Drinnan & Menzal 1995
    # Or : 
    # Parameters$ratioNodestoCm_RsWood_REF20deg= 0.717
    # with S$Parameters$ratioNodestoCm_RsWood_REF20deg*Perc_Nodes_compared_to_20deg_ref and
    # S$Table_Day$LAI[i-1]*S$Table_Day$ratioNodestoRs_Wood[i-1]
    
    # Very first flowering:
    Parameters$VF_Flowering= 5500 # dd, Rodriguez et al. (can be updated to 6047.604 to our case)
    #In Aquiares, March-May, source PhD Louise Meylan p.58. I keep mid-April then
    Parameters$Budstage1= 840       # dd
    Parameters$Budstage2= 2562   # dd
    
    # BudInitiation in number of buds per day
    Parameters$a_Budinit<- 0.00287 #Eq. 12 in Rodriguez et al., 2001
    Parameters$b_Budinit<- -0.0000041 # Eq. 12 in Rodriguez et al., 2001
    # degree days for each following bud initialisation :
    Parameters$Tffb= 4000 # Time of first floral buds (Rodriguez et al., 2001). 
    # Between 3000 and 4800 in Rodriguez et al. (2000 works well also)

    ### Parameters for bud dormancy break, Source, Drinnan 1992 and Rodriguez et al., 2011 eq. 13####
    Parameters$a_p<-5.78 #Rodriguez et al., 2011,Tab.1
    Parameters$b_p<- 1.90# 1.75#Rodriguez et al., 2011, Tab.1, taking average of the Columbian sites
    # Amount of cumulative rain to break the bud dormancy:
    Parameters$RainForBudBreak= 10 # mm Source: Zacharias et al. 2008
    # Zacharias, A., Camargo, M. de, Fazuoli, L., 2008. 
    # Modelo agrometeoroligico de estimativa do inicio da florada plena do cafeeiro. Bragantia 67, 249-256.
    Parameters$Max_Bud_Break= 12 # in number of buds per nodes, source : Rodriguez et al. (2011)
    
    ### Key Parameters for Fruit####
    Parameters$ageMaturity<-3#age de Maturite (Years) = la premiere floraison du brin; Van Oijen 2010 = Time 
    # between planting and first maturity
    Parameters$FruitMaturation= 2836 # in degree days. Source: Rodriguez 2011 Table 1
    # RV: Number of Days taken for fruit stage 4 (ripe= maturity)
    Parameters$FruitOverripe= 3304 # in degree days. Source: Rodriguez 2011 Table 1 
    # RV: Number of Days taken for fruit stage 5 (overripe, in  the  soil)
    # Parameters for the logistic fruit growth pattern: 
    Parameters$u_log= Parameters$FruitMaturation/2
    Parameters$s_log= 300
    
    # Sucrose concentration in berries throught time (days after flowering) parameters:
    Parameters$S_a= 5.3207
    Parameters$S_b= -28.5561
    Parameters$S_x0= 190.9721
    # Parameters$S_x0= 180 # Adapted to Aquiares (95% maturity ~ at 195 dd)
    Parameters$S_y0= 3.4980
    # Source : Pezzopane, J., Salva, T., Lima, V. and Fazuoli, L. 2011. 
    # Agrometeorological parameters for prediction of the maturation period of Arabica coffee cultivars.
    # International journal of biometeorology. 56, 5 (2011), 843-51
    # NB: prefered days after flowering than other indices because coffee beans needs time to mature.
    # Using DAF is then very usefull if we want that when fruits mature rapidly (increased temperature,
    # increased dd) quality doesn't.
    
    Parameters$Optimum_Berry_DM= 0.246 # Source: 0 Synthese Fresh and Dry Weight Coffee Fruit_Wintgens book
    # Parameters$Optimum_Berry_DM= 0.63
    # Parameters$Optimum_Berry_DM= 0.5
    # in, GUTIERREZ et al. (1998) + Arcilla and Jaramillo (2003) it is close to 0.5 gDM berry-1, 
    # 0.63 at the end in Marin et al. (2003)
    # 0.22 gDM in Vaast et al. (2005) : Fruit load and branch ring-barking...
    Parameters$kscale_Fruit= 0.05#coefficient empirique de l'exponentielle de la croissance des fruits
    # Parameters$Fruit_To_Seed_Ratio_DM= 0.21 # see Arcila (2007), Cenicafé.
    Parameters$Fruit_To_Seed_Ratio_DM= 0.63 # Wintgens, see "0 Synthese Fresh and Dry Weight Coffee F
    # ruit_Wintgens book.xlsx"
    # Parameters$Fruit_To_Seed_Ratio_DM<-0.70 # in F. Charbonnier ratio seed_fruit & C_N fruit.xlsx
    
    ### Key Parameters for Allocation NPP FINALE mesures: ####
    #NB: la somme des lambda ne fait pas 1, ce qui signifie que le delta est disponible pour les feuilles+racines fines
    # Source: "Inventario GEI-Beneficio Anexo 4 Aquiares.xlsx"
    # Parameters$lambdaRsWood<-0.091/2.3#0.246 CoffeeFlux 
    Parameters$lambdaRsWood= 0.08 # 0.051
    # 
    #WARNING model sensitive
    # Parameters$lambdaStumpCoarseRoot0= 0.0016
    Parameters$lambdaStumpCoarseRoot0= 0.02
    Parameters$lambdaStumpCoarseRoot40= 0.045
    # lambdas to allocate remaining carbon (1-lambdaRsWood-lambdaStumpCoarseRoot0-Fruit_Allocation) between
    # leaves and fine roots :
    # Parameters$lambdaLeaf_remain= 0.96
    # Parameters$lambdaFineRoot_remain= 0.04
    Parameters$lambdaLeaf_remain= 0.94
    Parameters$lambdaFineRoot_remain= 0.06
    
    ### Lifespan used for Mortality natural (days)####
    #WARNING model sensitive !!!
    Parameters$lifespanLeaf= 265 #265charbonnier et al. NPP paper, tab.6 
    #van oijen 2010: 750 d Marin et al. 2005
    #1200 days : Masera et al. 2003
    #Branch death increases with fruit load (0 ? 100%):7 ? 20% y-1, so mean life span decreases from
    # 14 to 5 years. (Vaast et al. 2005)
    # Parameters$lifespanRsWood<-7*365*4 #assumed little natural mortality 
    Parameters$lifespanRsWood= 8000
    Parameters$lifespanStumpCoarseRoot= 14600
    #van oijen 2010: 7300 d : Aranguren et al. 1982
    # Parameters$lifespanFineRoot<- 365*1.39# Defrenet, table 2
    Parameters$lifespanFineRoot= 365# Defrenet, table 2
    #van oijen 2010:Turn-over roots = 0.07 y-1, so mean life span & 5200 d.: Masera et al. 2003
    Parameters$MortalityRateFineRootprun= 0.001#0.1 #proportion des racines fines qui meurent au moment elagage 
    # biomasse aerienne aller chercher dans Defrenet
    
    ### Carbon content in gC gDM-1####
    Parameters$CContent_Fruit= 0.4857# SD = 0.0037, Whole fruit: source "Fabien ratio seed_fruit & C_N fruit.xlsx"
    # or Charbonnier paper NPP tab. 3
    Parameters$CContent_Leaf= 0.463#leaf: Aurelie Cambou paper or Charbonnier paper NPP tab. 3
    Parameters$CContent_RsWood= 0.463#resprout stem : Aurelie Cambou paper or Charbonnier paper NPP tab. 3
    Parameters$CContent_StumpCoarseRoot= 0.475#leaf : Aurelie Cambou paper or Charbonnier paper NPP tab. 3
    Parameters$CContent_FineRoots= 0.463
    
    ### Growth respiration coefficient 
    Parameters$epsilonFruit= 0.625
    # To compute using : http://www.science.poorter.eu/1994_Poorter_C&Nrelations.pdf :
    # 1.60 gC gC-1 -> 0.625, see : 0 Synthese Fresh and Dry Weight Coffee Fruit_Wintgens book.xlsx data composition
    Parameters$epsilonLeaf= 1/((1.38+1.176)/2) # 1.176 g glucose g−1 DW to 1.3801.	Cavatte et al., (2012)
    # Functional analysis of the relative growth rate, chemical composition, construction and maintenance costs...
    Parameters$epsilonRsWood= 1/1.20 # Dufrêne et al. 2005
    Parameters$epsilonStumpCoarseRoot= 1-(0.113/Parameters$CContent_StumpCoarseRoot)#1-0.237#1/1.25 #ceps+grosses racines
    # Parameters$epsilonFineRoot<-1-(0.113/0.5)#1-0.226 here, 1-0.28 in Dufrene et al., 2005#1/1.25 #racines fines
    Parameters$epsilonFineRoot= 0.7824726  # taken = to leaves
    Parameters$Opti_C_DemandFruit= Parameters$Optimum_Berry_DM*Parameters$CContent_Fruit+
        Parameters$Optimum_Berry_DM*Parameters$CContent_Fruit*(1-Parameters$epsilonFruit)
    # Opti_C_DemandFruit stands for the optimum demand in total carbon for each berry (including growth respiration)
    
    ### N content in gN gDM-1, source Coffee_chimie_PdVries_deWit.xls"
    Parameters$NContentFruit= 0.011 # Source: Van Oijen (1.1% of DM), up to 1.7 on mature fruits
    Parameters$NContentLeaf= 0.02955 
    # 28.2 to 30.9 g kg−1 DW by Ghini et al. (2015): Coffee growth, pest and yield responses to free-air CO2 enrichment
    # So it is equal to 0.02955 gN.gC-1 (quite the same as found in Van Oijen et al. 2010, I), but here we work with 0.0067...
    # In da Silva et al (2005) : 
    # Nitrogen= c(25.1,25.2,22.6,28.8,23.1,19.3,21.3,19.8,23.4,23.9,21.5,22.5)
    # mean(Nitrogen)/1000 # = 0.023
    Parameters$NContentRsWood= 0.0041 
    # Branch nitrogen content 5.50 mgN gdm Ceschia et al. (2002)
    Parameters$NContentStumpCoarseRoot= 0.005 
    # 1.20 mgN gdm Ceschia et al. (2002) 
    Parameters$NContentFineRoot= 0.018
    
    ###Maintenance respiration : 
    Parameters$Q10Fruit= 2.4 # Computed from whole plant chamber measurements (Charbonnier 2013)
    Parameters$Q10Leaf= 2.4 
    Parameters$Q10RsWood= 2.4 
    Parameters$Q10StumpCoarseRoot<- 1.65 # Root system : Van Oijen et al. (2010) average 1.4 to 1.9.
    Parameters$Q10FineRoot<- 1.65 # Root system : Van Oijen et al. (2010)
    Parameters$TMR= 15 # Base temperature for maintenance respiration
    Parameters$MRN= ((0.00055*12*12)+(0.00055*0.6*12*12))/2
    # unit corrected to give gC gN-1 d-1; Nitrogen dependency for all organs mol CO2
    # gN-1 h-1 = 5.5.10-4 Ryan (1991). Transformed in gDM gN-1 d-1 in the model using Ccontent of each organ
    # Accounting for 40% reduction during daytime (*1+ during night, *0.6 during daylight)
    Parameters$PaliveFruit= 1 # default
    Parameters$PaliveLeaf= 1 # default
    Parameters$PaliveRsWood= 0.37 # Ceschia et al., 2002 in Dufrene et al., 2005  
    Parameters$PaliveStumpCoarseRoot= 0.21# Ceschia et al., 2002 in Dufrene et al., 2005
    Parameters$PaliveFineRoot= 1 # default

    
    # Parameters for American Leaf Spot
    Parameters$SlopeAzimut= 180 # 0-364? by definition, E = 90?
    Parameters$Slope= 5         # % slope
    Parameters$RowDistance= 1.5 # distance between coffee rows 
    Parameters$Shade= 0.25      # Shade percentage see in Anna Deffner
    Parameters$Fertilization= 3 # Number of fertilizations per year
    Parameters$ShadeType= 1
    # 1 Legume only; 2	bananas and legume only;3	bananas and other plants;
    # 4	fruit and forest tree only; 5	no shade
    Parameters$CoffeePruning= "tree"
    # tree ; row ; 3 by block ; 4 NULL (no pruning)
    
    return(Parameters)
}
