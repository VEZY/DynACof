Parameters_f= function(){
    ########### Parameters for Aquiares
    ## Aquiares Plot Parameters
    ## Aquiares Coffee parameters
    ##Contact : O. Roupsard
    ###################################################
    ########### Aquiares Plot Parameters ####

    Parameters= list()
    Parameters$Location= "Aquiares"
    # Optionnal start date of simulation (used only if "Date" is missing from input Meteorology):
    Parameters$Start_Date= "1979/01/01"
    #Latitude (deg):
    Parameters$Latitude= 9.93833 # Aquiares
    #Longitude (deg)
    Parameters$Longitude= -83.72861
    #Time Zone
    Parameters$TimezoneCF= 6
    #Elevation
    Parameters$Elevation= 1040 # (m)
    # Parameters$Elevation= 608 #Google earth, CATIE agroforestry trial, Turrialba, Costa Rica
    Parameters$stocking_final<-6300 #assuming continuous replacement of the dead coffees
    Parameters$StockingRatio_to_Aquiares<-Parameters$stocking_final/6300 # 1.0

    #Canopy height (m)Allen FAO 1998 p. 20 Eq. 4
    Parameters$CanopyHeight.Coffee<-1.2
    Parameters$CanopyHeight.grass<-0.12
    Parameters$CanopyHeight.erythrina<-20
    Parameters$ZHT= 25

    # "percent of shade"
    #Lambda: latent heat of vaporization
    Parameters$lambda= 2.45 # MJ kgH2O-1

    Parameters$albedo= 0.144 # computed using MAESPA, source : MAESPA_Validation/1-Code/4-Aquiares_Metamodels.R
    #zero plane displacement height (m) #Allen FAO 1998 p. 20 Eq. 4 and Box. 4 p. 21 in Allen FAO 1998
    Parameters$DisplacementHeight.coffee<-2/3*Parameters$CanopyHeight.Coffee
    Parameters$DisplacementHeight.grass<-2/3*Parameters$CanopyHeight.grass
    Parameters$DisplacementHeight.erythrina<-2/3*Parameters$CanopyHeight.erythrina

    #roughness length zo (m)in the case where momentum and heat and vapour are at the same height:Allen FAO 1998 p. 20 Eq. 4
    Parameters$z0.coffee<-0.123*Parameters$CanopyHeight.Coffee
    Parameters$z0.grass<-0.123*Parameters$CanopyHeight.grass
    Parameters$z0.erythrina<-0.123*Parameters$CanopyHeight.erythrina

    return(Parameters)
}
