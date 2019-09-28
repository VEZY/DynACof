#' Energy and water models
#'
#' @description Computes the energy and water related variables for the shade tree (if any), the coffee
#' and the soil. Call different sub-models:
#'
#' * [`light_model_tree()`] for the light interception of the shade tree
#' * [`light_model_coffee()`] for the light interception of the coffee
#' * [`energy_model_tree()`] for the energy fluxes of the tree (H, LE, Tleaf...)
#' * [`energy_model_coffee()`] for the energy fluxes of the coffee (H, LE, Tleaf...)
#' * [`soil_model()`] the full soil model (water transport, H, T Soil...)
#' * [`balance_model()`] the energy balance at plot scale model (H, LE, Rn...)
#'
#' @param S The simulation list
#' @param i The day index.
#'
#' @return Nothing, modify the list of simulation `S` in place. See [DynACof()] for more details.
#'
energy_water_models= function(S,i){
  # NB: carefull the order of execution is important here, some function need previous ones in particular order
  if(S$Sim$Stocking_Tree[i] > 0.0){
    light_model_tree(S,i)
  }

  light_model_coffee(S,i)

  if(S$Sim$Stocking_Tree[i] > 0.0){
    energy_model_tree(S,i)
  }

  soil_model(S,i)
  energy_model_coffee(S,i)

  # Soil temperature: we have to know TairCanopy to compute it, but we have to know H_Soil in energy_model_coffee!
  # so we have to compute the soil before the coffee.
  S$Sim$TSoil[i]= S$Sim$TairCanopy[i] + (S$Sim$H_Soil[i] * S$Parameters$MJ_to_W) /
    (bigleaf::air.density(S$Sim$TairCanopy[i], S$Met_c$Pressure[i]/10.0) * S$Parameters$Cp *
       G_soilcan(Wind= S$Met_c$WindSpeed[i], ZHT=S$Parameters$ZHT, Z_top= max(S$Sim$Height_Tree[i], S$Parameters$Height_Coffee),
                 LAI = S$Sim$LAI_Tree[i]  +  S$Sim$LAI[i], extwind= S$Parameters$extwind))

  balance_model(S,i) # Energy balance
}


#' Light interception models
#'
#' @description Computes the light interception (and transmission) for the shade tree or the coffee.
#'
#' @param S The simulation list
#' @param i The day index.
#'
#' @return Nothing, modify the list of simulation `S` in place. See [`DynACof()`] for more details.
#'
#' @aliases light_model_coffee
#'
#' @seealso [`energy_water_models()`]
#' @export
#'
light_model_tree= function(S,i){
  # Metamodel for kdif and kdir
  S$Parameters$k(S,i)
  S$Sim$APAR_Dif_Tree[i]=
    (S$Met_c$PAR[i]*S$Met_c$FDiff[i])*
    (1-exp(-S$Sim$K_Dif_Tree[i]*S$Sim$LAI_Tree[i]))
  S$Sim$APAR_Dir_Tree[i]= (S$Met_c$PAR[i]*(1-S$Met_c$FDiff[i]))*
    (1-exp(-S$Sim$K_Dir_Tree[i]*S$Sim$LAI_Tree[i]))

  S$Sim$APAR_Tree[i]= max(0,S$Sim$APAR_Dir_Tree[i]+S$Sim$APAR_Dif_Tree[i])

  S$Sim$Transmittance_Tree[i]=
    1-(S$Sim$APAR_Tree[i]/S$Met_c$PAR[i])
  S$Sim$Transmittance_Tree[i][is.nan(S$Sim$Transmittance_Tree[i])]= 1
}


#' @rdname light_model_tree
#' @export
light_model_coffee= function(S,i){
  # Light interception ------------------------------------------------------

  S$Sim$K_Dif[i]= S$Parameters$k_Dif
  S$Sim$K_Dir[i]= S$Parameters$k_Dir

  #APAR coffee
  S$Sim$PAR_Trans_Tree[i]= S$Met_c$PAR[i]-S$Sim$APAR_Tree[i] # PAR above coffee layer
  S$Sim$APAR_Dif[i]=
    max(0,(S$Sim$PAR_Trans_Tree[i]*S$Met_c$FDiff[i])*
          (1-exp(-S$Sim$K_Dif[i]*S$Sim$LAI[i])))
  APAR_Dir= max(0,(S$Sim$PAR_Trans_Tree[i]*(1-S$Met_c$FDiff[i]))*
                  (1-exp(-S$Sim$K_Dir[i]*S$Sim$LAI[i])))
  # APAR_Dir is not part of S$Sim because it can be easily computed by
  # S$Met_c$PARm2d1-S$Sim$APAR_Dif
  S$Sim$APAR[i]= APAR_Dir+S$Sim$APAR_Dif[i]
  S$Sim$PAR_Trans[i]= S$Sim$PAR_Trans_Tree[i]-S$Sim$APAR[i] # PAR above soil layer
}


#' Energy fluxes models
#'
#' @description Computes the energy-related variables such as H, LE, Tleaf for the shade tree or the coffee.
#'
#' @param S The simulation list
#' @param i The day index.
#'
#' @return Nothing, modify the list of simulation `S` in place. See [`DynACof()`] for more details.
#'
#' @aliases energy_model_tree
#'
#' @seealso [`energy_water_models()`]
#' @export
#'
energy_model_coffee= function(S,i){
  # Energy balance ----------------------------------------------------------

  # Transpiration Coffee
  S$Sim$T_Coffee[i]= S$Parameters$T_Coffee(S,i)

  S$Sim$H_Coffee[i]= S$Parameters$H_Coffee(S,i)

  # Metamodel Coffee leaf water potential
  S$Sim$PSIL[i]=
    S$Sim$SoilWaterPot[previous_i(i,1)] -
    (S$Sim$T_Coffee[i] / S$Parameters$M_H20) / S$Parameters$KTOT

  # Tcanopy Coffee : using bulk conductance if no trees, interlayer conductance if trees
  # Source: Van de Griend and Van Boxel 1989.
  if(S$Sim$Height_Tree[i]>S$Parameters$Height_Coffee){

    S$Sim$TairCanopy[i]=
      S$Sim$TairCanopy_Tree[i]+((S$Sim$H_Coffee[i]+S$Sim$H_Soil[i])*S$Parameters$MJ_to_W)/
      (bigleaf::air.density(S$Sim$TairCanopy_Tree[i],S$Met_c$Pressure[i]/10)*
         S$Parameters$Cp*
         G_interlay(Wind= S$Met_c$WindSpeed[i], ZHT = S$Parameters$ZHT,
                    LAI_top= S$Sim$LAI_Tree[i],
                    LAI_bot= S$Sim$LAI[i],
                    Z_top= S$Sim$Height_Canopy[i],
                    extwind = S$Parameters$extwind))

    S$Sim$Tleaf_Coffee[i]=
      S$Sim$TairCanopy[i]+(S$Sim$H_Coffee[i]*S$Parameters$MJ_to_W)/
      (bigleaf::air.density(S$Sim$TairCanopy[i],S$Met_c$Pressure[i]/10)*
         S$Parameters$Cp*
         Gb_h(Wind = S$Met_c$WindSpeed[i], wleaf= S$Parameters$wleaf,
              LAI_lay=S$Sim$LAI[i],
              LAI_abv=S$Sim$LAI_Tree[i],
              ZHT = S$Parameters$ZHT,
              Z_top = S$Sim$Height_Canopy[i],
              extwind= S$Parameters$extwind))

  }else{

    S$Sim$TairCanopy[i]=
      S$Met_c$Tair[i]+((S$Sim$H_Coffee[i]+S$Sim$H_Soil[i])*S$Parameters$MJ_to_W)/
      (bigleaf::air.density(S$Sim$TairCanopy_Tree[i],S$Met_c$Pressure[i]/10)*
         S$Parameters$Cp*
         G_bulk(Wind = S$Met_c$WindSpeed[i], ZHT = S$Parameters$ZHT,
                Z_top = S$Sim$Height_Canopy[i],
                LAI = S$Sim$LAI[i],
                extwind = S$Parameters$extwind))

    S$Sim$Tleaf_Coffee[i]=
      S$Sim$TairCanopy[i]+(S$Sim$H_Coffee[i]*S$Parameters$MJ_to_W)/
      (bigleaf::air.density(S$Sim$TairCanopy[i],S$Met_c$Pressure[i]/10)*
         S$Parameters$Cp *
         Gb_h(Wind= S$Met_c$WindSpeed[i], wleaf= S$Parameters$wleaf,
              LAI_lay= S$Sim$LAI[i],
              LAI_abv= S$Sim$LAI_Tree[i],
              ZHT= S$Parameters$ZHT,
              Z_top= S$Sim$Height_Canopy[i],
              extwind= S$Parameters$extwind))
  }
  # NB: if no trees, TairCanopy_Tree= Tair

  # Recomputing soil temperature knowing TairCanopy

  S$Sim$TSoil[i]=
    S$Sim$TairCanopy[i]+(S$Sim$H_Soil[i]*S$Parameters$MJ_to_W)/
    (bigleaf::air.density(S$Sim$TairCanopy[i],S$Met_c$Pressure[i]/10)*
       S$Parameters$Cp*
       G_soilcan(Wind= S$Met_c$WindSpeed[i], ZHT=S$Parameters$ZHT,
                 Z_top= S$Sim$Height_Canopy[i],
                 LAI = S$Sim$LAI_Tree[i] + S$Sim$LAI[i],
                 extwind= S$Parameters$extwind))

  S$Sim$DegreeDays_Tcan[i]=
    GDD(Tmean = S$Sim$TairCanopy[i],MinTT = S$Parameters$MinTT,
        MaxTT = S$Parameters$MaxTT)

}

#' @rdname energy_model_coffee
#' @export
energy_model_tree= function(S,i){
  # Transpiration Tree
  S$Sim$T_Tree[i]= S$Parameters$T_Tree(S,i)
  # Sensible heat Tree
  S$Sim$H_Tree[i]= S$Parameters$H_Tree(S,i)

  S$Sim$PSIL_Tree[i]=
    S$Sim$SoilWaterPot[previous_i(i,1)] -
    (S$Sim$T_Tree[i] / S$Parameters$M_H20) / S$Parameters$KTOT_Tree

  # Computing the air temperature in the shade tree layer:
  S$Sim$TairCanopy_Tree[i]=
    S$Met_c$Tair[i]+(S$Sim$H_Tree[i]*S$Parameters$MJ_to_W)/
    (S$Met_c$Air_Density[i]*S$Parameters$Cp*
       G_bulk(Wind= S$Met_c$WindSpeed[i], ZHT= S$Parameters$ZHT,
              LAI= S$Sim$LAI_Tree[i],
              extwind= S$Parameters$extwind,
              Z_top= S$Sim$Height_Tree[previous_i(i,1)]))
  # NB : using WindSpeed because wind extinction is already computed in G_bulk (until top of canopy).

  S$Sim$Tleaf_Tree[i]=
    S$Sim$TairCanopy_Tree[i]+(S$Sim$H_Tree[i]*S$Parameters$MJ_to_W)/
    (S$Met_c$Air_Density[i]*S$Parameters$Cp*
       Gb_h(Wind = S$Met_c$WindSpeed[i], wleaf= S$Parameters$wleaf_Tree,
            LAI_lay= S$Sim$LAI_Tree[i],
            LAI_abv= 0,ZHT = S$Parameters$ZHT,
            Z_top = S$Sim$Height_Tree[previous_i(i,1)],
            extwind= S$Parameters$extwind))

}
