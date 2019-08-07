#' Energy balance
#'
#' @description Computes the different components of the energy balance considering the shade tree,
#' the coffee and the soil.
#'
#' @param S The simulation list
#' @param i The day index.
#'
#' @return Nothing, modify the list of simulation `S` in place. See [DynACof()] for more details.
#'
balance_model= function(S,i){

  # Tree LE and Rn (can not compute them in the Tree function because we need IntercRevapor)
  S$Sim$LE_Tree[i]= (S$Sim$T_Tree[i]+S$Sim$IntercRevapor[i]*(S$Sim$LAI_Tree[i]/S$Sim$LAIplot[i]))*S$Parameters$lambda
  S$Sim$Rn_Tree[i]= S$Sim$H_Tree[i] + S$Sim$LE_Tree[i]

  # Coffea layer
  S$Sim$LE_Coffee[i]= (S$Sim$T_Coffee[i]+S$Sim$IntercRevapor[i]*(S$Sim$LAI[i]/S$Sim$LAIplot[i]))*S$Parameters$lambda
  S$Sim$Rn_Coffee[i]= S$Sim$H_Coffee[i] + S$Sim$LE_Coffee[i]

  # Plot transpiration
  S$Sim$T_tot[i]= S$Sim$T_Tree[i]+S$Sim$T_Coffee[i]

  # Evapo-Transpiration
  S$Sim$ETR[i]= S$Sim$T_tot[i]+S$Sim$E_Soil[i]+S$Sim$IntercRevapor[i]

  # Total plot energy:
  S$Sim$H_tot[i]= S$Sim$H_Coffee[i]+S$Sim$H_Tree[i]+S$Sim$H_Soil[i]
  S$Sim$LE_tot[i]= S$Sim$LE_Coffee[i]+S$Sim$LE_Tree[i]+S$Sim$LE_Soil[i]
  S$Sim$Rn_tot[i]= S$Sim$Rn_Coffee[i]+S$Sim$Rn_Tree[i]+S$Sim$Rn_Soil[i]

  # Latent and Sensible heat fluxes
  S$Sim$LE_Plot[i]= S$Sim$ETR[i]*S$Parameters$lambda # NB: LE_Plot should be == to LE_tot
}
