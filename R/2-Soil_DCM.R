
#' Soil module subroutine
#'
#' @description Make all computations for soil water balance for the ith
#'              day by modifying the \code{S} list in place.
#'
#' @param S The main simulation list to make the computation on and to modify.
#' @param i The index of the day since the first day of the simulation.
#'
#' @return Modify the list of simulation \code{S} in place. See \code{\link{DynACof}} for
#'         more details.
#'
#' @note This function shouldn't be called by the user. It is made as a subroutine so it is easier for
#'       advanced users to modify the code.
#'
#' @keywords internal
#'
#' @seealso \code{\link{DynACof}}
#'
#' @export
Soilfun= function(S,i){

  #Rn or AE per layer (radiation reaching every layer, valid only during dailight hours,
  # not during night hours)
  # Rn understorey, source Shuttleworth & Wallace, 1985, eq. 21
  S$Table_Day$Rn_Soil[i]=
    S$Met_c$Rn[i]*exp(-S$Parameters$k_Rn*S$Table_Day$LAIplot[i])
  # source: Shuttleworth & Wallace, 1985, eq. 2.
  # NB: soil heat storage is negligible at daily time-step (or will equilibrate soon),
  # removing it

  #1/ Rainfall interception, source Gomez-Delgado et al.2011, Box A: IntercMax=AX;
  S$Table_Day$IntercMax[i]= S$Parameters$IntercSlope*S$Table_Day$LAIplot[i]

  S$Table_Day$CanopyHumect[i]=
    max(0,S$Table_Day$CanopyHumect[previous_i(i,1)]+S$Met_c$Rain[i])

  Potential_LeafEvap=
    PENMON(Rn= S$Met_c$Rn[i], Wind= S$Met_c$WindSpeed[i], Tair = S$Met_c$Tair[i],
           ZHT = S$Parameters$ZHT,TREEH = max(S$Table_Day$Height_Tree[i],S$Parameters$Height_Coffee),
           Pressure = S$Met_c$Pressure[i],
           Gs = 1E09, VPD = S$Met_c$VPD[i])

  if(S$Table_Day$CanopyHumect[i]<=S$Table_Day$IntercMax[i]){
    S$Table_Day$Throughfall[i]= 0
    S$Table_Day$IntercRevapor[i]= min(S$Table_Day$CanopyHumect[i], Potential_LeafEvap)
    S$Table_Day$CanopyHumect[i]= max(0,S$Table_Day$CanopyHumect[i]-S$Table_Day$IntercRevapor[i])
  }else{
    S$Table_Day$Throughfall[i]=S$Table_Day$CanopyHumect[i]-S$Table_Day$IntercMax[i]
    S$Table_Day$IntercRevapor[i]=min(S$Table_Day$IntercMax[i],Potential_LeafEvap)
    S$Table_Day$CanopyHumect[i]=max(0,S$Table_Day$IntercMax[i]-S$Table_Day$IntercRevapor[i])
  }

  # 2/ SURFACE RUNOFF / INFILTRATION source Gomez-Delgado et al. 2011,
  # Box B:WSurfResMax = BX; WSurfaceRes=Bt;
  #ExcessRunoff=QB2; SuperficialRunoff=QB1;  TotSuperficialRunoff=QB; Infiltration=i
  # 2.a Adding throughfall to superficial-box, calculation of surface runoff, updating of
  # stock in superficial-box
  S$Table_Day$WSurfaceRes[i]=
    S$Table_Day$WSurfaceRes[previous_i(i,1)] + S$Table_Day$Throughfall[i]

  if(S$Table_Day$WSurfaceRes[i] > S$Parameters$WSurfResMax){
    S$Table_Day$ExcessRunoff[i] = S$Table_Day$WSurfaceRes[i]-S$Parameters$WSurfResMax
    S$Table_Day$WSurfaceRes[i]= S$Parameters$WSurfResMax # removing ExcessRunoff
    S$Table_Day$SuperficialRunoff[i] = S$Parameters$kB * S$Table_Day$WSurfaceRes[i]
    #Subsuperficial runoff from runoffbox
    S$Table_Day$TotSuperficialRunoff[i] =
      S$Table_Day$ExcessRunoff[i] + S$Table_Day$SuperficialRunoff[i]
    S$Table_Day$WSurfaceRes[i] =
      S$Table_Day$WSurfaceRes[i] - S$Table_Day$SuperficialRunoff[i]
  }else{
    #updating WSurfaceRes, the ExcessRunoff has already been retrieved
    S$Table_Day$ExcessRunoff[i]=0
    S$Table_Day$SuperficialRunoff[i] = S$Parameters$kB * S$Table_Day$WSurfaceRes[i]
    S$Table_Day$TotSuperficialRunoff[i] = S$Table_Day$SuperficialRunoff[i]
    S$Table_Day$WSurfaceRes[i] = S$Table_Day$WSurfaceRes[i] -
      S$Table_Day$SuperficialRunoff[i]}

  # 2.b Computing the infiltration capacity as a function of soil water content in W_1
  S$Table_Day$W_1[i]= S$Table_Day$W_1[previous_i(i,1)]

  if(S$Table_Day$W_1[i] <= S$Parameters$Wm1){
    S$Table_Day$InfilCapa[i]= S$Parameters$fo # InfilCapa: infiltration capacity
  }else{
    if(S$Table_Day$W_1[i]<= S$Parameters$Wf1){
      S$Table_Day$InfilCapa[i]= S$Parameters$fo-(S$Table_Day$W_1[i]-S$Parameters$Wm1)*
        (S$Parameters$fo - S$Parameters$fc) / (S$Parameters$Wf1 - S$Parameters$Wm1)
    }else{
      S$Table_Day$InfilCapa[i]=S$Parameters$fc
    }
  }

  # 2.c Calculating infiltration from superficial-box to soil-boxes and updating stock in superficial-box
  if(S$Table_Day$InfilCapa[i]<= S$Table_Day$WSurfaceRes[i]){
    S$Table_Day$Infiltration[i]= S$Table_Day$InfilCapa[i]   # infiltration (m?dt-1)
    S$Table_Day$WSurfaceRes[i]=
      S$Table_Day$WSurfaceRes[i] - S$Table_Day$Infiltration[i]
  }else{
    S$Table_Day$Infiltration[i]= S$Table_Day$WSurfaceRes[i]
    S$Table_Day$WSurfaceRes[i]= 0
  }

  #3/ Adding Infiltration to soil water content of the previous day, computing drainage,
  # source Gomez-Delgado et al. 2010
  # RV: same as CanopyHumect
  # S$Table_Day$W_1[i]= S$Table_Day$W_1[previous_i(i,1)]+S$Zero_then_One[i]*S$Table_Day$Infiltration[i]
  S$Table_Day$W_1[i]= S$Table_Day$W_1[previous_i(i,1)]+S$Table_Day$Infiltration[i]

  #Preventing W_1 to be larger than the soil storage at field capacity:
  if(S$Table_Day$W_1[i] > S$Parameters$Wf1){
    S$Table_Day$Drain_1[i]= S$Table_Day$W_1[i] - S$Parameters$Wf1
    S$Table_Day$W_1[i] = S$Parameters$Wf1
  }else{S$Table_Day$Drain_1[i]= 0}     # Water excess in the root-box that drains (m)

  # RV: same as CanopyHumect
  # S$Table_Day$W_2[i]= S$Table_Day$W_2[previous_i(i,1)]+S$Zero_then_One[i]*S$Table_Day$Drain_1[i]
  S$Table_Day$W_2[i]= S$Table_Day$W_2[previous_i(i,1)]+S$Table_Day$Drain_1[i]

  #Preventing W_2 to be larger than the soil storage at field capacity:
  if(S$Table_Day$W_2[i] > S$Parameters$Wf2){
    S$Table_Day$Drain_2[i]= S$Table_Day$W_2[i] - S$Parameters$Wf2
    S$Table_Day$W_2[i] = S$Parameters$Wf2
  }else{S$Table_Day$Drain_2[i]= 0}     # Water excess in the root-box that drains (m)

  # RV: same as CanopyHumect
  # S$Table_Day$W_3[i]= S$Table_Day$W_3[previous_i(i,1)]+S$Zero_then_One[i]*S$Table_Day$Drain_2[i]
  S$Table_Day$W_3[i]= S$Table_Day$W_3[previous_i(i,1)]+S$Table_Day$Drain_2[i]

  #Preventing W_3 to be larger than the soil storage at field capacity:
  if(S$Table_Day$W_3[i] > S$Parameters$Wf3){
    S$Table_Day$Drain_3[i]= S$Table_Day$W_3[i] - S$Parameters$Wf3
    S$Table_Day$W_3[i] = S$Parameters$Wf3
  }else{S$Table_Day$Drain_3[i]= 0}     # Water excess in the root-box that drains (m)

  #3/First computing water per soil layer
  S$Table_Day$EW_1[i]= S$Table_Day$W_1[i]-S$Parameters$Wm1 # Extractable water (m)
  # Relative extractable water (dimensionless):
  S$Table_Day$REW_1[i]= S$Table_Day$EW_1[i]/(S$Parameters$Wf1-S$Parameters$Wm1)
  S$Table_Day$EW_2[i]= S$Table_Day$W_2[i]-S$Parameters$Wm2
  S$Table_Day$REW_2[i]= S$Table_Day$EW_2[i]/(S$Parameters$Wf2-S$Parameters$Wm2)
  S$Table_Day$EW_3[i]= S$Table_Day$W_3[i]-S$Parameters$Wm3
  S$Table_Day$REW_3[i]= S$Table_Day$EW_3[i]/(S$Parameters$Wf3-S$Parameters$Wm3)
  S$Table_Day$EW_tot[i]= S$Table_Day$EW_1[i]+S$Table_Day$EW_2[i]+S$Table_Day$EW_3[i]
  S$Table_Day$REW_tot[i]= S$Table_Day$EW_tot[i]/
    ((S$Parameters$Wf1-S$Parameters$Wm1)+(S$Parameters$Wf2-S$Parameters$Wm2)+
       (S$Parameters$Wf3-S$Parameters$Wm3))

  #4/Evaporation of the Understorey, E_Soil (from W_1 only)
  S$Table_Day$E_Soil[i]= S$Table_Day$Rn_Soil[i]*S$Parameters$Soil_LE_p/S$Parameters$lambda

  #Avoiding depleting W_1 below Wm1 and udating Wx after retrieving actual E_Soil
  if((S$Table_Day$W_1[i]-S$Table_Day$E_Soil[i])>=S$Parameters$Wm1){
    S$Table_Day$W_1[i]= S$Table_Day$W_1[i]-S$Table_Day$E_Soil[i]
  }else{S$Table_Day$E_Soil[i]= S$Table_Day$W_1[i]-S$Parameters$Wm1
  S$Table_Day$W_1[i]= S$Parameters$Wm1}

  #6/ Root Water Extraction by soil layer, source Granier et al., 1999
  S$Table_Day$RootWaterExtract_1[i]= S$Table_Day$T_tot[i]*S$Parameters$RootFraction1
  S$Table_Day$RootWaterExtract_2[i]= S$Table_Day$T_tot[i]*S$Parameters$RootFraction2
  S$Table_Day$RootWaterExtract_3[i]= S$Table_Day$T_tot[i]*S$Parameters$RootFraction3
  #Avoiding depleting Wx below Wmx, and udating Wx after retrieving actual RootWaterExtract
  if((S$Table_Day$W_1[i]-S$Table_Day$RootWaterExtract_1[i])>=S$Parameters$Wm1){
    S$Table_Day$W_1[i]= S$Table_Day$W_1[i]-S$Table_Day$RootWaterExtract_1[i]
  }else{S$Table_Day$RootWaterExtract_1[i]=S$Table_Day$W_1[i]-S$Parameters$Wm1
  S$Table_Day$W_1[i]=S$Parameters$Wm1}

  if((S$Table_Day$W_2[i]-S$Table_Day$RootWaterExtract_2[i])>=S$Parameters$Wm2){
    S$Table_Day$W_2[i]=S$Table_Day$W_2[i]-S$Table_Day$RootWaterExtract_2[i]
  }else{S$Table_Day$RootWaterExtract_2[i]=S$Table_Day$W_2[i]-S$Parameters$Wm2
  S$Table_Day$W_2[i]=S$Parameters$Wm2}

  if((S$Table_Day$W_3[i]-S$Table_Day$RootWaterExtract_3[i])>=S$Parameters$Wm3){
    S$Table_Day$W_3[i]=S$Table_Day$W_3[i]-S$Table_Day$RootWaterExtract_3[i]
  }else{S$Table_Day$RootWaterExtract_3[i]=S$Table_Day$W_3[i]-S$Parameters$Wm3
  S$Table_Day$W_3[i]=S$Parameters$Wm3}



  #8/Second Updating water per soil layer
  S$Table_Day$W_tot[i]= S$Table_Day$W_1[i]+S$Table_Day$W_2[i]+S$Table_Day$W_3[i]
  S$Table_Day$EW_1[i]= S$Table_Day$W_1[i]-S$Parameters$Wm1 # Extractable water (m)
  S$Table_Day$REW_1[i]= S$Table_Day$EW_1[i]/(S$Parameters$Wf1-S$Parameters$Wm1)
  # Relative extractable water (dimensionless)
  S$Table_Day$EW_2[i]= S$Table_Day$W_2[i]-S$Parameters$Wm2
  S$Table_Day$REW_2[i]= S$Table_Day$EW_2[i]/(S$Parameters$Wf2-S$Parameters$Wm2)
  S$Table_Day$EW_3[i]= S$Table_Day$W_3[i]-S$Parameters$Wm3
  S$Table_Day$REW_3[i]= S$Table_Day$EW_3[i]/(S$Parameters$Wf3-S$Parameters$Wm3)
  S$Table_Day$EW_tot[i]= S$Table_Day$EW_1[i]+S$Table_Day$EW_2[i]+S$Table_Day$EW_3[i]
  S$Table_Day$REW_tot[i]= S$Table_Day$EW_tot[i]/
    ((S$Parameters$Wf1-S$Parameters$Wm1)+(S$Parameters$Wf2-S$Parameters$Wm2)+
       (S$Parameters$Wf3-S$Parameters$Wm3))

  #9/ Soil water deficit
  if(S$Parameters$REWc*S$Parameters$EWMtot-S$Table_Day$EW_tot[i]>0){
    S$Table_Day$SWD[i]= S$Parameters$REWc*S$Parameters$EWMtot-S$Table_Day$EW_tot[i]
  }else{S$Table_Day$SWD[i]= 0}

  # Soil Water potential, Campbell (1974) equation
  S$Table_Day$SoilWaterPot[i]=
    S$Parameters$PSIE*(((S$Table_Day$W_1[i]+S$Table_Day$W_2[i]+
                           S$Table_Day$W_3[i])/3750)/S$Parameters$PoreFrac)^(-S$Parameters$B)


  S$Table_Day$LE_Soil[i]= S$Table_Day$E_Soil[i]*S$Parameters$lambda

  S$Table_Day$H_Soil[i]= S$Table_Day$Rn_Soil[i]*(1-S$Parameters$Soil_LE_p)

  S$Table_Day$Q_Soil[i]= 0
  # RV: Q_Soil is negligible at yearly time-step, and equilibriate between several
  # days anyway.
  S$Table_Day$Rn_Soil[i]=
    S$Table_Day$H_Soil[i] + S$Table_Day$LE_Soil[i] + S$Table_Day$Q_Soil[i]

}
