
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
  S$Sim$Rn_Soil[i]=
    S$Met_c$Rn[i]*exp(-S$Parameters$k_Rn*S$Sim$LAIplot[i])
  # source: Shuttleworth & Wallace, 1985, eq. 2.
  # NB: soil heat storage is negligible at daily time-step (or will equilibrate soon),
  # removing it

  #1/ Rainfall interception, source Gomez-Delgado et al.2011, Box A: IntercMax=AX;
  S$Sim$IntercMax[i]= S$Parameters$IntercSlope*S$Sim$LAIplot[i]

  S$Sim$CanopyHumect[i]=
    max(0,S$Sim$CanopyHumect[previous_i(i,1)]+S$Met_c$Rain[i])

  Potential_LeafEvap=
    PENMON(Rn= S$Met_c$Rn[i], Wind= S$Met_c$WindSpeed[i], Tair = S$Met_c$Tair[i],
           ZHT = S$Parameters$ZHT,Z_top = max(S$Sim$Height_Tree[i],S$Parameters$Height_Coffee),
           Pressure = S$Met_c$Pressure[i],Gs = 1E09, VPD = S$Met_c$VPD[i],LAI= S$Sim$LAIplot[i],
           extwind = S$Parameters$extwind, wleaf= S$Parameters$wleaf)

  if(S$Sim$CanopyHumect[i]<=S$Sim$IntercMax[i]){
    S$Sim$Throughfall[i]= 0
    S$Sim$IntercRevapor[i]= min(S$Sim$CanopyHumect[i], Potential_LeafEvap)
    S$Sim$CanopyHumect[i]= max(0,S$Sim$CanopyHumect[i]-S$Sim$IntercRevapor[i])
  }else{
    S$Sim$Throughfall[i]=S$Sim$CanopyHumect[i]-S$Sim$IntercMax[i]
    S$Sim$IntercRevapor[i]=min(S$Sim$IntercMax[i],Potential_LeafEvap)
    S$Sim$CanopyHumect[i]=max(0,S$Sim$IntercMax[i]-S$Sim$IntercRevapor[i])
  }

  # 2/ SURFACE RUNOFF / INFILTRATION source Gomez-Delgado et al. 2011,
  # Box B:WSurfResMax = BX; WSurfaceRes=Bt;
  #ExcessRunoff=QB2; SuperficialRunoff=QB1;  TotSuperficialRunoff=QB; Infiltration=i
  # 2.a Adding throughfall to superficial-box, calculation of surface runoff, updating of
  # stock in superficial-box
  S$Sim$WSurfaceRes[i]=
    S$Sim$WSurfaceRes[previous_i(i,1)] + S$Sim$Throughfall[i]

  if(S$Sim$WSurfaceRes[i] > S$Parameters$WSurfResMax){
    S$Sim$ExcessRunoff[i] = S$Sim$WSurfaceRes[i]-S$Parameters$WSurfResMax
    S$Sim$WSurfaceRes[i]= S$Parameters$WSurfResMax # removing ExcessRunoff
    S$Sim$SuperficialRunoff[i] = S$Parameters$kB * S$Sim$WSurfaceRes[i]
    #Subsuperficial runoff from runoffbox
    S$Sim$TotSuperficialRunoff[i] =
      S$Sim$ExcessRunoff[i] + S$Sim$SuperficialRunoff[i]
    S$Sim$WSurfaceRes[i] =
      S$Sim$WSurfaceRes[i] - S$Sim$SuperficialRunoff[i]
  }else{
    #updating WSurfaceRes, the ExcessRunoff has already been retrieved
    S$Sim$ExcessRunoff[i]=0
    S$Sim$SuperficialRunoff[i] = S$Parameters$kB * S$Sim$WSurfaceRes[i]
    S$Sim$TotSuperficialRunoff[i] = S$Sim$SuperficialRunoff[i]
    S$Sim$WSurfaceRes[i] = S$Sim$WSurfaceRes[i] -
      S$Sim$SuperficialRunoff[i]
  }

  # 2.b Computing the infiltration capacity as a function of soil water content in W_1
  S$Sim$W_1[i]= S$Sim$W_1[previous_i(i,1)]

  if(S$Sim$W_1[i] <= S$Parameters$Wm1){
    S$Sim$InfilCapa[i]= S$Parameters$fo # InfilCapa: infiltration capacity
  }else{
    if(S$Sim$W_1[i]<= S$Parameters$Wf1){
      S$Sim$InfilCapa[i]= S$Parameters$fo-(S$Sim$W_1[i]-S$Parameters$Wm1)*
        (S$Parameters$fo - S$Parameters$fc) / (S$Parameters$Wf1 - S$Parameters$Wm1)
    }else{
      S$Sim$InfilCapa[i]=S$Parameters$fc
    }
  }

  # 2.c Calculating infiltration from superficial-box to soil-boxes and updating stock in superficial-box
  if(S$Sim$InfilCapa[i]<= S$Sim$WSurfaceRes[i]){
    S$Sim$Infiltration[i]= S$Sim$InfilCapa[i]   # infiltration (m?dt-1)
    S$Sim$WSurfaceRes[i]=
      S$Sim$WSurfaceRes[i] - S$Sim$Infiltration[i]
  }else{
    S$Sim$Infiltration[i]= S$Sim$WSurfaceRes[i]
    S$Sim$WSurfaceRes[i]= 0
  }

  #3/ Adding Infiltration to soil water content of the previous day, computing drainage,
  # source Gomez-Delgado et al. 2010
  S$Sim$W_1[i]= S$Sim$W_1[previous_i(i,1)]+S$Sim$Infiltration[i]

  #Preventing W_1 to be larger than the soil storage at field capacity:
  if(S$Sim$W_1[i] > S$Parameters$Wf1){
    S$Sim$Drain_1[i]= S$Sim$W_1[i] - S$Parameters$Wf1
    S$Sim$W_1[i] = S$Parameters$Wf1
  }else{S$Sim$Drain_1[i]= 0}     # Water excess in the root-box that drains (m)

  # RV: same as CanopyHumect
  S$Sim$W_2[i]= S$Sim$W_2[previous_i(i,1)]+S$Sim$Drain_1[i]

  #Preventing W_2 to be larger than the soil storage at field capacity:
  if(S$Sim$W_2[i] > S$Parameters$Wf2){
    S$Sim$Drain_2[i]= S$Sim$W_2[i] - S$Parameters$Wf2
    S$Sim$W_2[i] = S$Parameters$Wf2
  }else{S$Sim$Drain_2[i]= 0}     # Water excess in the root-box that drains (m)

  S$Sim$W_3[i]= S$Sim$W_3[previous_i(i,1)]+S$Sim$Drain_2[i]

  #Preventing W_3 to be larger than the soil storage at field capacity:
  if(S$Sim$W_3[i] > S$Parameters$Wf3){
    S$Sim$Drain_3[i]= S$Sim$W_3[i] - S$Parameters$Wf3
    S$Sim$W_3[i] = S$Parameters$Wf3
  }else{S$Sim$Drain_3[i]= 0}     # Water excess in the root-box that drains (m)

  #3/First computing water per soil layer
  S$Sim$EW_1[i]= S$Sim$W_1[i]-S$Parameters$Wm1 # Extractable water (m)
  # Relative extractable water (dimensionless):
  S$Sim$REW_1[i]= S$Sim$EW_1[i]/(S$Parameters$Wf1-S$Parameters$Wm1)
  S$Sim$EW_2[i]= S$Sim$W_2[i]-S$Parameters$Wm2
  S$Sim$REW_2[i]= S$Sim$EW_2[i]/(S$Parameters$Wf2-S$Parameters$Wm2)
  S$Sim$EW_3[i]= S$Sim$W_3[i]-S$Parameters$Wm3
  S$Sim$REW_3[i]= S$Sim$EW_3[i]/(S$Parameters$Wf3-S$Parameters$Wm3)
  S$Sim$EW_tot[i]= S$Sim$EW_1[i]+S$Sim$EW_2[i]+S$Sim$EW_3[i]
  S$Sim$REW_tot[i]= S$Sim$EW_tot[i]/
    ((S$Parameters$Wf1-S$Parameters$Wm1)+(S$Parameters$Wf2-S$Parameters$Wm2)+
       (S$Parameters$Wf3-S$Parameters$Wm3))

  #4/Evaporation of the Understorey, E_Soil (from W_1 only)
  S$Sim$E_Soil[i]= S$Sim$Rn_Soil[i]*S$Parameters$Soil_LE_p/S$Parameters$lambda

  #Avoiding depleting W_1 below Wm1 and udating Wx after retrieving actual E_Soil
  if((S$Sim$W_1[i]-S$Sim$E_Soil[i])>=S$Parameters$Wm1){
    S$Sim$W_1[i]= S$Sim$W_1[i]-S$Sim$E_Soil[i]
  }else{S$Sim$E_Soil[i]= S$Sim$W_1[i]-S$Parameters$Wm1
  S$Sim$W_1[i]= S$Parameters$Wm1}

  #6/ Root Water Extraction by soil layer, source Granier et al., 1999
  S$Sim$RootWaterExtract_1[i]= S$Sim$T_tot[i]*S$Parameters$RootFraction1
  S$Sim$RootWaterExtract_2[i]= S$Sim$T_tot[i]*S$Parameters$RootFraction2
  S$Sim$RootWaterExtract_3[i]= S$Sim$T_tot[i]*S$Parameters$RootFraction3
  #Avoiding depleting Wx below Wmx, and udating Wx after retrieving actual RootWaterExtract
  if((S$Sim$W_1[i]-S$Sim$RootWaterExtract_1[i])>=S$Parameters$Wm1){
    S$Sim$W_1[i]= S$Sim$W_1[i]-S$Sim$RootWaterExtract_1[i]
  }else{S$Sim$RootWaterExtract_1[i]=S$Sim$W_1[i]-S$Parameters$Wm1
  S$Sim$W_1[i]=S$Parameters$Wm1}

  if((S$Sim$W_2[i]-S$Sim$RootWaterExtract_2[i])>=S$Parameters$Wm2){
    S$Sim$W_2[i]=S$Sim$W_2[i]-S$Sim$RootWaterExtract_2[i]
  }else{S$Sim$RootWaterExtract_2[i]=S$Sim$W_2[i]-S$Parameters$Wm2
  S$Sim$W_2[i]=S$Parameters$Wm2}

  if((S$Sim$W_3[i]-S$Sim$RootWaterExtract_3[i])>=S$Parameters$Wm3){
    S$Sim$W_3[i]=S$Sim$W_3[i]-S$Sim$RootWaterExtract_3[i]
  }else{S$Sim$RootWaterExtract_3[i]=S$Sim$W_3[i]-S$Parameters$Wm3
  S$Sim$W_3[i]=S$Parameters$Wm3}



  #8/Second Updating water per soil layer
  S$Sim$W_tot[i]= S$Sim$W_1[i]+S$Sim$W_2[i]+S$Sim$W_3[i]
  S$Sim$EW_1[i]= S$Sim$W_1[i]-S$Parameters$Wm1 # Extractable water (m)
  S$Sim$REW_1[i]= S$Sim$EW_1[i]/(S$Parameters$Wf1-S$Parameters$Wm1)
  # Relative extractable water (dimensionless)
  S$Sim$EW_2[i]= S$Sim$W_2[i]-S$Parameters$Wm2
  S$Sim$REW_2[i]= S$Sim$EW_2[i]/(S$Parameters$Wf2-S$Parameters$Wm2)
  S$Sim$EW_3[i]= S$Sim$W_3[i]-S$Parameters$Wm3
  S$Sim$REW_3[i]= S$Sim$EW_3[i]/(S$Parameters$Wf3-S$Parameters$Wm3)
  S$Sim$EW_tot[i]= S$Sim$EW_1[i]+S$Sim$EW_2[i]+S$Sim$EW_3[i]
  S$Sim$REW_tot[i]= S$Sim$EW_tot[i]/
    ((S$Parameters$Wf1-S$Parameters$Wm1)+(S$Parameters$Wf2-S$Parameters$Wm2)+
       (S$Parameters$Wf3-S$Parameters$Wm3))

  #9/ Soil water deficit
  if(S$Parameters$REWc*S$Parameters$EWMtot-S$Sim$EW_tot[i]>0){
    S$Sim$SWD[i]= S$Parameters$REWc*S$Parameters$EWMtot-S$Sim$EW_tot[i]
  }else{S$Sim$SWD[i]= 0}

  # Soil Water potential, Campbell (1974) equation
  S$Sim$SoilWaterPot[i]=
    S$Parameters$PSIE*(((S$Sim$W_1[i]+S$Sim$W_2[i]+
                           S$Sim$W_3[i])/(S$Parameters$TotalDepth*1000))/
                         S$Parameters$PoreFrac)^(-S$Parameters$B)

  S$Sim$LE_Soil[i]= S$Sim$E_Soil[i]*S$Parameters$lambda

  S$Sim$H_Soil[i]= S$Sim$Rn_Soil[i]*(1-S$Parameters$Soil_LE_p)

  S$Sim$Q_Soil[i]= 0
  # RV: Q_Soil is negligible at yearly time-step, and equilibriate between several
  # days anyway.
  S$Sim$Rn_Soil[i]=
    S$Sim$H_Soil[i] + S$Sim$LE_Soil[i] + S$Sim$Q_Soil[i]

}
