Parameters_f= function(){
    
    Parameters= list()
    Parameters$TotalDepth= 3.75 # m
    #Aquiares Soil layer capacity Wmi= minimum water content i=layer (mm)
    Parameters$Wm1=210#400#510 measured, but the wilting point must be much lower
    Parameters$Wm2=58#100#172 measured, but the wilting point must be much lower  
    Parameters$Wm3=64#600#805 measured, but the wilting point must be much lower 
    
    #Aquiares field capacity per layer
    #Aquiares Soil layer capacity Wfi=field capacity,i=layer (mm)
    Parameters$Wf1=290#661.58-40#670 in semih, 661 in daily mean
    Parameters$EWM1=Parameters$Wf1-Parameters$Wm1 #270
    Parameters$Wf2=66#203.98-20#205
    Parameters$EWM2=Parameters$Wf2-Parameters$Wm2#105
    Parameters$Wf3=69#924.69-85#930
    Parameters$EWM3=Parameters$Wf3-Parameters$Wm3#330
    Parameters$EWMtot=Parameters$EWM2+Parameters$EWM1+Parameters$EWM3 #705
    
    #Re-Calibration of the CS615 probes in the field
    Parameters$AmplitudeReductionFactorW1<-0.45
    Parameters$AmplitudeReductionFactorW2<-0.35
    Parameters$AmplitudeReductionFactorW3<-0.08
    
    # Coeff rainfall interception, 0.133*3 = 0.4mm= Interception maximum capacity (mm) (from Siles et al. JH 2010), with average LAI = 3
    Parameters$IntercSlope<- 0.2#*5#mm per Unit of LAI, assumed to be linear. Assumed to be larger per day than per semih
    
    #Surface runoff
    Parameters$WSurfResMax_mm <- 120#0.0299*1000 # Maximum soil water level at the surface reservoir. Above this value, excess rainfall runs-off inmediately (mm)
    Parameters$fc <- 7.44597e-06*1800*1000#prmts(3)           # Minimum infiltration capacity, or minimum "fi" (field capacity) (mm d-1)
    Parameters$alpha <- 101.5610#prmts(4)        # Multiplicative coefficient for calculating the maximum infiltration capacity (alpha >= 1)
    Parameters$fo <- Parameters$fc * Parameters$alpha         # Maximum infiltration capacity, or minimum "fi" (for Ct=0) (mmd-1)
    
    # Discharge
    Parameters$kB <- 2.11549e-05*1800# prmts(2)# Discharge coefficient for surface runoff from surface reservoir (d-1)
    Parameters$rm<-0.6
    
    #k_Rn = extinction coeff for Rn
    #k_Rn<-k #around 0.5, used same as for PAR, Granier et al, 1999, p. 273 top
    Parameters$k_Rn= 0.283 #Shuttleworth and wallace, 1985, p. 851, 
    
    #G = soil heat flux as a fraction of Rn
    Parameters$fG=0.2
    
    #Partitioning of Available Energy for the understorey : LE/(H+LE)
    #Source "Modele ?vaporation soil.xls" with eucalyptus data, following the curve adjustemnt proposed in Jarvis 1976, eq.4
    Parameters$PartHLEu.b1<-1.29
    Parameters$PartHLEu.b2<-6.21
    Parameters$PartHLEu.q<-0.0053
    Parameters$Soil_H_LE_partitioning= 0.66
    
    # Soil layers height measured by each TDR probe
    Parameters$HeightTDR15cm<-225 #mm
    Parameters$HeightTDR30cm<-225 #mm
    Parameters$HeightTDR60cm<-350 #mm
    Parameters$HeightTDR100cm<-450 #mm, sum=1250 mm
    Parameters$HeightTDR150cm<-500 #mm
    Parameters$HeightTDR200cm<-500 #mm
    Parameters$HeightTDR250cm<-500 #mm
    Parameters$HeightTDR300cm<-500 #mm
    Parameters$HeightTDR350cm<-500 #mm,sum=3750
    Parameters$HeightTDR400cm<-500 #mm,sum=4250
    
    # Soil layers height along whole rooting depth in Defrenet et al. 2016sub
    Parameters$Heightlayer1<-1 #m
    Parameters$Heightlayer2<-0.75 #m
    Parameters$Heightlayer3<-170 #m
    
    # Soil Water potential :
    Parameters$PSIE= -0.0002580542 # mean PSIE
    Parameters$PoreFrac= 0.4       # mean pore fraction
    Parameters$B= 4.71             # mean b
    
    
    #Root fractions, source Defrenet "densit? vs biomasse fosse 450.xls" sheet "extrapolation 450", see table
    #In absence of data for the shade tree, we consider the parameters are the same for the 2 species
    Parameters$RootFraction1=0.87
    Parameters$RootFraction2=0.069
    Parameters$RootFraction3=0.061
    
    Parameters$LAImax<-5.5 
    
    #Constant  critical Relative extractable water REWc : Source Granier et al., 1999 Biljou
    Parameters$REWc=0.40

    return(Parameters)
}