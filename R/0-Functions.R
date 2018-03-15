####################################################################################################
####----------------------------------- MAESPA Metamodels --------------------------------------####
# Aim: All utility functions used in the model.
# Authors: Olivier Roupsard (ecophysiological functions) and Rémi Vezy (others)
# Date: 12/09/2017
####################################################################################################


Import_Parameters= function(path= NULL,
                            Names= list(
                              Site="1-Site.R",
                              Soil="3-Soil.R",
                              Coffee="4-Coffee.R",
                              Tree=NULL)){
  # This function imports all parameters for the model
  if(!is.null(path)){
    for(x in Names){
      if(is.character(x)){
        source(file = file.path(path,x))
      }
    }
  }
  Parameters= c(Constants(),site(),coffee(),soil(),if(!is.null(Names$Tree)){Tree()}else{list(Tree_Species= "No_Shade")})

  return(Parameters)
}


# ETo = Allen Potential EvapoTranspiration at 3m (mm h-1) -----------------
#Source : Allen 1998 FAO, Eq. 53 p. 74
Fun.ETo<-function(
  Rn.MJm2semih.param, # MJ m-2 semih
  Delta.param,        # kPa °C-1
  Psi.param,          # kPa °C-1
  Tair.param,         # Celsius degrees
  WindSpeed.param,    # m s-1
  VPD.param          # hPa
){
  Var.ETo1<-(0.408*Delta.param*(Rn.MJm2semih.param-0.1*Rn.MJm2semih.param)*2)/
    (Delta.param+Psi.param*(1+0.34*WindSpeed.param))
  Var.ETo2<-(Psi.param *(37/(Tair.param+273))*WindSpeed.param*VPD.param/10)/
    (Delta.param+Psi.param*(1+0.34*WindSpeed.param))
  Var.ETo<-Var.ETo1+Var.ETo2
  attr(Var.ETo, 'varnames') <- 'ETo'
  attr(Var.ETo, 'units') <- 'mm h-1'
  return(Var.ETo)
}


# Function Psi = psychrometric constant (kPa °C-1) ------------------------
#Source : Allen 1998 FAO Eq. 8 p. 32, tab. 2.2 p. 214
Fun.Psi<-function(
  Pressure.param    # hPa
){
  #specific heat of air at constant pressure:
  Cst.Cp<-1.013*0.001 #(MJ kgair-1 ?C-1) :  Allen 1998 FAO Eq. 8 p. 32
  #epsilon: ratio molecular weight of water vapour/dry air
  Cst.epsilon<-0.622
  #Lambda: latent heat of vaporization
  Cst.lambda<-2.45 #MJ kgH2O-1
  Var.Psi<-Cst.Cp*Pressure.param/10/(Cst.epsilon*Cst.lambda)
  attr(Var.Psi, 'varnames') <- 'Psi'
  attr(Var.Psi, 'units') <- 'kPa ?C-1'
  return(Var.Psi)
  # Data vector (Psi, kPa ?C-1)
}


# ETPenman = Penman Potential EvapoTranspiration at 3m (mm h-1) -----------
#Source : To check in Coffee_Flux_Final data.xls
Fun.ETPen<-function(
  Rn.Wm2.param,     # W m-2
  Delta.param,      # kPa °C-1
  VPD.param,        # hPa
  ra.grass.param    # s m-1
){
  #Lambda: latent heat of vaporization
  Cst.lambda<-2.45 #MJ kgH2O-1
  Var.ETPen<-3600*((Delta.param*10*(Rn.Wm2.param*0.9)+(1.33*1000*VPD.param/ra.grass.param)))/(Delta.param*10+0.66)/(Cst.lambda*1000000)
  attr(Var.ETPen, 'varnames') <- 'ETPen'
  attr(Var.ETPen, 'units') <- 'mm h-1'
  return(Var.ETPen)
}

# VPD = Vapour Pressure Deficit (hPa) -------------------------------------
# #Source : Allen 1998 FAO p. 36, Eq. 11 and 12
# Fun.VPD.fromRHandTair <- function(
#     RH.param,                # Data vector of relative humidity (\code{%})
#     Temperature.param      # Data vector of air temperature (Celsius degree)
# ){
#     Var.VPD <- 6.108 * (1 -RH.param/100) * exp(17.27*Temperature.param/(237.3+Temperature.param))
#     Var.VPD[Var.VPD<0]= 0
#     attr(Var.VPD, 'varnames') <- 'VPD'
#     attr(Var.VPD, 'units') <- 'hPa'
#     return(Var.VPD)
# }
# NB : using the function bigleaf::rH.to.VPD now

fConvertCtoK <- function(
  ## Convert degree Celsius to degree Kelvin
  Celsius.V.n           ##<< Data vector in Celsius (degC)
){
  Kelvin.V.n <-  Celsius.V.n + 273.15
  attr(Kelvin.V.n, 'varnames') <- 'Temp_K'
  attr(Kelvin.V.n, 'units') <- 'degK'
  return(Kelvin.V.n)
  ##value<<
  ## Data vector in temperature Kelvin (Temp_K, degK)
}

######Function Rho.dryair = masse volumique de l'air sec (kg m-3)####
#Source : https://fr.wikipedia.org/wiki/Masse_volumique_de_l%27air
# Fun.Rho.dryair<-function(
#     P.param_Pa,       #Pressure in Pa
#     Temp_Kelvin # Temperature in Kelvin
# )
# {
#     #Molar Mass of Air
#     Cst.Mair<-28.965338*0.001#kg mol-1
#     #Perfect gas universal constant
#     Cst.R<- 8.3144621# J?K-1?mol-1
#     Var.Rho.dryair<- P.param_Pa*Cst.Mair/(Cst.R*Temp_Kelvin)
#     attr(Var.Rho.dryair, 'varnames') <- 'Rho.dryair'
#     attr(Var.Rho.dryair, 'units') <- 'kg m3'
#     return(Var.Rho.dryair)
# }
# Now taken from bigleaf::air.density()

######Function ra = aerodynamic conductance (s m-1) ####
#Source : Allen FAO 1998 p. 20 Eq. 4
#NB : the*0.1 should be kept in "Cst.z0.param*0.1", see Box. 4 in page 21 from Allen FAO 1998
Fun.ra<-function(
  Cst.MeasHeight.param,            # m
  Cst.DisplacementHeight.param,    # m
  Cst.z0.param,                    # m
  WindSpeed.param                  # m s-1
){
  #von Karman's constant:
  Cst.karman<-0.41 #unitless
  Var.ra<-log((Cst.MeasHeight.param-Cst.DisplacementHeight.param)/Cst.z0.param)*
    log((Cst.MeasHeight.param-Cst.DisplacementHeight.param)/(Cst.z0.param*0.1))/(Cst.karman^2)/
    WindSpeed.param
  attr(Var.ra, 'varnames') <- 'ra'
  attr(Var.ra, 'units') <- 's m-1'
  return(Var.ra)
}

######Function e = Vapour Pressure (hPa)####
#Source : Allen 1998 FAO p. 36, Eq. 11 and 12
Fun.e.fromRHandTair <- function(
  RH.param,                ##<< Data vector of relative humidity ( %)
  Temperature.param            ##<< Data vector of air temperature (?C)
){
  Var.e <- 6.108 * RH.param/100 * exp(17.27*Temperature.param/(237.3+Temperature.param))
  attr(Var.e, 'varnames') <- 'e'
  attr(Var.e, 'units') <- 'hPa'
  return(Var.e)
}

######Function esat (hPa)####
#Source : Allen 1998 FAO p. 215, Eq. 11
Fun.esat.fromTair <- function(
  Temperature.param            ##<< Data vector of air temperature (?C)
){
  Var.esat <- 6.108 * exp(17.27*Temperature.param/(237.3+Temperature.param))
  attr(Var.esat, 'varnames') <- 'esat'
  attr(Var.esat, 'units') <- 'hPa'
  return(Var.esat)
}

######Function Delta = slope of vapour pressure curve for different temperatures (kPa ?C-1)####
#Source : Allen 1998 FAO Eq. 13 p. 37, tab 2.4 p.216
Fun.Delta.slopevapourpressure<-function(
  Temperature.param             #?C
)
{
  Var.Delta<-4098*(0.6108*exp((17.27*Temperature.param)/(Temperature.param+237.3)))/(Temperature.param+237.3)^2
  attr(Var.Delta, 'varnames') <- 'Delta'
  attr(Var.Delta, 'units') <- 'kPa ?C-1'
  return(Var.Delta)
  # Data vector (Delta, kPa ?C-1)
}

#Test function Delta
#Delta25m<-Fun.Delta.slopevapourpressure(Temperature.param=25)
#Delta25m : should yield 0.1886 at 25?C

fCalcSunPosition <- function(
  ##title<<
  ## Calculate the position of the sun
  DoY.V.n,               ##<< Data vector with day of year (DoY)
  Hour.V.n,             ##<< Data vector with time as decimal hour
  Lat_deg.n,            ##<< Latitude in (decimal) degrees
  Long_deg.n,           ##<< Longitude in (decimal) degrees
  TimeZone_h.n         ##<< Time zone (in hours)
  ##author<<
  ## AMM
){
  # Formulas taken from Alessandro Cescatti's C++ code
  # Fractional year in radians
  FracYear_rad.V.n <- 2 * pi * (DoY.V.n-1) / 365.24

  # Equation of time in hours, accounting for changes in the time of solar noon
  EqTime_h.V.n <- 0.0072*cos(FracYear_rad.V.n)-0.0528*cos(2*FracYear_rad.V.n)-
    0.0012*cos(3*FracYear_rad.V.n)-0.1229*sin(FracYear_rad.V.n)-0.1565*sin(2*FracYear_rad.V.n)-
    0.0041*sin(3*FracYear_rad.V.n)

  # Local time in hours
  LocTime_h.V.n <- (Long_deg.n/15 - TimeZone_h.n)

  # Solar time
  # Correction for local time and equation of time
  #SolTime_h.V.n <-  Hour.V.n + LocTime_h.V.n + EqTime_h.V.n
  SolTime_h.V.n <-  Hour.V.n
  # Conversion to radians
  SolTime_rad.V.n <- (SolTime_h.V.n - 12) * pi / 12.0
  # Correction for solar time < -pi to positive, important for SolAzim_rad.V.n below
  SolTime_rad.V.n[SolTime_rad.V.n < -pi]= SolTime_rad.V.n[SolTime_rad.V.n < -pi]+2*pi
  attr(SolTime_h.V.n, 'varnames') <- 'SolTime'
  attr(SolTime_h.V.n, 'units') <- 'hour'

  #Solar declination in radians, accounting for the earth axis tilt
  SolDecl_rad.V.n <- (0.33281-22.984*cos(FracYear_rad.V.n)-0.34990*cos(2*FracYear_rad.V.n)-
                        0.13980*cos(3*FracYear_rad.V.n)+3.7872*sin(FracYear_rad.V.n)+
                        0.03205*sin(2*FracYear_rad.V.n) + 0.07187*sin(3*FracYear_rad.V.n))/180*pi
  attr(SolDecl_rad.V.n, 'varnames') <- 'SolDecl'
  attr(SolDecl_rad.V.n, 'units') <- 'rad'

  # Solar elevation (vertical, zenithal angle) in radians with zero for horizon
  SolElev_rad.V.n <-  asin(sin(SolDecl_rad.V.n)*sin(Lat_deg.n/180*pi)+cos(SolDecl_rad.V.n)*
                             cos(Lat_deg.n/180*pi) * cos(SolTime_rad.V.n))
  attr(SolElev_rad.V.n, 'varnames') <- 'SolElev'
  attr(SolElev_rad.V.n, 'units') <- 'rad'

  # Solar azimuth (horizontal angle) with zero for North
  SolAzim_cos.V.n <- (cos(SolDecl_rad.V.n)*cos(SolTime_rad.V.n)-sin(SolElev_rad.V.n)*
                        cos(Lat_deg.n/180*pi))/(sin(Lat_deg.n/180*pi)*cos(SolElev_rad.V.n))
  # Correction if off edge values
  SolAzim_cos.V.n[SolAzim_cos.V.n > +1] <- 1
  SolAzim_cos.V.n[SolAzim_cos.V.n < -1] <- 1
  # Conversion to radians
  SolAzim_rad.V.n <- acos(SolAzim_cos.V.n)
  # Determine if solar azimuth is East or West depending on solar time
  SolAzim_rad.V.n[SolTime_rad.V.n<0]= pi - SolAzim_rad.V.n[SolTime_rad.V.n<0]
  SolAzim_rad.V.n[SolTime_rad.V.n>=0]= pi + SolAzim_rad.V.n[SolTime_rad.V.n>=0]
  attr(SolAzim_cos.V.n, 'varnames') <- 'SolAzim'
  attr(SolAzim_cos.V.n, 'units') <- 'rad'

  ##value<<
  ## Data list with the following items:
  SolPosition.L <- list(
    SolTime = SolTime_h.V.n,     ##<< Solar time (SolTime, hours)
    SolDecl = SolDecl_rad.V.n,  ##<< Solar declination (SolDecl, rad)
    SolElev = SolElev_rad.V.n,  ##<< Solar elevation with 0 at horizon (SolElev, rad)
    SolAzim = SolAzim_rad.V.n  ##<< Solar azimuth with 0 at North (SolAzim, rad)
  )
}


fCalcExtRadiation <- function(
  ##title<<
  ## Calculate the extraterrestrial solar radiation with the eccentricity correction
  DoY.V.n           ##<< Data vector with day of year (DoY)
  ## AMM
){
  # Calculate extraterrestrial solar radiation after Lanini, 2010 (Master thesis, Bern University)
  # Fractional year in radians
  FracYear_rad.V.n <- 2*pi*(DoY.V.n-1) /365.24

  # Total solar irradiance
  SolarIrr_Wm2.c <- 1366.1 #W/m-2 #OK verif "modele Astronomie Turrialba.xl

  #Eccentricity correction
  ExtRadiation.V.n<- SolarIrr_Wm2.c*(1.00011+0.034221*cos(FracYear_rad.V.n)+0.00128*
                                       sin(FracYear_rad.V.n)+0.000719*cos(2*FracYear_rad.V.n)+
                                       0.000077*sin(2*FracYear_rad.V.n))
  attr(ExtRadiation.V.n, 'varnames') <- 'ExtRad'
  attr(ExtRadiation.V.n, 'units') <- 'W_m-2'
  ExtRadiation.V.n
  ##value<<
  ## Data vector of extraterrestrial radiation (ExtRad, W_m-2)
}
#Test function fCalcExtRadiation #returns ExtRadiation.V.n= 1413.982 (W m-2)
#Test<-fCalcExtRadiation (DoY.V.n= 1)
#Test

#################Extra-terrestrial radiation above the GPS point = Potential radiation####################

#####################Diffuse computation with Spitters et al. 1986, page 226 ##############

fSpitters <- function(
  ##title<<
  ## Calculate diffuse computation with Spitters et al. 1986, page 226
  Rg.V.n,  ##<< global radiation
  PotRadiation.V.n,  ##<< potential extraterrestrial radiation at GPS point
  sinbeta.V.n,  ##<< Sine of solar elevation (sinbeta)
  PARtot.V.n ##PARtot
  ##author ORoupsard, see ? 12joursLN_PAR_semih_Vegetation_Int_2002.XLS ?
){
  # Computing
  #Rg/RaNonCloudiness (Lui and Jordan 1960, Iqbal 1983 and GLA Manual, Fraser) assumed to be < 0.85. I consider transmission of the atmosphere cannot be greater than 0.85, see example in Spitters et al. 1986, page 219
  RgbyRaNonCloudiness.V.n= Rg.V.n/PotRadiation.V.n
  RgbyRaNonCloudiness.V.n[RgbyRaNonCloudiness.V.n<0]= NA
  RgbyRaNonCloudiness.V.n[RgbyRaNonCloudiness.V.n>0.85]= NA
  attr(RgbyRaNonCloudiness.V.n, 'varnames') <- 'RgbyRaNonCloudiness'
  attr(RgbyRaNonCloudiness.V.n, 'units') <- 'none'
  RparamSpitters.V.n<-0.847-1.61*sinbeta.V.n+1.04*(sinbeta.V.n^2)#in the regression of diffuse share of transmission
  KparamSpitters.V.n<-(1.47-RparamSpitters.V.n)/1.66# in the regression of diffuse share of transmission
  FDiff.V.n<-ifelse(RgbyRaNonCloudiness.V.n<=0.22,1,
                    ifelse((RgbyRaNonCloudiness.V.n<=0.35&RgbyRaNonCloudiness.V.n>0.22),
                           1-6.4*(RgbyRaNonCloudiness.V.n-0.22)^2,
                           ifelse((RgbyRaNonCloudiness.V.n<=KparamSpitters.V.n&
                                     RgbyRaNonCloudiness.V.n>0.35),
                                  1.47-1.66*RgbyRaNonCloudiness.V.n,RparamSpitters.V.n)))
  attr(FDiff.V.n, 'varnames') <- 'FDiff'
  attr(FDiff.V.n, 'units') <- 'none'
  PARdiffSpitters.V.n<-PARtot.V.n*FDiff.V.n
  attr(PARdiffSpitters.V.n, 'varnames') <- 'PARdiffSpitters'
  attr(PARdiffSpitters.V.n, 'units') <- 'micmolphot m-2 s-1'

  ##value<<
  ## Data vector
  Spitters.L <- list(
    RgbyRaNonCloudiness=RgbyRaNonCloudiness.V.n
    ,FDiff=FDiff.V.n
    ,PARdiffSpitters=PARdiffSpitters.V.n
  )
}

####Net radiation FAO#####################################################################
fNetRadiationFAO <- function(DayNight.V.n, ClearSkyRadiation.V.n, Rg.V.n, Tair.V.n, eVais.V.n
                             ##author ORoupsard, see ? 12joursLN_PAR_semih_Vegetation_Int_2002.XLS ?
                             ##reference<<FAO, p. 51-52 eq 38-40
)
{
  #Stefan-Boltzman's constant:
  Cst.StefanBoltzman.Wm2K<-(2.043e-10/3600)*1000000
  Cst.albedo<-0.23 # #Albedo FAO 1998 p. 51 Eq. 38, for reference grass
  RgbyClearSky.V.n= pmax(0,pmin(1,Rg.V.n/ClearSkyRadiation.V.n))
  RgbyClearSky.V.n[ClearSkyRadiation.V.n==0]= 1
  RgbyClearSky.V.n[DayNight.V.n!=1]= 0
  attr(RgbyClearSky.V.n, 'varnames') <- 'RgbyClearSky'
  attr(RgbyClearSky.V.n, 'units') <- '-'
  RnShortwave.V.n<-pmax(0,Rg.V.n*(1-Cst.albedo))
  attr(RnShortwave.V.n, 'varnames') <- 'RnShortwave'
  attr(RnShortwave.V.n, 'units') <- 'Wm-2'
  RnLongwave.V.n<- Cst.StefanBoltzman.Wm2K*(Tair.V.n+273.16)^4*(0.34-0.14*sqrt(eVais.V.n/10))*
    (1.35*RgbyClearSky.V.n-0.35)
  attr(RnLongwave.V.n, 'varnames') <- 'RnLongwave'
  attr(RnLongwave.V.n, 'units') <- 'Wm-2'
  RnmodFAO.V.n<-RnShortwave.V.n+RnLongwave.V.n
  attr(RnmodFAO.V.n, 'varnames') <- 'RnmodFAO'
  attr(RnmodFAO.V.n, 'units') <- 'Wm-2'

  ##value<<
  NetRadiationFAO = list(
    RgbyClearSky=RgbyClearSky.V.n
    ,RnShortwave=RnShortwave.V.n
    ,RnLongwave=RnLongwave.V.n
    ,RnmodFAO=RnmodFAO.V.n
  )
}


Convert.Var= function(Var, Conv,KHRS=48){
  # Utility function for multiple type of conversions.
  Conv= match.arg(Conv, choices = c("mmol.m-2.s-1_To_mm.semihour","mmol.m-2.s-1_To_W.m-2",
                                    "mm.semihour_To_W.m-2",
                                    "W.m-2_To_mm.semihour",
                                    "MJ.m-2.s-1_to_W.m-2",
                                    "W.m-2_to_MJ.m-2.s-1",
                                    "umol.m-2.s-1_to_W.m-2",
                                    "W.m-2_to_umol.m-2.s-1"))
  SPERHR = 3600 * 24.0 / KHRS
  CONVol.to = SPERHR * 1E-06 * 18 # From MAESPA, mmol.m-2.s-1 to mm
  CONV_mm.to.W= (2.45*10^6)/SPERHR
  CONV_W.to.umol= 4.57
  if(Conv=="mmol.m-2.s-1_To_mm.semihour"){Var= Var*CONVol.to}
  if(Conv=="mmol.m-2.s-1_To_W.m-2"){Var= Var*CONVol.to*CONV_mm.to.W}
  if(Conv=="mm.semihour_To_W.m-2"){Var= Var*CONV_mm.to.W}
  if(Conv=="W.m-2_To_mm.semihour"){Var= Var/CONV_mm.to.W}
  if(Conv=="MJ.m-2.s-1_to_W.m-2"){Var= Var*10^6}
  if(Conv=="W.m-2_to_MJ.m-2.s-1"){Var= Var*10^-6}
  if(Conv=="umol.m-2.s-1_to_W.m-2"){Var= Var/CONV_W.to.umol}
  if(Conv=="W.m-2_to_umol.m-2.s-1"){Var= Var*CONV_W.to.umol}
  return(Var)
}




add.alpha <- function(col, alpha=1){
  # From GIS.Tools,
  # https://gist.githubusercontent.com/mages/5339689/raw/2aaa482dfbbecbfcb726525a3d81661f9d802a8e/add.alpha.R
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha=alpha))
}

previous_i= function(x,n_prev){
  x= x-n_prev
  x[x<=0]= 1
  return(x)
}

Init_Table_Day= function(S){
  # Function to initialise the model variables.
  # Initialise shade tree variables according to species also

  ########## Initializing State Variables ####
  S$Table_Day$LAI= 0.1
  S$Table_Day$LAIplot= 0

  #Leaf Area per Plant location, to convert per ha using density,cannot be zero at beginning,
  # otherwise, GPP does not start and nothing grows
  S$Table_Day$CM_RE=  0
  S$Table_Day$Rm= 0
  S$Table_Day$CM_SCR= 0
  S$Table_Day$Demand_Fruit= 0
  S$Table_Day$CM_Fruit= 0
  S$Table_Day$Sucrose_Mass= 0
  S$Table_Day$Harvest_Maturity= 0
  S$Table_Day$CM_Leaf= 1
  S$Table_Day$CM_FRoot= 0
  S$Table_Day$CM_RsWood= 0
  S$Table_Day$DM_Leaf=
    S$Table_Day$DM_FRoot=
    S$Table_Day$DM_RsWood=
    S$Table_Day$DM_Fruit=
    S$Table_Day$DM_SCR=
    S$Table_Day$DM_Fruit_Cohort=
    S$Table_Day$DM_RE= 0
  S$Table_Day$Mact_SCR=
    S$Table_Day$Mnat_SCR= 0
  S$Table_Day$Mprun_RsWood= 0
  S$Table_Day$DegreeDays_Tcan=
    S$Table_Day$Budinit=
    S$Table_Day$BudBreak=
    S$Table_Day$Bud_available=
    S$Table_Day$BudBreak_cohort=
    S$Table_Day$Alloc_Fruit_Cohort=
    S$Table_Day$NPP_Fruit_Cohort=
    S$Table_Day$CM_Fruit_Cohort=
    S$Table_Day$Maturation_duration_d=
    S$Table_Day$Sucrose_Content= 0
  S$Table_Day$Temp_cor_Bud= 1

  S$Table_Day$NPP_RE= 0
  S$Table_Day$Tcan_Diurnal_Cof_deg= 0
  S$Table_Day$Rn_tot=NA_real_
  S$Table_Day$lue=
    S$Table_Day$GPP=
    S$Table_Day$K_Dif_Cof=
    S$Table_Day$K_Dir_Cof=
    S$Table_Day$Consumption_RE=
    S$Table_Day$Offer_Total=
    S$Table_Day$Carbon_Lack_Mortality=
    S$Table_Day$Alloc_RsWood=
    S$Table_Day$NPP_RsWood=
    S$Table_Day$Rc_RsWood=
    S$Table_Day$Mnat_RsWood=
    S$Table_Day$Mact_RsWood=
    S$Table_Day$Rm_RsWood=
    S$Table_Day$lambdaSCRage=
    S$Table_Day$Alloc_SCR=
    S$Table_Day$NPP_SCR=
    S$Table_Day$Rc_SCR=
    S$Table_Day$Rm_SCR=
    S$Table_Day$Harvest_Maturity_Pot=
    S$Table_Day$ratioNodestoLAI=
    S$Table_Day$Offer_Fruit=
    S$Table_Day$Alloc_Fruit=
    S$Table_Day$Overriped_Fruit=
    S$Table_Day$NPP_Fruit=
    S$Table_Day$Rc_Fruit=
    S$Table_Day$Harvest_Fruit=
    S$Table_Day$Rm_Fruit=
    S$Table_Day$Offer_Leaf=
    S$Table_Day$Alloc_Leaf=
    S$Table_Day$NPP_Leaf=
    S$Table_Day$Rc_Leaf=
    S$Table_Day$Mnat_Leaf=
    S$Table_Day$M_ALS=
    S$Table_Day$MnatALS_Leaf=
    S$Table_Day$Mprun_Leaf=
    S$Table_Day$Mact_Leaf=
    S$Table_Day$Rm_Leaf=
    S$Table_Day$Demand_FRoot=
    S$Table_Day$Offer_FRoot=
    S$Table_Day$Alloc_FRoot=
    S$Table_Day$NPP_FRoot=
    S$Table_Day$Rc_FRoot=
    S$Table_Day$Mnat_FRoot=
    S$Table_Day$Mprun_FRoot=
    S$Table_Day$Mact_FRoot=
    S$Table_Day$Rm_FRoot=
    S$Table_Day$Rc=
    S$Table_Day$Ra=
    S$Table_Day$NPP=
    S$Table_Day$Cbalance=0

  S$Table_Day$Date_harvest= NA_integer_
  # Initializing water and energy balance variables at the begining of each cycle.
  S$Table_Day$Throughfall=
    S$Table_Day$IntercRevapor=
    S$Table_Day$ExcessRunoff=
    S$Table_Day$SuperficialRunoff1=
    S$Table_Day$TotSuperficialRunoff=
    S$Table_Day$InfilCapa=
    S$Table_Day$Infiltration=
    S$Table_Day$Drain_1=
    S$Table_Day$Drain_2=
    S$Table_Day$Drain_3=
    S$Table_Day$EW_1=
    S$Table_Day$REW_1=
    S$Table_Day$EW_2=
    S$Table_Day$REW_2=
    S$Table_Day$EW_3=
    S$Table_Day$REW_3=
    S$Table_Day$EW_tot=
    S$Table_Day$REW_tot=
    S$Table_Day$E_Soil=
    S$Table_Day$RootWaterExtract3=
    S$Table_Day$LE_Plot=
    S$Table_Day$LE_Soil=
    S$Table_Day$H_Soil=
    S$Table_Day$Q_Soil=
    S$Table_Day$Rn_Soil=
    S$Table_Day$LE_Tree=
    S$Table_Day$H_tot=
    S$Table_Day$LE_tot=
    S$Table_Day$Diff_T=
    S$Table_Day$Tcan_Coffee=
    S$Table_Day$APAR_Dif=
    S$Table_Day$APAR=
    S$Table_Day$PAR_Soil=
    S$Table_Day$Tcan_MAESPA_Coffee=
    S$Table_Day$SoilWaterPot=
    S$Table_Day$LeafWaterPotential=
    S$Table_Day$AEu=
    S$Table_Day$IntercMax=
    S$Table_Day$T_Cof=
    S$Table_Day$T_tot=
    S$Table_Day$RootWaterExtract_1=
    S$Table_Day$RootWaterExtract_2=
    S$Table_Day$ETR_Plot=
    S$Table_Day$SWD_tot=
    S$Table_Day$H_Coffee=
    S$Table_Day$Rn_Coffee=
    S$Table_Day$LE_Coffee=0

  S$Table_Day$W_1=290#670 #assumed to be at full level at the time of planting and 1rst of January
  S$Table_Day$W_2=66#205
  S$Table_Day$W_3=69#850
  S$Table_Day$W_tot= S$Table_Day$W_1+S$Table_Day$W_2+S$Table_Day$W_3

  S$Table_Day$CanopyHumect=0 #assume to be dry foliage
  S$Table_Day$WSurfaceRes=0 #assume to half of max

  if(S$Parameters$Tree_Species=="No_Shade"){
    No_Shade.init(S)
  }else{
    Tree.init(S)
  }
}

No_Shade.init= function(S){
  # NB: if Tree_Species is NULL (i.e. no shade trees), then do not add
  # any trees related variables to the main table, except for the few ones
  # needed in-code (e.g. Tree Height for GBCANMS):
  # Shade tree layer computations (common for all species)
  # Should output at least APAR_Tree, LAI_Tree, T_Tree, Rn_Tree, H_Tree,
  # LE_Tree (sum of transpiration + leaf evap)
  # And via allometries: Height_Tree for canopy boundary layer conductance
  S$Table_Day$LAI_Tree=
    S$Table_Day$APAR_Tree=
    S$Table_Day$T_Tree=
    S$Table_Day$Rn_Tree=
    S$Table_Day$H_Tree=
    S$Table_Day$LE_Tree=
    S$Table_Day$Height_Tree=
    0
}

No_Shade= function(...){

}

Tree.init= function(S){
  # Initialisation of Shade tree variables:
  S$Table_Day$CM_Leaf_Tree= 0.01   #Dry Mass of Leaf
  S$Table_Day$CM_Stem_Tree= 0.01
  S$Table_Day$CM_Branch_Tree= 0.01
  S$Table_Day$CM_FRoot_Tree= 0.01
  S$Table_Day$CM_CoarseRoot_Tree= 0.01
  S$Table_Day$CM_Reserves_Tree= 0.15

  S$Table_Day$LAI_Tree=
    S$Table_Day$CM_Leaf_Tree*(S$Parameters$SLA_Tree/1000)/
    S$Parameters$CContent_Leaf_Tree

  S$Table_Day$Trunk_H_Tree=
    S$Table_Day$Crown_H_Tree=
    S$Table_Day$LA_Tree=
    0

  S$Table_Day$DM_Leaf_Tree=
    S$Table_Day$DM_Branch_Tree=
    S$Table_Day$DM_Stem_Tree=
    S$Table_Day$DM_CoarseRoot_Tree=
    S$Table_Day$DM_FRoot_Tree=
    S$Table_Day$DM_Stem_FGM_Tree=
    S$Table_Day$DM_RE_Tree= 0


  S$Table_Day$Mprun_Branch_Tree=
    S$Table_Day$Mprun_FRoot_Tree=
    S$Table_Day$Mprun_Leaf_Tree=
    S$Table_Day$Mact_Stem_Tree=
    S$Table_Day$Mact_CoarseRoot_Tree=
    S$Table_Day$Rm_Tree=
    S$Table_Day$DBH_Tree=
    S$Table_Day$Crown_H_Tree=
    S$Table_Day$CrownProj_Tree=
    S$Table_Day$LAD_Tree=
    0

  S$Table_Day$K_Dif_Tree=
    S$Table_Day$K_Dir_Tree=
    S$Table_Day$APAR_Dif_Tree=
    S$Table_Day$APAR_Dir_Tree=
    S$Table_Day$APAR_Tree=
    S$Table_Day$Transmittance_Tree=
    S$Table_Day$lue_Tree=
    S$Table_Day$T_Tree=
    S$Table_Day$H_Tree=
    S$Table_Day$GPP_Tree=
    S$Table_Day$Rm_Leaf_Tree=
    S$Table_Day$Rm_CoarseRoot_Tree=
    S$Table_Day$Rm_Branch_Tree=
    S$Table_Day$Rm_Stem_Tree=
    S$Table_Day$Rm_FRoot_Tree=
    S$Table_Day$Offer_Total_Tree=
    S$Table_Day$Alloc_Stem_Tree=
    S$Table_Day$NPP_Stem_Tree=
    S$Table_Day$Rc_Stem_Tree=
    S$Table_Day$Alloc_CoarseRoot_Tree=
    S$Table_Day$NPP_CoarseRoot_Tree=
    S$Table_Day$Rc_CoarseRoot_Tree=
    S$Table_Day$Alloc_Branch_Tree=
    S$Table_Day$NPP_Branch_Tree=
    S$Table_Day$Rc_Branch_Tree=
    S$Table_Day$Mact_Branch_Tree=
    S$Table_Day$Alloc_Leaf_Tree=
    S$Table_Day$NPP_Leaf_Tree=
    S$Table_Day$Rc_Leaf_Tree=
    S$Table_Day$Mact_Leaf_Tree=
    S$Table_Day$Alloc_FRoot_Tree=
    S$Table_Day$NPP_FRoot_Tree=
    S$Table_Day$Rc_FRoot_Tree=
    S$Table_Day$Mact_FRoot_Tree=
    S$Table_Day$Alloc_Reserves_Tree=
    S$Table_Day$Rc_Reserves_Tree=
    S$Table_Day$Rc_Tree=
    S$Table_Day$Ra_Tree=
    S$Table_Day$DeltaCM__Tree=
    S$Table_Day$NPP_Tree=
    S$Table_Day$Cbalance_Tree=
    S$Table_Day$Height_Tree=
    S$Table_Day$CrownRad_Tree=
    S$Table_Day$NPP_Reserves_Tree=
    S$Table_Day$Consumption_RE_Tree=
    0

  # Thinning:
  S$Table_Day$Stocking_Tree= S$Parameters$StockingTree_treeha1/10000
  S$Table_Day$TimetoThin_Tree= 0
  S$Table_Day$MThinning_Stem_Tree= 0
  S$Table_Day$MThinning_CoarseRoot_Tree= 0
  S$Table_Day$MThinning_Branch_Tree= 0
  S$Table_Day$MThinning_Leaf_Tree=0
  S$Table_Day$MThinning_FRoot_Tree= 0


  # Energy budget :
  S$Table_Day$Rn_Tree=
    S$Table_Day$H_Tree=
    S$Table_Day$Rn_tot=
    S$Table_Day$Rn_Tree=
    0
}

Shade.Tree= function(S,i){
  # Shade tree layer computations (common for all species)
  # Should output at least APAR_Tree, LAI_Tree, T_Tree, Rn_Tree, H_Tree,
  # LE_Tree (sum of transpiration + leaf evap)
  # And via allometries: Height_Tree for canopy boundary layer conductance


  # Metamodel for kdif and kdir
  S$Parameters$k(S,i)

  #For stocking Cordia=50, without thinning, with metamodel
  S$Table_Day$APAR_Dif_Tree[i]=
    (S$Met_c$PAR_MJ[i]*S$Met_c$FDiff[i])*
    (1-exp(-S$Table_Day$K_Dif_Tree[i]*S$Table_Day$LAI_Tree[i-S$Zero_then_One[i]]))#MJ m-2 d-1
  S$Table_Day$APAR_Dir_Tree[i]= (S$Met_c$PAR_MJ[i]*(1-S$Met_c$FDiff[i]))*
    (1-exp(-S$Table_Day$K_Dir_Tree[i]*S$Table_Day$LAI_Tree[i-S$Zero_then_One[i]]))#MJ m-2 d-1
  S$Table_Day$APAR_Tree[i]= max(0,S$Table_Day$APAR_Dir_Tree[i]+S$Table_Day$APAR_Dif_Tree[i])
  # S$Table_Day$APAR_Tree[i]= S$Met_c$PAR_MJ[i]*(1-exp(-S$Table_Day$k_Tree[i]*
  # S$Table_Day$LAI_Tree[i-S$Zero_then_One[i]]))
  S$Table_Day$Transmittance_Tree[i]=
    1-(S$Table_Day$APAR_Tree[i]/S$Met_c$PAR_MJ[i])
  S$Table_Day$Transmittance_Tree[i][is.nan(S$Table_Day$Transmittance_Tree[i])]=0
  # Calling the metamodels for LUE, Transpiration and sensible heat flux :
  S$Parameters$Metamodels(S,i)

  #GPP
  S$Table_Day$GPP_Tree[i]= S$Table_Day$lue_Tree[i]*S$Table_Day$APAR_Tree[i]
  #Tree Thinning threshold when Transmittance <=S$Parameters$ThinThresh, then
  if(S$Table_Day$Transmittance_Tree[i]<S$Parameters$ThinThresh){
    S$Table_Day$TimetoThin_Tree[i]=1
  }

  # Maintenance respiration -------------------------------------------------

  # Rm is computed at the beginning of the day on the drymass of the previous day.
  S$Table_Day$Rm_Leaf_Tree[i]=
    S$Parameters$PaliveLeaf_Tree*S$Table_Day$DM_Leaf_Tree[i-S$Zero_then_One[i]]*
    S$Parameters$MRN_Tree*S$Parameters$NContentLeaf_Tree*S$Parameters$Q10Leaf_Tree^
    ((S$Met_c$Tair[i]-S$Parameters$TMR)/10)

  S$Table_Day$Rm_CoarseRoot_Tree[i]=
    S$Parameters$PaliveCoarseRoot_Tree*
    S$Table_Day$DM_CoarseRoot_Tree[i-S$Zero_then_One[i]]*
    S$Parameters$MRN_Tree*S$Parameters$NContentCoarseRoot_Tree*
    S$Parameters$Q10CoarseRoot_Tree^(
      (S$Met_c$Tair[i]-S$Parameters$TMR)/10)

  S$Table_Day$Rm_Branch_Tree[i]=
    S$Parameters$PaliveBranch_Tree*
    S$Table_Day$DM_Branch_Tree[i-S$Zero_then_One[i]]*
    S$Parameters$MRN_Tree*S$Parameters$NContentBranch_Tree*
    S$Parameters$Q10Branch_Tree^(
      (S$Met_c$Tair[i]-S$Parameters$TMR)/10)

  S$Table_Day$Rm_Stem_Tree[i]=
    S$Parameters$PaliveStem_Tree[S$Table_Day$Plot_Age[i],2]*
    S$Table_Day$DM_Stem_Tree[i-S$Zero_then_One[i]]*
    S$Parameters$MRN_Tree*S$Parameters$NContentStem_Tree*
    S$Parameters$Q10Stem_Tree^(
      (S$Met_c$Tair[i]-S$Parameters$TMR)/10)

  S$Table_Day$Rm_FRoot_Tree[i]=
    S$Parameters$PaliveFRoot_Tree*
    S$Table_Day$DM_FRoot_Tree[i-S$Zero_then_One[i]]*
    S$Parameters$MRN*S$Parameters$NContentFRoot_Tree*
    S$Parameters$Q10FRoot_Tree^(
      (S$Met_c$Tair[i]-S$Parameters$TMR)/10)

  S$Table_Day$Rm_Tree[i]=
    S$Table_Day$Rm_Leaf_Tree[i]+ S$Table_Day$Rm_CoarseRoot_Tree[i]+
    S$Table_Day$Rm_Branch_Tree[i]+S$Table_Day$Rm_Stem_Tree[i]+
    S$Table_Day$Rm_FRoot_Tree[i]

  ############################----- Shade Tree Allocation ----############################

  ########## Potential use of reserves####
  # Reserves are used only if GPP doesn't meet the maintenance respiration + 10% need.
  # Thus, if GPP is < to Rm*1.1, then we take the needed C to meet the Rm (Rm*1.1-GPP), but not more than
  # there is C in the reserves
  if(S$Table_Day$GPP_Tree[i]<(1.2*S$Table_Day$Rm_Tree[i])){
    S$Table_Day$Consumption_RE_Tree[i]=
      max(0,min(S$Table_Day$CM_Reserves_Tree[previous_i(i,1)],S$Parameters$kres_max_Tree*S$Table_Day$Rm_Tree[i]))
  }

  ### Offer Function: NB, Rm is used from the previous i, assumed not very different but could
  # also be computed first, actually
  S$Table_Day$Offer_Total_Tree[i]=
    S$Table_Day$GPP_Tree[i]-S$Table_Day$Rm_Tree[i]+S$Table_Day$Consumption_RE_Tree[i]
  # If the offer is negative, there is mortality

  #### Stem ####
  # Offer: NB, Rm is used from the previous i, assumed not very different but could also be computed first, actually
  S$Table_Day$Alloc_Stem_Tree[i]=
    S$Parameters$lambdaStem_Tree*S$Table_Day$Offer_Total_Tree[i]
  #NPP = Offer * growth cost coefficient:
  S$Table_Day$NPP_Stem_Tree[i]=
    S$Parameters$epsilonStem_Tree*S$Table_Day$Alloc_Stem_Tree[i]
  # Growth respiration = Offer * (1-growth cost coefficient):
  S$Table_Day$Rc_Stem_Tree[i]=
    (1-S$Parameters$epsilonStem_Tree)*S$Table_Day$Alloc_Stem_Tree[i]
  # Mortality: No mortality yet for this compartment.
  # If stem mortality has to be set, write it here.


  #### Coarse Roots ####
  # Offer:
  S$Table_Day$Alloc_CoarseRoot_Tree[i]=
    S$Parameters$lambdaCoarseRoot_Tree*S$Table_Day$Offer_Total_Tree[i]
  #NPP
  S$Table_Day$NPP_CoarseRoot_Tree[i]= S$Parameters$epsilonCoarseRoot_Tree*
    S$Table_Day$Alloc_CoarseRoot_Tree[i]
  # Growth respiration:
  S$Table_Day$Rc_CoarseRoot_Tree[i]= (1-S$Parameters$epsilonCoarseRoot_Tree)*
    S$Table_Day$Alloc_CoarseRoot_Tree[i]
  # Natural mortality
  S$Table_Day$Mact_CoarseRoot_Tree[i]=
    S$Table_Day$CM_CoarseRoot_Tree[i-S$Zero_then_One[i]]/S$Parameters$lifespanCoarseRoot_Tree


  #### Branches ####
  # NB: Served first as Erythrina must regrow branches in priority after pruning
  S$Table_Day$Alloc_Branch_Tree[i]=
    S$Parameters$lambdaBranchWood_Tree*S$Table_Day$Offer_Total_Tree[i]
  #NPP
  S$Table_Day$NPP_Branch_Tree[i]=
    S$Parameters$epsilonBranch_Tree*S$Table_Day$Alloc_Branch_Tree[i]
  # Growth respiration:
  S$Table_Day$Rc_Branch_Tree[i]=
    (1-S$Parameters$epsilonBranch_Tree)*S$Table_Day$Alloc_Branch_Tree[i]
  # Natural mortality:
  S$Table_Day$Mact_Branch_Tree[i]=
    S$Table_Day$CM_Branch_Tree[i-S$Zero_then_One[i]]/S$Parameters$lifespanBranch_Tree


  #### Leaves ####
  # Offer:
  S$Table_Day$Alloc_Leaf_Tree[i]=
    S$Parameters$lambdaLeaf_Tree*S$Table_Day$Offer_Total_Tree[i]
  #NPP
  S$Table_Day$NPP_Leaf_Tree[i]=
    S$Parameters$epsilonLeaf_Tree*S$Table_Day$Alloc_Leaf_Tree[i]
  # Growth respiration:
  S$Table_Day$Rc_Leaf_Tree[i]=
    (1-S$Parameters$epsilonLeaf_Tree)*S$Table_Day$Alloc_Leaf_Tree[i]

  # Leaf Fall ---------------------------------------------------------------

  if(S$Met_c$DOY[i]%in%S$Parameters$Fall_Period_Tree&S$Table_Day$Plot_Age[i]>1){
    # Phenology (leaf mortality increases in this period) if Leaf_Fall_Tree is TRUE
    S$Table_Day$Mact_Leaf_Tree[i]=
      S$Table_Day$CM_Leaf_Tree[i-S$Zero_then_One[i]]*S$Parameters$Leaf_fall_rate_Tree
  }else{
    # Or just natural litterfall assuming no diseases
    S$Table_Day$Mact_Leaf_Tree[i]=
      S$Table_Day$CM_Leaf_Tree[i-S$Zero_then_One[i]]/S$Parameters$lifespanLeaf_Tree
  }

  #### Fine roots ####
  # Offer
  S$Table_Day$Alloc_FRoot_Tree[i]=
    S$Parameters$lambdaFRoot_Tree*S$Table_Day$Offer_Total_Tree[i]
  # NPP
  S$Table_Day$NPP_FRoot_Tree[i]=
    S$Parameters$epsilonFRoot_Tree*S$Table_Day$Alloc_FRoot_Tree[i]
  # Growth respiration
  S$Table_Day$Rc_FRoot_Tree[i]=
    (1-S$Parameters$epsilonFRoot_Tree)*S$Table_Day$Alloc_FRoot_Tree[i]
  # Natural mortality
  S$Table_Day$Mact_FRoot_Tree[i]=
    S$Table_Day$CM_FRoot_Tree[i-S$Zero_then_One[i]]/S$Parameters$lifespanFRoot_Tree


  #### Reserves ####

  # Offer
  S$Table_Day$Alloc_Reserves_Tree[i]=
    S$Parameters$lambdaReserves_Tree*S$Table_Day$Offer_Total_Tree[i]
  # Allocation
  S$Table_Day$NPP_Reserves_Tree[i]=
    S$Parameters$epsilonReserves_Tree*S$Table_Day$Alloc_Reserves_Tree[i]
  # Cost of allocating to reserves
  S$Table_Day$Rc_Reserves_Tree[i]=
    (1-S$Parameters$epsilonReserves_Tree)*S$Table_Day$Alloc_Reserves_Tree[i]



  # Pruning -----------------------------------------------------------------

  # NB: several dates of pruning are allowed
  if(S$Table_Day$Plot_Age[i]%in%S$Parameters$Pruning_Age_Tree&&S$Met_c$DOY[i]%in%S$Parameters$date_pruning_Tree){
    # Leaves pruning :
    S$Table_Day$Mprun_Leaf_Tree[i]=
      S$Table_Day$CM_Leaf_Tree[i-S$Zero_then_One[i]]*S$Parameters$pruningIntensity_Tree
    # Total mortality (cannot exceed total leaf dry mass):
    S$Table_Day$Mact_Leaf_Tree[i]=
      min(S$Table_Day$Mact_Leaf_Tree[i] + S$Table_Day$Mprun_Leaf_Tree[i],
          S$Table_Day$CM_Leaf_Tree[i-S$Zero_then_One[i]])

    # Branch pruning:
    S$Table_Day$Mprun_Branch_Tree[i]=
      S$Table_Day$CM_Branch_Tree[i-S$Zero_then_One[i]]*S$Parameters$pruningIntensity_Tree
    S$Table_Day$Mact_Branch_Tree[i]=
      min((S$Table_Day$Mact_Branch_Tree[i]+S$Table_Day$Mprun_Branch_Tree[i]),
          S$Table_Day$CM_Branch_Tree[i-S$Zero_then_One[i]])

    # Effect of pruning on fine roots  (assumed half the leaves mortality, may be wrong):
    # S$Table_Day$Mprun_FRoot_Tree[i]= (S$Table_Day$Mprun_Leaf_Tree[i]*0.5)
    S$Table_Day$Mprun_FRoot_Tree[i]=
      S$Table_Day$CM_FRoot_Tree[i-S$Zero_then_One[i]]*S$Parameters$pruningIntensity_Tree
    S$Table_Day$Mact_FRoot_Tree[i]=
      min(S$Table_Day$Mact_FRoot_Tree[i]+S$Table_Day$Mprun_FRoot_Tree[i],
          S$Table_Day$CM_FRoot_Tree[i-S$Zero_then_One[i]])
  }


  # Thinning ----------------------------------------------------------------

  if(S$Table_Day$TimetoThin_Tree[i]==1){
    # First, reduce stocking by the predefined rate of thining:
    S$Table_Day$Stocking_Tree[i:nrow(S$Table_Day)]=
      S$Table_Day$Stocking_Tree[i-1]*(1-S$Parameters$RateThinning_Tree)
    # Then add mortality (removing) due to thining :
    S$Table_Day$MThinning_Stem_Tree[i]=
      S$Table_Day$CM_Stem_Tree[i-S$Zero_then_One[i]]*S$Parameters$RateThinning_Tree
    S$Table_Day$MThinning_CoarseRoot_Tree[i]=
      S$Table_Day$CM_CoarseRoot_Tree[i-S$Zero_then_One[i]]*S$Parameters$RateThinning_Tree
    S$Table_Day$MThinning_Branch_Tree[i]=
      S$Table_Day$CM_Branch_Tree[i-S$Zero_then_One[i]]*S$Parameters$RateThinning_Tree
    S$Table_Day$MThinning_Leaf_Tree[i]=
      S$Table_Day$CM_Leaf_Tree[i-S$Zero_then_One[i]]*S$Parameters$RateThinning_Tree
    S$Table_Day$MThinning_FRoot_Tree[i]=
      S$Table_Day$CM_FRoot_Tree[i-S$Zero_then_One[i]]*S$Parameters$RateThinning_Tree
  }

  # Dry Mass update ---------------------------------------------------------

  S$Table_Day$CM_Leaf_Tree[i]=
    S$Table_Day$CM_Leaf_Tree[i-S$Zero_then_One[i]]+S$Table_Day$NPP_Leaf_Tree[i]-
    S$Table_Day$Mact_Leaf_Tree[i]-S$Table_Day$MThinning_Leaf_Tree[i]

  S$Table_Day$CM_Branch_Tree[i]=
    S$Table_Day$CM_Branch_Tree[i-S$Zero_then_One[i]]+S$Table_Day$NPP_Branch_Tree[i]-
    S$Table_Day$Mact_Branch_Tree[i]-S$Table_Day$MThinning_Branch_Tree[i]

  S$Table_Day$CM_Stem_Tree[i]=
    S$Table_Day$CM_Stem_Tree[i-S$Zero_then_One[i]]+S$Table_Day$NPP_Stem_Tree[i]-
    S$Table_Day$Mact_Stem_Tree[i]-S$Table_Day$MThinning_Stem_Tree[i]

  S$Table_Day$CM_CoarseRoot_Tree[i]=
    S$Table_Day$CM_CoarseRoot_Tree[i-S$Zero_then_One[i]]+
    S$Table_Day$NPP_CoarseRoot_Tree[i]- S$Table_Day$Mact_CoarseRoot_Tree[i]-
    S$Table_Day$MThinning_CoarseRoot_Tree[i]

  S$Table_Day$CM_FRoot_Tree[i]=
    S$Table_Day$CM_FRoot_Tree[i-S$Zero_then_One[i]]+
    S$Table_Day$NPP_FRoot_Tree[i]-S$Table_Day$Mact_FRoot_Tree[i]-
    S$Table_Day$MThinning_FRoot_Tree[i]

  S$Table_Day$CM_Reserves_Tree[i]=
    S$Table_Day$CM_Reserves_Tree[i-S$Zero_then_One[i]]+
    S$Table_Day$NPP_Reserves_Tree[i]-S$Table_Day$Consumption_RE_Tree[i]

  ##########################################
  S$Table_Day$DM_Leaf_Tree[i]=
    S$Table_Day$CM_Leaf_Tree[i]/S$Parameters$CContent_Leaf_Tree
  S$Table_Day$DM_Branch_Tree[i]=
    S$Table_Day$CM_Branch_Tree[i]/S$Parameters$CContent_wood_Tree
  S$Table_Day$DM_Stem_Tree[i]=
    S$Table_Day$CM_Stem_Tree[i]/S$Parameters$CContent_wood_Tree
  S$Table_Day$DM_CoarseRoot_Tree[i]=
    S$Table_Day$CM_CoarseRoot_Tree[i]/S$Parameters$CContent_wood_Tree
  S$Table_Day$DM_FRoot_Tree[i]=
    S$Table_Day$CM_FRoot_Tree[i]/S$Parameters$CContent_wood_Tree

  S$Table_Day$Rc_Tree[i]=
    S$Table_Day$Rc_CoarseRoot_Tree[i]+S$Table_Day$Rc_Leaf_Tree[i]+
    S$Table_Day$Rc_Branch_Tree[i]+S$Table_Day$Rc_Stem_Tree[i]+
    S$Table_Day$Rc_FRoot_Tree[i]+S$Table_Day$Rc_Reserves_Tree[i]

  S$Table_Day$Ra_Tree[i]=
    S$Table_Day$Rm_Tree[i]+S$Table_Day$Rc_Tree[i]

  # NPP_Tree
  S$Table_Day$NPP_Tree[i]=
    S$Table_Day$NPP_Stem_Tree[i]+S$Table_Day$NPP_Branch_Tree[i]+
    S$Table_Day$NPP_Leaf_Tree[i]+S$Table_Day$NPP_CoarseRoot_Tree[i]+
    S$Table_Day$NPP_FRoot_Tree[i]+S$Table_Day$NPP_Reserves_Tree[i]

  # Daily C balance that should be nil every day:
  S$Table_Day$Cbalance_Tree[i]=
    S$Table_Day$Offer_Total_Tree[i]-(S$Table_Day$NPP_Tree[i]+S$Table_Day$Rc_Tree[i])

  S$Table_Day$LAI_Tree[i]= S$Table_Day$DM_Leaf_Tree[i]*(S$Parameters$SLA_Tree/1000)
  # Allometries ------------------------------------------------------------
  S$Parameters$Allometries(S,i)

  S$Table_Day$LAIplot[i]= S$Table_Day$LAIplot[i] + S$Table_Day$LAI_Tree[i]
}


write.results= function(FinalList,output=".RData",Simulation_Name= NULL,...){
  if(is.null(Simulation_Name)){
    Simulation_Name= gsub(" ","_",paste(FinalList$Parameters$Location,FinalList$Parameters$Species_ID,
                                        FinalList$Parameters$StockingTree_treeha1,
                                        paste(paste(names(unlist(FinalList$Climate)),
                                                    unlist(FinalList$Climate), sep="="), collapse='_'),collapse="_"))
  }
  if(!dir.exists(file.path("3-Results/",Simulation_Name))){
    dir.create(file.path("3-Results/",Simulation_Name))
  }
  if(output!=".RData"){
    fwrite(FinalList$Table_Day,
           paste0("3-Results/",Simulation_Name,"/Simulation_Output_",
                  format(FinalList$Met_c$Date[1], '%m-%Y'), "_",
                  format(tail(FinalList$Met_c$Date,1), '%m-%Y'),".csv"),
           sep=";",row.names = F, col.names = TRUE, ...)
    fwrite(FinalList$PerCohortFruitDemand,
           paste0("3-Results/",Simulation_Name,"/PerCohortFruitDemand", "_",
                  format(FinalList$Met_c$Date[1], '%m-%Y'), "_",
                  format(tail(FinalList$Met_c$Date,1), '%m-%Y'),".csv"),
           sep=";",row.names = F, col.names = TRUE, ...)
    fwrite(FinalList$Met_c,
           paste0("3-Results/",Simulation_Name,"/Met_c", "_",
                  format(FinalList$Met_c$Date[1], '%m-%Y'), "_",
                  format(tail(FinalList$Met_c$Date,1), '%m-%Y'),".csv"),
           sep=";",row.names = F, col.names = TRUE, ...)
    capture.output(FinalList$Parameters, file =  paste0("3-Results/",Simulation_Name,"/Parameters", "_",
                                                        format(FinalList$Met_c$Date[1], '%m-%Y'), "_",
                                                        format(tail(FinalList$Met_c$Date,1), '%m-%Y'),".txt"))
  }else{
    S= FinalList
    save(S,file = paste0("3-Results/",Simulation_Name,"/Simulation", "_",
                         format(FinalList$Met_c$Date[1], '%m-%Y'), "_",
                         format(tail(FinalList$Met_c$Date,1), '%m-%Y'),".RData"))
  }

  cat("Simulation results saved in ", file.path(getwd(),"3-Results",Simulation_Name), "\n")
}



plot.stacked <- function(
  x, y,
  order.method = "as.is",
  ylab="", xlab="",
  border = NULL, lwd=1,
  col=rainbow(length(y[1,])),
  ylim=NULL,xlim=NULL,
  density= -1, lty=1,
  Fignum= NULL,
  ...
){
  #plot.stacked makes a stacked plot where each y series is plotted on top
  #of the each other using filled polygons
  #
  #Arguments include:
  #'x' - a vector of values
  #'y' - a matrix of data series (columns) corresponding to x
  #'order.method' = c("as.is", "max", "first")
  #  "as.is" - plot in order of y column
  #  "max" - plot in order of when each y series reaches maximum value
  #  "first" - plot in order of when each y series first value > 0
  #'col' - fill colors for polygons corresponding to y columns (will recycle)
  #'border' - border colors for polygons corresponding to y columns (will recycle) (see ?polygon for details)
  #'lwd' - border line width for polygons corresponding to y columns (will recycle)
  #'...' - other plot arguments
  # Source : https://gist.github.com/menugget/7864471#file-plot-stacked-2-r
  # Adapted to use time series.
  # if(sum(y < 0) > 0) stop("y cannot contain negative numbers")

  if(!is.matrix(y)){y= as.matrix(y)}

  if(is.null(border)) border <- par("fg")
  border <- as.vector(matrix(border, nrow=ncol(y), ncol=1))
  density= as.vector(matrix(density, nrow=ncol(y), ncol=1))
  col <- as.vector(matrix(col, nrow=ncol(y), ncol=1))
  lwd <- as.vector(matrix(lwd, nrow=ncol(y), ncol=1))
  lty <- as.vector(matrix(lty, nrow=ncol(y), ncol=1))

  if(order.method == "max") {
    ord <- order(apply(y, 2, which.max))
    y <- y[, ord]
    col <- col[ord]
    border <- border[ord]
    density= density[ord]
    lty= lty[ord]
  }

  if(order.method == "first") {
    ord <- order(apply(y, 2, function(x) min(which(x>0))))
    y <- y[, ord]
    col <- col[ord]
    border <- border[ord]
    density= density[ord]
    lty= lty[ord]
  }

  top.old <- rep(0,length(x))
  polys <- vector(mode="list", ncol(y))
  for(i in seq(polys)){
    top.new <- top.old + y[,i]
    polys[[i]] <- list(x=c(x, rev(x)), y=c(top.old, rev(top.new)))
    top.old <- top.new
  }

  if(is.null(ylim)) ylim <- range(sapply(polys, function(x) range(x$y, na.rm=TRUE)), na.rm=TRUE)
  if(is.null(xlim)){xlim=c(min(x, na.rm = T),max(x, na.rm = T))}
  plot(x,y[,1], ylab=ylab, xlab=xlab, ylim=ylim, t="n",xlim=xlim, ...)
  text(x = xlim[1], y = ylim[2]-(ylim[2]-ylim[1])/10,labels = Fignum, pos = 4)
  for(i in seq(polys)){
    polygon(polys[[i]], border=border[i], col=col[i], lwd=lwd[i], density= density[i],lty=lty[i])
  }

}


r2_fun= function(sim,obs,na.rm= T){
  SStot= sum((obs-mean(obs,na.rm= T))^2, na.rm = na.rm) # total sum of squares
  # SSreg= sum((sim-mean(obs))^2) # explained sum of squares
  SSres= sum((obs-sim)^2, na.rm = na.rm) # residual sum of squares
  rsquared= round(1-(SSres/SStot),3)
  return(rsquared)
}
rmse_fun= function(sim,obs,na.rm= T){round(sqrt(mean((sim-obs)^2,na.rm=na.rm)), 3)}
mse_fun= function(sim,obs,na.rm= T){round(mean((sim-obs)^2,na.rm = na.rm), 3)}


check_C_balance= function(S){
  # Check the C balance for each simulation cycle.
  # Input: S list, from simulation outpout.
  # Output: Percentage of closure:
  # If >100, C_Offer>sum(mortality + exports + Delta dry mass + respiration + reserves)
  print("Computing C balance as : GPP-(Mortality + Delta C Mass + Growth Respiration + Maintenance Respiration)")
  Table_balance=
    S$Table_Day%>%
    # filter(Plot_Age==1)%>%
    # head(.,100)%>%
    group_by(Cycle)%>%
    summarise(
      C_Offer= sum(GPP),
      Exports_Mortality=
        sum(Mact_RsWood+Mact_SCR+
              Mact_Leaf+Mact_FRoot+Overriped_Fruit)+
        sum(Harvest_Fruit,na.rm = T)+sum(Carbon_Lack_Mortality),
      C_Delta= tail(CM_RsWood+CM_SCR+CM_Leaf+CM_FRoot+CM_RE+CM_Fruit,1)-
        head(CM_RsWood+CM_SCR+CM_Leaf+CM_FRoot+CM_RE+CM_Fruit,1),
      Consump_RE= tail(Consumption_RE,1),
      Rm= sum(Rm), Ra=sum(Ra),
      Rc= sum(Rc), RE= sum(NPP_RE))
  Cbalance_Cof= Table_balance$C_Offer-(Table_balance$C_Delta+Table_balance$Rc+Table_balance$Rm+Table_balance$Exports_Mortality)
  print(paste("The carbon balance for Coffee is :", round(Cbalance_Cof,2)))
  print(paste("Wich represents",
              abs(round(1-(Table_balance$C_Delta+Table_balance$Rc+Table_balance$Rm+Table_balance$Exports_Mortality)/
                          Table_balance$C_Offer,5)), "% of the cumulated C Offer"))
  invisible(Cbalance_Cof)
}

F_repartition= function(xi,u_log,s_log){1/(1+exp(-((xi-u_log)/s_log)))}
F_densite= function(xi,u_log,s_log){
  exp(-((xi-u_log)/s_log))/(s_log*(1+exp(-((xi-u_log)/s_log)))^2)
}
F_Integ_Dens= function(x,index,u_log,s_log){
  # Compute integrated density (for not continuous xi)
  F_repartition(x[seq_along(index)+1],u_log,s_log)-
    F_repartition(x[seq_along(index)],u_log,s_log)
}
Sucrose_cont_perc= function(x,a,b,x0,y0){
  # Sucrose accumulates with a logistic increase on fruit throught time
  # Source : Pezzopane et al. (2012)
  # Return : Sucrose content (% total dry mass)
  (y0+a/(1+(x/x0)^b))/100
}



#' Daily growing degree days (GDD)
#'
#' @description Compute the physiological degree days at daily time-step
#'              using the maximum and minimum daily temperature (and optionally using the
#'              average daily temperature, not recommanded).
#'
#' @param Tmax    Maximum daily temperature (Celsius degree)
#' @param Tmin    Minimum daily temperature (Celsius degree)
#' @param MinTT   Minimum temperature threshold, also called base temperature (Celsius degree), default to 5.
#' @param MaxTT   Maximum temperature threshold (Celsius degree), optional, default to NULL
#' @param Round   Boolean. /!\ Important: round the result to 2 decimal, with default to \code{TRUE}.
#' @param Tmean   Optional. Average daily temperature (Celsius degree). Only needed if Tmax and Tmin are missing.
#'
#' @details
#' Please keep in mind that this function gives an approximation of the degree days. GDD are
#' usually computed as the integral of hourly (or less) values.
#' The round argument is provided for convenience, as growing temperatures with less than 3 digits are likely to be
#' within the measurement error, and will probably have no visible effect on plant phenology.
#' Caution, use Tmean only if Tmax and Tmin are not available because it tends to give less powerful approximation.
#'
#'
#' @return \item{GDD}{Growing degree days (Celsius degree)}
#'
#' @examples
#' # Growing degree days over 10 days :
#' # Set the seed :
#' set.seed(1)
#' GDD(Tmax = rnorm(n = 10, mean = 30, sd = 1),
#'     Tmin = rnorm(n = 10, mean = 10, sd = 1),
#'     MinTT = 10, MaxTT = 32)
#'
#' @export
GDD= function(Tmax=NULL,Tmin=NULL,MinTT=5,MaxTT=NULL,Round=T,Tmean=NULL){
  if(!is.null(Tmax)&&!is.null(Tmin)){
    DD= (Tmax+Tmin)/2-MinTT
    DD[((Tmax+Tmin)/2)>MaxTT]= 0
  }else if(!is.null(Tmean)){
    DD= Tmean-MinTT
    DD[Tmean>MaxTT]= 0
  }
  DD[DD<0]= 0
  return(if(Round){round(DD,2)}else{DD})
}




Import_Aquiares_data= function(S=NULL){
  # is S is provided, merge the two data.frames
  Aquiares_df= fread("1-DATA/OutDF.MeteoHydroCFluxes2009to2016.csv", data.table = F)
  Aquiares_df%<>%
    select(DateSemih, DOY, Year, PARtot, PPT= Rain, Tair, Tair3m_GF, VPD,
           GPPLasslop_GF, SEGPPLasslop_GF, LE_GF, Rn.Wm2,
           LAI_coffee_fromNDVI_GF, LAI_Coffee_measured,LAIplot,LAIeryth_towerplot,
           LAIeryth_basin,H_GF)%>%
    group_by(Year,DOY)%>%
    summarise(
      Date= mean(as.POSIXct(DateSemih)),
      # Meteorology:
      PAR= sum(PARtot/4.57*1E-6*3600*24.0/48), # in MJ.m-2.day-1
      Tair_abv= mean(Tair),
      Tair_3m= mean(Tair3m_GF),
      VPD= mean(VPD),

      # Water:
      PPT= sum(PPT),
      # ETR= sum(Convert.Var(LE_GF,"mmol.m-2.s-1_To_mm.semihour")),
      # ETR_W= sum(Convert.Var(LE_GF,"mmol.m-2.s-1_To_W.m-2")*1E-6*3600*24/48),
      ETR= sum(Convert.Var(LE_GF,"W.m-2_To_mm.semihour")),
      # Energy:
      H= sum(H_GF*1E-6*3600*24/48),            # in MJ.m-2.day-1
      Rn= sum(Rn.Wm2*1E-6*3600*24/48),       # in MJ.m-2.day-1
      # Carbon
      GPP= sum(GPPLasslop_GF*30*60*10^-6*12),      # In gC.m-2.day-1
      GPP_SE= sum(SEGPPLasslop_GF*30*60*10^-6*12),      # In gC.m-2.day-1
      # Leaf area index:
      LAI_Cof= mean(LAI_coffee_fromNDVI_GF),
      LAI_Tree= mean(LAIeryth_towerplot),
      LAI_Plot= mean(LAIplot)
      # LAICof_meas= mean(LAI_Coffee_measured),
      # LAIEry_basin= mean(LAIeryth_basin)
    )%>%mutate(Date= as.POSIXct(format(Date, "%Y-%m-%d")))
  if(!is.null(S)){
    S$Table_Day$Date= as.POSIXct(S$Met_c$Date)
    colnames(Aquiares_df)[-c(1:3)]= paste0(colnames(Aquiares_df),"_Meas")[-c(1:3)]
    S$Table_Day= merge(S$Table_Day, Aquiares_df, by = "Date", all.x = T)

    return(S)
  }else{
    attr(Aquiares_df,"Units")= c("Year","Day Of Year","Posixct time","MJ m-2 day-1",
                                 "Celsius degree","Celsius degree","hPa","mm","mm day-1",
                                 "MJ m-2 day-1","MJ m-2 day-1",
                                 "gC m-2 day-1","gC m-2 day-1","m2 leaves m-2 soil",
                                 # "m2 leaves m-2 soil","m2 leaves m-2 soil",
                                 "m2 leaves m-2 soil","m2 leaves m-2 soil")
    return(Aquiares_df)
  }
}


#' Warn or stop execution if mandatory meteorology input variables are not provided
#'
#' @description Help to explain which variable is missing and/or if there are replacements
#'
#' @param Var            Input variable name
#' @param replacement    Replacement variable that is used to compute \code{"Var"}
#' @param type           Type of error to return : either
#' @note This function helps to debug the model when some mandatory meteorological variables
#'       are missing from input: either an error (default), or a warning.
#'       If \code{"replacement"} is not provided in the meteorology file either, this function
#'       will return an error with a hint on which variables can be provided to compute
#'       \code{"Var"}
#'
#' @keywords internal
warn.var= function(Var,replacement,type="error"){
  if(type=="error"){
    stop(paste(Var,"missing from input Meteo. Cannot proceed unless provided.",
               if(!missing(replacement)){
                 paste("Hint:",Var,"can be computed alternatively using",
                       paste(replacement,collapse = " or "),"if provided in Meteo file")
               }),call.=FALSE)
  }else{
    warning(paste(Var,"missing from input Meteo. Computed from",replacement),call.=F)
  }
}


# Overide bigleaf function to make it vectorized :
rH.to.VPD <- function(rH,Tair){
  if(any(rH > 1)){
    warning("relative humidity (rH) has to be between 0 and 1.")
  }
  esat <- Esat.slope(Tair)[,"Esat"]
  VPD  <- esat - rH*esat
  return(VPD)
}




#' American Leaf Spot
#'
#' @description Compute the percentage of leaves on coffee dying from
#'              American Leaf Spot disease, drought excluded.
#'
#' @param Elevation           Site elevation       (m.a.s.l)
#' @param SlopeAzimut         Slope azimuth        (degree)
#' @param Slope               Slope percentage     (\eqn{%})
#' @param RowDistance         Coffee rows distance (m)
#' @param Shade               Shade percentage     (\eqn{%})
#' @param CanopyHeight.Coffee Coffee Height        (m)
#' @param Fertilization       N fertilization per year
#' @param ShadeType           Shade type:  1= Legume only ; 2= bananas and legume ;
#'                                         3= bananas and other plants ; 4=	fruit and
#'                                         forest tree only ; 5= no shade
#' @param CoffeePruning       Character specifying the pruning management.
#'                            Values: tree, row, block or NULL.
#' @param df_rain             Data frame with DOY, year and Rain (mm) values
#'
#' @details It is good practice to use shade tree transmittance to compute \code{Shade} percentage
#'          (\eqn{Shade= 1-Transmittance}).
#'
#'
#' @return \item{ALS}{Percentage of dead leaves by ALS by day (\eqn{%  day-1})}
#'
#' @references Avelino et al. (2007) Topography and Crop Management Are Key Factors
#' for the Development of American Leaf Spot Epidemics on Coffee in Costa Rica
#' File: "Ia_digitized from Avelino 2007 - JA_lineaire.xlsx"
#' @examples
#' # df_rain has this structure :
#' df_rain= data.frame(DOY= 1:365, year= rep(2018,365), Rain= rnorm(n = 365,mean = 5, sd = 1))
#' ALS(Elevation = 1000, df_rain= df_rain)
#'
#' @export
ALS= function(Elevation, SlopeAzimut= 0, Slope=0, RowDistance= 1.5,
              Shade= 0, CanopyHeight.Coffee=2,Fertilization= 3,
              ShadeType= 5, CoffeePruning= c("tree","row","block",NULL),
              df_rain){

  CoffeePruning= match.arg(CoffeePruning)
  Defol_ALS_pc= rep(0,nrow(df_rain))
  ######### American Leaf Spot (ALS) ####
  ##Key Parameters for American Leaf Spot (ALS)

  Ia_Elevation= -0.000000000080801*Elevation^3 -
    0.0000068050865205*Elevation^2+0.0184187089250643*Elevation-
    11.9175485660755

  Ia_SlopeAzimut= 0.0000000000035758*SlopeAzimut^5 - 0.0000000002237767*
    SlopeAzimut^4 -0.0000013880057625*SlopeAzimut^3 +0.000492076366075*
    SlopeAzimut^2-0.046667060826288*SlopeAzimut + 0.492587332814335

  Ia_Slope= -0.0006135200057836*Slope^2+0.0611762490434003*Slope-
    0.96950860406751

  Ia_RowDistance= -1.67586900817065*RowDistance^2 + 4.61297139285148*
    RowDistance - 2.7957057499133

  Ia_Shade= 0.0180902153201516*Shade - 0.218143985209743
  Ia_CoffeeHeight= 0.734362166489921*CanopyHeight.Coffee - 1.36982218159893
  Ia_NbFertiliz= -0.163949361429501*Fertilization + 0.395095964560203

  Ia_ShadeType<-if(ShadeType==1){-0.3}else{
    if(ShadeType==2){-0.2}else{
      if(ShadeType==3){-0.18}else{
        if(ShadeType==4){0.65}else{
          if(ShadeType==5){0.28}}}}}
  Ia_CoffeePruning<-if(CoffeePruning=="tree"){0.05}else{
    if(CoffeePruning=="row"){0.3}else{
      if(CoffeePruning=="block"){-0.35}else{
        if(is.null(CoffeePruning)){0.65}}}}

  # American Leaf Spot :

  ShortDrought15JuneAugust_mm=
    df_rain%>%
    transmute(year, Rain, mid_june_to_mid_august= ifelse(DOY>=166&DOY<=227,T,F))%>%
    group_by(year)%>%
    transmute(SumRainJunetoAugust_Year = sum(Rain*mid_june_to_mid_august))%>%
    .[["SumRainJunetoAugust_Year"]]
  Ia_ShortDrought15JuneAugust= 0.0012200723943985*ShortDrought15JuneAugust_mm - 0.923932085933056

  Sum_Ia_ALS= Ia_Elevation+Ia_SlopeAzimut+Ia_Slope+Ia_RowDistance+Ia_Shade+
    Ia_CoffeeHeight+Ia_NbFertiliz+Ia_ShadeType+Ia_CoffeePruning+Ia_ShortDrought15JuneAugust
  Sum_Ia_ALS[Sum_Ia_ALS<0]= 0
  Sum_Ia_ALS= 0.2797*Sum_Ia_ALS+0.3202


  Defol_ALS_pc[df_rain$DOY>15&df_rain$DOY<166]= 0
  Defol_ALS_pc[df_rain$DOY>=166&df_rain$DOY<=366]=
    Sum_Ia_ALS[df_rain$DOY>=166&df_rain$DOY<=366]*
    exp(0.0180311*(df_rain$DOY[df_rain$DOY>=166&
                                 df_rain$DOY<=366]-166))
  Defol_ALS_pc[df_rain$DOY<=15]=
    Sum_Ia_ALS[df_rain$DOY<=15]*
    exp(0.0180311*(df_rain$DOY[df_rain$DOY<=15]-166+365))

  Defol_ALS= (Defol_ALS_pc-Defol_ALS_pc[previous_i(1:nrow(S$Met_c),n_prev = 1)])/100
  Defol_ALS[Defol_ALS<0]= 0

  return(Defol_ALS)
}






#' Trigonometric Functions (degree angles version)
#'
#' @description These functions give the obvious trigonometric functions.
#'              They respectively compute the cosine, sine, tangent,
#'              arc-cosine, arc-sine, arc-tangent as in the base functions
#'              \code{\link[base]{Trig}}, but use input in degree instead
#'              of radian.
#'
#' @aliases sin_deg tan_deg acos_deg asin_deg atan_deg
#'
#' @param x    Angle in degree
#'
#' @details
#' The conversions between radian to degree is:
#'
#' \deqn{x*pi/180}
#'
#' @examples
#' # cosinus of an angle of 120 degree:
#' cos_deg(120)
#' # should yield -0.5, as in the base version:
#' cos(120*pi/180)
#'
#' @export
cos_deg= function(x){
  cos(x*pi/180)
}


#' @rdname cos_deg
#' @export
sin_deg= function(x){
  sin(x*pi/180)
}

#' @rdname cos_deg
#' @export
tan_deg= function(x){
  tan(x*pi/180)
}

#' @rdname cos_deg
#' @export
acos_deg= function(x){
  acos(x)*180/pi
}

#' @rdname cos_deg
#' @export
asin_deg= function(x){
  asin(x)*180/pi
}

#' @rdname cos_deg
#' @export
atan_deg= function(x){
  atan(x)*180/pi
}



#' Diffuse fraction
#'
#' @description Compute the daily diffuse fraction from the total daily incident radiation
#'
#' @param DOY         Day Of Year from 1st January (day)
#' @param RAD         Incident total radiation (MJ m-2 d-1)
#' @param Latitude    Latitude (deg)
#' @param type        Model type, one of Spitters, Page or Gopinathan (default to Spitters)
#'
#' @details The daily extra-terrestrial radiation at a plane parallel to the earth surface
#'          (\eqn{S0_d} or \eqn{H0} depending on the source) is computed following
#'          Khorasanizadeh and Mohammadi (2016).
#'          The daily diffuse fraction is computed following DB models from :
#' \itemize{
#'   \item Spitters et al. (1986), for de Bilt in Netherlands and stated that their model is
#'         valid for a wide range of climate conditions
#'   \item Page (1967) using the data from 10 widely-spread sites in the 40N to 40S latitude belt
#'   \item Gopinathan and Soler (1995) from 40 widely distributed locations in the latitude range
#'         of 36S to 60N.
#' }
#'
#'
#'
#' @return \item{\eqn{Hd/H}}{Daily diffuse fraction (%)}
#'
#' @references Duffie, J.A. and W.A. Beckman, Solar engineering of thermal processes. 2013: John Wiley & Sons.
#'             Gopinathan, K. and A. Soler, Diffuse radiation models and monthly-average, daily, diffuse data for
#'             a wide latitude range. Energy, 1995. 20(7): p. 657-667.
#'             Kalogirou, S.A., Solar energy engineering: processes and systems. 2013: Academic Press.
#'             Khorasanizadeh, H. and K. Mohammadi, Diffuse solar radiation on a horizontal surface:
#'             Reviewing and categorizing the empirical models. Renewable and Sustainable Energy Reviews,
#'             2016. 53: p. 338-362.
#'             Liu, B.Y.H. and R.C. Jordan, The interrelationship and characteristic distribution of direct,
#'             diffuse and total solar radiation. Solar Energy, 1960. 4(3): p. 1-19.
#'             Page, J. The estimation of monthly mean values of daily total short wave radiation on vertical
#'             and inclined surfaces from sunshine records 40S-40N. in Proceedings of the United Nations
#'             Conference on New Sources of Energy: Solar Energy, Wind Power and Geothermal Energy, Rome, Italy. 1967.
#'             Spitters, C.J.T., H.A.J.M. Toussaint, and J. Goudriaan, Separating the diffuse and direct
#'             component of global radiation and its implications for modeling canopy photosynthesis Part I.
#'             Components of incoming radiation. Agricultural and Forest Meteorology, 1986. 38(1): p. 217-229.

#'
#' @examples
#' # Daily diffuse fraction of january 1st at latitude 35 N, with a RAD of 25 MJ m-2 day-1 :
#' Diffuse_d(DOY= 1,RAD= 25, Latitude= 35)
#'
#' @export
Diffuse_d= function(DOY, RAD, Latitude= 35, type=c("Spitters","Page","Gopinathan")){

  type= match.arg(type)

  TRANS = RAD/Rad_ext(DOY = DOY,Latitude = Latitude)

  if(type=="Spitters"){
    FDIF= rep(0.23,length(TRANS))
    FDIF[TRANS<0.07]= 1.
    FDIF[TRANS>=0.07&TRANS<0.35]= 1. - 2.3*(TRANS[TRANS>=0.07&TRANS<0.35]-0.07)^2
    FDIF[TRANS>=0.35&TRANS<0.75]= 1.33 - 1.46*TRANS[TRANS>=0.35&TRANS<0.75]
  }

  if(type=="Page"){
    FDIF= 1 - 1.13*TRANS
  }

  if(type=="Gopinathan"){
    FDIF= 0.91138 - 0.96225*TRANS
  }

  return(FDIF)
}


#' Daily extra-terrestrial radiation at a plane parallel to the earth surface
#'
#' @description Compute the daily extra-terrestrial radiation at a plane parallel to the
#'              earth surface (\eqn{S0} or \eqn{H0} depending on the source) is computed
#'              following Khorasanizadeh and Mohammadi (2016).
#'
#' @param DOY         Day Of Year from 1st January (day)
#' @param Latitude    Latitude (deg)
#' @param Gsc         The solar constant (W m-2), default to \code{Constants()$Gsc} (=1367).
#'
#' @details The daily extra-terrestrial radiation at a plane parallel to the earth surface
#'          (\eqn{S0_d} or \eqn{H0} depending on the source) is computed following
#'          Khorasanizadeh and Mohammadi (2016).
#'
#' @return \item{\eqn{S0}}{Daily extra-terrestrial radiation (MJ m-2 d-1)}
#'
#' @references Khorasanizadeh, H. and K. Mohammadi, Diffuse solar radiation on a horizontal surface:
#'             Reviewing and categorizing the empirical models. Renewable and Sustainable Energy Reviews,
#'             2016. 53: p. 338-362.
#'
#' @examples
#' # Daily extra-terrestrial radiation on january 1st at latitude 35 N :
#' Rad_ext(DOY= 1,Latitude= 35)
#'
#' @export
Rad_ext= function(DOY,Latitude,Gsc=Constants()$Gsc){
  solar_declin= 23.45*sin_deg(((DOY+284)*360)/365)
  sunset_hour_angle= acos_deg(-tan_deg(Parameters$Latitude)*tan_deg(solar_declin))
  S0= (86400/pi)*Parameters$Gsc*(1+0.033*cos_deg((360*DOY)/365))*
    (cos_deg(Parameters$Latitude)*cos_deg(solar_declin)*sin_deg(sunset_hour_angle)+
       ((pi*sunset_hour_angle)/180)*sin_deg(Parameters$Latitude)*sin_deg(solar_declin))
  return(S0*10^-6)
}





#' Daily net radiation
#'
#' @description Compute the daily net radiation of the system using incident radiation, air
#'              temperature, wind speed, relative humidity and the albedo. A clear description
#'              of this methodology can be found in Allen et al. (1998) or in An et al. (2017).
#'
#' @param DOY         Day Of Year from 1st January (day)
#' @param RAD         Incident daily total radiation (MJ m-2 d-1)
#' @param Tmax        Maximum daily air temperature (celsius degree)
#' @param Tmin        Minimum daily air temperature (celsius degree)
#' @param Rh          Average daily relative humidity (\code{%})
#' @param VPD         Mean daily Vapor Pressure Deficit (hPa), only needed if \code{Rh} is missing
#' @param RAD         Incident total radiation (MJ m-2 d-1)
#' @param Latitude    Latitude (deg)
#' @param Elevation   Elevation (m)
#' @param albedo      Shortwave surface albedo (-)
#' @param sigma       Stefan-Boltzmann constant (W m-2 K-4), default to \code{Constants()$sigma}.
#' @param Gsc         Solar constant (W m-2), default to \code{Constants()$Gsc} (=1367).
#'
#' @details The daily net radiation is computed using the surface albedo. This method is only a
#'          simple estimation. Several parameters (ac, bc, a1 and b1) are taken from
#'          Evett et al. (2011). The net radiation is computed as:
#'          \deqn{Rn= (1-albedo)*RAD-(ac*(RAD/Rso)+bc)*(a1+b1*ea^0.5)*sigma*((Tmax^4+Tmin^4)/2)}
#'          And is derived from the equation :
#'          \deqn{Rn= (1-albedo)*RAD-Rln}
#'          where \eqn{Rln} is the net upward longwave radiation flux.
#'          The actual vapor pressure \deqn{ea} can be computed using either VPD or the relative
#'          humidity and the maximum and minimum daily temperature. If both are provided, Rh will
#'          be used.
#' @return \item{\eqn{Rn}}{Daily net radiation (MJ m-2 d-1)}
#'
#' @references An, N., S. Hemmati, and Y.-J. Cui, Assessment of the methods for determining net
#'             radiation at different time-scales of meteorological variables. Journal of Rock
#'             Mechanics and Geotechnical Engineering, 2017. 9(2): p. 239-246.
#'
#' @examples
#' # Daily net radiation on january 1st at latitude 9 N :
#' Rad_net(DOY= 1,RAD= 5,Tair= 13.9,VPD=1.05,Latitude=9,Elevation=1000,albedo=0.146)
#'
#' @export
Rad_net= function(DOY,RAD,Tmax,Tmin,VPD,Rh=NULL,Latitude,Elevation,albedo,type_ea=c("VPD","Temp"),
                  sigma= Constants()$sigma,Gsc= Constants()$Gsc){
  Rsa= Rad_ext(DOY = DOY,Latitude = Latitude, Gsc = Gsc)
  Rso= (0.75+0.00002*S$Parameters$Elevation)*Rsa

  if(!is.null(Rh)){
    ea= (Rh/100)*((bigleaf::Esat.slope(Tair = Tmax)[,"Esat"]+
                     bigleaf::Esat.slope(Tair = Tmin)[,"Esat"])/2)
  }else if(!is.null(VPD)){
    ea= bigleaf::Esat.slope(
      bigleaf::dew.point(Tair = (Tmax+Tmin)/2, VPD = VPD/10, accuracy = 0.001)
    )[,"Esat"]
  }else{
    stop("Rh or VPD needed")
  }


  ac= 1.35
  bc= -0.35
  a1= 0.35
  b1= -0.14

  Rn= (1-albedo)*RAD-(ac*(RAD/Rso)+bc)*(a1+b1*ea^0.5)*sigma*((Tmax^4+Tmin^4)/2)
  return(Rn)
}


#' Evapotranspiration
#'
#' @description Compute the daily evaporation or transpiration of the surface using the
#'              Penman-Monteith equation.
#'
#' @param Rn          Net radiation (MJ m-2 d-1)
#' @param Wind        Wind speed (m s-1)
#' @param Tair        Air temperature (Celsius degree)
#' @param ZHT         Wind measurement height (m)
#' @param TREEH       Average tree height (m)
#' @param P_Pa        Atmospheric pressure (kPa)
#' @param Gs          Stomatal conductance (mol m-2 s-1)
#' @param VPD         Vapor pressure deficit (kPa)
#' @param Parameters  Constant parameters, default to Constants(), if different values are needed:
#'                    Cp - specific heat of air for constant pressure (J K-1 kg-1) \cr
#'                    Rgas - universal gas constant (J mol-1 K-1) \cr
#'                    Kelvin - conversion degree Celsius to Kelvin \cr
#'                    H2OMW - conversion from kg to mol for H2O (kg mol-1)
#'
#'
#' @details The daily evapotranspiration is computed using the Penman-Monteith equation, and a
#'          set of conductances as :
#'          \deqn{ET= (Delta * Rn*10^6 + rho * Cp * (VPD/10) * GH) / (Delta + gamma * (1 + GH / GV))/\lambda}
#'          where \eqn{\Delta} is the slope of the saturation vapor pressure curve (kPa K-1),
#'          \eqn{\rho} is the air density (kg m-3), GH the canopy boundary layer conductance (m s-1),
#'          \eqn{\gamma} the psychrometric constant (kPa K-1) and \eqn{GV} the boundary + stomatal
#'          conductance to water vapour (m s-1). To simulate evaporation, \eqn{Gs} can be set
#'          to nearly infinite (e.g. \eqn{Gs= 1E09}).
#' @return \item{\eqn{ET}}{Daily evapotranspiration (mm d-1)}
#'
#' @references Allen R.G., Pereira L.S., Raes D., Smith M., 1998: Crop evapotranspiration -
#'              Guidelines for computing crop water requirements - FAO Irrigation and drainage
#'              paper 56.
#'
#' @seealso \code{\link[bigleaf]{reference.ET}} and \href{https://maespa.github.io/}{MAESPA model}
#'
#' @examples
#' # leaf evaporation of a forest :
#' PENMON(Rn= 12, Wind= 0.5, Tair= 16, ZHT= 26, TREEH= 25, Pressure= 900, Gs = 1E09, VPD= 2.41)
#'
#' @export
PENMON= function(Rn,Wind,Tair,ZHT,TREEH,Parameters= Constants(),Pressure,Gs,VPD){

  CMOLAR = (Pressure*100) / (Parameters$Rgas * (Tair+Parameters$Kelvin))
  GB = GBCANMS(WIND = Wind, ZHT = ZHT, TREEH = TREEH)$Canopy*CMOLAR    # in mol m-2 s-1

  # Latent heat of water vapour at air temperature (J mol-1)
  LHV = bigleaf::latent.heat.vaporization(Tair = Tair)*Parameters$H2OMW
  GH = GB

  GV = 1./(1./(Gs) + 1./GB)

  gamma  = bigleaf::psychrometric.constant(Tair = Tair,pressure = Pressure/10)
  Delta  = bigleaf::Esat.slope(Tair = Tair)[,"Delta"]
  rho    = bigleaf::air.density(Tair = Tair,pressure = Pressure/10)

  LE_ref =
    (Delta * Rn*10^6 + rho * Parameters$Cp * (VPD/10) * GH) /
    (Delta + gamma * (1 + GH / GV))

  ET_ref = bigleaf::LE.to.ET(LE_ref,Tair = Tair)
  return(ET_ref)
}


#' Canopy and Soil boundary layer conductance
#'
#' @description This function assumes two aerodynamic conductances in series:
#'  1) from the atmosphere to the canopy, based on Van de Griend (1989), this is actually two conductances,
#'   one in the inertial sublayer and one in the roughness sublayer.
#'  2) within the canopy to the soil, based on Choudhury & Monteith (1988).
#'
#' @param WIND        Average daily windspeed (m s-1)
#' @param ZHT         Wind measurement height (m)
#' @param TREEH       Average tree height (m)
#' @param Z0          Roughness length (m), default to 0.1*TREEH
#' @param ZPD         Zero-plane displacement (m), defaults to 0.75*TREEH
#' @param GBCANMS1MIN Minimum allowed atmosphere to canopy conductance (mol m-2 s-1), default to 0.0123
#' @param VONKARMAN   Con Karman constant, default to \code{Constants()$vonkarman}, 0.41.
#' @details The defaults for Z0 and ZPD are computed very simply. Other simple formulations:
#'          1) Lettau (1969) proposed an other way: \deqn{Z0= 0.5 . h* . s / S} where \code{h*}
#'          is canopy height, s is the average silhouette area (projected area of the tree on a vertical plane)
#'           and S the specific area, with \deqn{S= A/n}, where A is the total plot area and n the number of trees.
#'          2) For ZPD, Verhoef et al. (1997) said \deqn{d= 0.67}. h is a good proxy without any prior knowledge.
#' @return \item{\eqn{GBH}}{Canopy and Soil boundary layer conductance (mol m-2 s-1)}
#'
#' @references Van de Griend (1989), Choudhury & Monteith (1988)
#'
#' @examples
#' # Canop conductance of a forest:
#' GBCANMS(WIND = 3, ZHT = 26, TREEH = 25)$Canopy
#' # And the conductance above its soil layer:
#' GBCANMS(WIND = 3, ZHT = 26, TREEH = 25)$Soil
#'
#' @export
GBCANMS= function(WIND,ZHT,TREEH,Z0=TREEH*0.1,ZPD=TREEH*0.75,GBCANMS1MIN = 0.0123,VONKARMAN= Constants()$vonkarman){
  # Canopy boundary layer conductance (from Jones 1992 p 68)
  # in m s-1
  # GBCANMS1MIN default is GBCANMS1 for WIND= 0.035 and CANOPY HEIGHT at 25 m
  #**********************************************************************

  # In this model, we assumed 2 aerodynamic conductances in series
  # 1) from the atmosphere to the canopy, based on Van de Griend 1989
  #       this is actually 2 conductances, one in the inertial sublayer and one in the roughness sublayer
  # 2) Within the canopy to the soil, based on CHoudhury & Monteith 1988

  # Aerodynamic conductance from the canopy to the atmosphere
  # Formula from Jones 1992 p 68, aerodynamic conductance air-canopy - air, adapted from the CASTANEA model (Dufrene et al., 2005)
  # OLIVER & MAYHEAD (1974) :
  # ZPD = 0.75 * TREEH
  # Z0 = 0.1 *  TREEH
  # ZPD = 0.67 * TREEH
  # Z0 = 0.046 *  TREEH
  # RV: Z0 and ZPD are unknown... Carefull, model is sensible to z0.
  # NB: Lettau (1969) proposed an other way: Z0= 0.5 . h* . s / S where h* is canopy height, s is
  # the average silhouette area (projected area of the tree on a vertical plane) and S the specific
  # area, with S= A/n, where A is the total plot area and n the number of trees.
  # For ZPD, Verhoef et al. (1997) said d= 0.67 . h is a good proxy without any prior knowledge.

  ZSTAR=ZHT
  # ZSTAR can be corrected if lower than maximum tree height (TREEH) because GBCANMS have to be computed
  # for the whole canopy.

  # RV: Observations indicate that z* lies 1–2 times the height of the canopy above the canopy (Garratt 1992, Harman&Finnigan 2007).
  # In their paper z* is the height of the roughness sublayer.


  # RV & GLM 05/2017 : If ZSTAR (=ZHT) is below TREEH (e.g. wind measurements are made under canopy)
  # wind is increased to obtain a wind above the canopy for GBH (Wind in the roughness layer).
  if(ZSTAR<=TREEH){
    WIND = WIND * exp(0.13155 * (TREEH/ZSTAR -1))
    ZSTAR= TREEH*2.0
  }

  # Aerodynamic conductance between the atmosphere and the canopy
  # Reference Wind used in the conductance calculation
  # (this is ustar, the friction velocity)
  WINDSTAR = WIND * VONKARMAN / log((ZSTAR-ZPD)/Z0)

  # We supposed 2 aerodynamic conductances, one in the inertial layer (from ZSTAR to ZW)
  # and another one in the roughness layer from ZSTAR to TREEH
  # According to Van de Griend 1989, we can assumed that :
  ALPHA1 = 1.5
  ZW = ZPD + ALPHA1 * (TREEH-ZPD)

  # Aerodynamic conductance in the inertial sublayer (Van de Griend 1989)
  GBCANMSINI = WINDSTAR*VONKARMAN /(log((ZSTAR - ZPD)/(ZW - ZPD)))
  # Aerodynamic conductance in the roughness layer
  # The roughness layer is located between TREEH and a height ZW, according to
  # GBCANMSROU = WINDSTAR*VONKARMAN * ((ZW - TREEH)/(ZW - ZPD))
  GBCANMSROU = WINDSTAR*VONKARMAN / ((ZW - TREEH)/(ZW - ZPD)) #glm 03/2016

  # Total aerodynamic conductance between the canopy and the atmosphere
  GBCANMS1 = 1/ (1/GBCANMSINI + 1/GBCANMSROU)
  GBCANMS1[GBCANMS1<GBCANMS1MIN]= 0.0123 # RV, GBCANMS1 for WIND= 0.035 and CANOPY HEIGHT at 25 m


  # Aerodynamic conductance between the soil surface to the the canopy, 2nd conductance term from choudhury et al. 1988
  # based on an exponential decrease of wind speed with height
  ALPHA = 2
  Z0HT2 = 0.01

  # Assuming uniform vegetation, the aerodynamic conductivity at the top of the canopy KH,
  # and following Van de Griend 1989
  KH = ALPHA1 * VONKARMAN * WINDSTAR * (TREEH - ZPD)

  # Aerodynamic conductance soil-air below canopy according to Chourdhury et al., 1988
  GBCANMS2 = ALPHA * KH / ( TREEH * exp(ALPHA) * (exp(-ALPHA * Z0HT2/TREEH)  -  exp(-ALPHA * (ZPD+Z0) / TREEH) ) )

  return(list(Canopy= GBCANMS1, Soil= GBCANMS2))
}
