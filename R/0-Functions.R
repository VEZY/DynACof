
#' Find the ith previous index
#'
#' @description Find the ith previous index while avoiding 0 or negative indexes.
#'
#' @param x      Current index
#' @param n_prev Target number of indexes before x
#'
#'
#' @details This function is used to find the nth previous index without making an error with negative
#'          or 0 index.
#'
#' @keywords internal
#'
#' @examples
#' # Find the 10th index before 15:
#' previous_i(15,10)
#' # Find the 10th index before 5:
#' previous_i(5,10)
#' # should return 1 (no index before 1)
#'
#' @export
previous_i= function(x,n_prev){
  x= x-n_prev
  x[x<=0]= 1
  return(x)
}


#' Write output results to disk
#'
#' @description Write the ouptut list to disk either under a unique \code{.RData} file or three separated
#'              files:
#' \itemize{
#'   \item Model Output (csv file)
#'   \item Meteoroloy output (input to the model, csv file)
#'   \item Model parameters (txt file)
#' }
#'
#' @param FinalList        The model output list
#' @param output           Output format. Character. \code{".RData"} if single file, or anything else for
#'                         3 separate file. Default: \code{".RData"}
#' @param Simulation_Name  The name of the simulation. Used for the name of the outputs. Default: \code{NULL}.
#' @param Outpath          The path to the folder to write on.
#' @param ...              Further parameters to pass to \code{\link[data.table]{fwrite}}. Only used if
#'                         \code{output!=".RData"}.
#'
#' @export
write.results= function(FinalList,output=".RData",Simulation_Name= NULL,Outpath= "Outputs",...){

  if(!is.null(Outpath)&&!dir.exists(file.path(Outpath))){
    dir.create(file.path(Outpath))
  }
  if(is.null(Outpath)){Outpath="."}
  if(output!=".RData"){
    data.table::fwrite(FinalList$Table_Day,
           paste0(Outpath,"/",Simulation_Name,".csv"),
           sep=";",row.names = F, col.names = TRUE, ...)
    data.table::fwrite(FinalList$Met_c,
           paste0(Outpath,"/",Simulation_Name,"_Meteorology.csv"),
           sep=";",row.names = F, col.names = TRUE, ...)
    capture.output(FinalList$Parameters,
                   file =  paste0(Outpath,"/",Simulation_Name,"_Parameters",".txt"))
  }else{
    S= FinalList
    save(S,file = paste0(Outpath,"/",Simulation_Name,".RData"))
  }

  message("Simulation results saved in ", file.path(getwd(),Outpath,Simulation_Name), "\n")
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
#'
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
#' @return \item{\eqn{Hd/H}}{Daily diffuse fraction (\%)}
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
  sunset_hour_angle= acos_deg(-tan_deg(Latitude)*tan_deg(solar_declin))
  S0= (86400/pi)*Gsc*(1+0.033*cos_deg((360*DOY)/365))*
    (cos_deg(Latitude)*cos_deg(solar_declin)*sin_deg(sunset_hour_angle)+
       ((pi*sunset_hour_angle)/180)*sin_deg(Latitude)*sin_deg(solar_declin))
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
#' @param Rh          Average daily relative humidity (\code{\%})
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
  Rso= (0.75+0.00002*Elevation)*Rsa

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
#' @param VONKARMAN   Von Karman constant, default to \code{Constants()$vonkarman}, 0.41.
#' @details The defaults for Z0 and ZPD are computed very simply. Other simple formulations:
#'          1) Lettau (1969) proposed an other way: \deqn{Z0= 0.5 . h* . s / S} where \code{h*}
#'          is canopy height, s is the average silhouette area (projected area of the tree on a vertical plane)
#'           and S the specific area, with \deqn{S= A/n}, where A is the total plot area and n the number of trees.
#'          2) For ZPD, Verhoef et al. (1997) said \deqn{d= 0.67}. h is a good proxy without any prior knowledge.
#' @return A list of three :
#'         \item{ustar}{The friction velocity (m s-1)}
#'         \item{\eqn{Canopy}}{Atmosphere to canopy boundary layer conductance (mol m-2 s-1)}
#'         \item{Soil}{Canopy to soil boundary layer conductance (mol m-2 s-1)}
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
  # According to Van de Griend 1989, we can assume that :
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

  return(list(ustar= WINDSTAR,Canopy= GBCANMS1, Soil= GBCANMS2))
}


# Overwrite bigleaf function to make it vectorized :
rH.to.VPD <- function(rH,Tair){
  if(any(rH > 1)){
    warning("relative humidity (rH) has to be between 0 and 1.")
  }
  esat <- Esat.slope(Tair)[,"Esat"]
  VPD  <- esat - rH*esat
  return(VPD)
}


#' Get the average wind speed at center of canopy layer
#'
#' @description Calculate the wind speed decrease in two steps:
#' \enumerate{
#'   \item Decrease the measured wind speed from measurement height until top of the canopy
#'         using the formulation of Van de Griend and Van Boxel (1989)
#'   \item Decrease wind speed further with increasing canopy depth using an exponential
#'         extinction coefficient and a cumulated LAI above the target point.
#' }
#'
#' @param Wind      Above canopy wind speed (m s-1)
#' @param LAI_lay   Leaf area index of the layer (m2 leaves m-2 soil)
#' @param LAI_abv   Cumulated leaf area index above the layer (m2 leaves m-2 soil)
#' @param extwind   Extinction coefficient. Default: \code{0}, no extinction.
#' @param Z_top     Average canopy height of the taller crop (m)
#' @param ZHT       Wind measurement height (m)
#' @param Z0        Roughness length (m). Default: \code{0.1*Z_top}
#' @param ZPD       Zero-plane displacement (m), Default: \code{0.75*Z_top}
#' @param alpha     Constant for diffusivity at top canopy. Default: \code{1.5} following
#'                  Van de Griend et al (1989).
#' @param ZW        Top height of the roughness sublayer (m). Default: \code{ZPD+alpha*(Z2-ZPD)}
#' @param vonkarman Von Karman constant, default to \code{Constants()$vonkarman}, 0.41.
#'
#' @details As the function computes the average wind speed at the center of the canopy layer, it
#'          uses the \code{LAI_lay} parameter to add half of the target layer to the cumulated LAI
#'          above:
#'          \deqn{WindLay=Wh*e^{^{\left(-extwind*\left(LAI_{abv}+\frac{LAI_{lay}}{2}\right)\right)}}}{
#'          WindLay= Wh*e(-extwind*(LAI_abv+LAI_lay/2)}
#'          with \code{Wh} the wind speed at top of the canopy.
#'          Note: the \code{alpha} parameter can also be computed as:
#'          \deqn{alpha=\frac{zw-d}{Z2-d}}{alpha= (zw-d)/(Z2-d)}
#' @return \item{WindLay}{The winspeed at the center of the layer (m s-1)}
#'
#' @references Van de Griend, A.A. and J.H. Van Boxel, Water and surface energy balance model
#'             with a multilayer canopy representation for remote sensing purposes. Water
#'             Resources Research, 1989. 25(5): p. 949-971.
#'             Part of the code is taken from the \href{https://maespa.github.io}{MAESPA model}.
#'
#' @examples
#' # Windspeed in a coffee layer managed in agroforestry system
#' GetWind(Wind=3,LAI_lay=4,LAI_abv=0.3,extwind= 0.58,Z_top = 24,ZHT = 25)
#'
#' @export
GetWind= function(Wind,LAI_lay,LAI_abv,extwind=0,Z_top,ZHT,Z0=Z_top*0.1,
                  ZPD=Z_top*0.75,alpha=1.5,ZW=ZPD+alpha*(Z_top-ZPD),
                  vonkarman=Constants()$vonkarman){
  if(ZHT<Z_top){
    warning("Measurement height lower than canopy height (ZHT < Z_top), forcing ZHT > Z_top")
    ZHT= 1.01*Z_top
  }
  Ustar = Wind*vonkarman/log((ZHT-ZPD)/Z0) # by inverting eq.41 from Van de Griend
  # Wind at the top of the canopy:
  Uh= (Ustar/vonkarman)*log((ZW-ZPD)/Z0)-(Ustar/vonkarman)*(1-((Z_top-ZPD)/(ZW-ZPD)))

  LAI_abv = LAI_lay/2+LAI_abv
  WindLay= Uh*exp(-extwind*LAI_abv)

  return(WindLay)
}


#' Bulk aerodynamic conductance
#'
#' @description Compute the aerodynamic conductance for sensible and
#'              latent heat above the canopy following Van de Griend and
#'              Van Boxel (1989).
#'
#' @param Wind      Average daily wind speed above canopy (m s-1)
#' @param ZHT       Wind measurement height (m)
#' @param Z_top     Average canopy height of the taller crop (m)
#' @param Z0        Roughness length (m). Default: \code{0.1*Z_top}
#' @param ZPD       Zero-plane displacement (m), Default: \code{0.75*Z_top}
#' @param alpha     Constant for diffusivity at top canopy. Default: \code{1.5} following
#'                  Van de Griend et al (1989).
#' @param ZW        Top height of the roughness sublayer (m). Default: \code{ZPD+alpha*(Z_top-ZPD)}
#' @param LAI       Leaf area index of the upper layer (m2 leaf m-2 soil)
#' @param extwind   Extinction coefficient. Default: \code{0}, no extinction.
#' @param vonkarman Von Karman constant, default to \code{Constants()$vonkarman}, 0.41.

#' @details \code{alpha} can also be computed as:
#'          \deqn{alpha=\frac{zw-d}{Z_top-d}}{alpha= (zw-d)/(Z_top-d)}
#'          The bulk aerodynamic conductance \eqn{ga_bulk}{ga_bulk} is computed as follow:
#'          \deqn{ga_bulk=\frac{1}{r1+r2+r3}}{ga_bulk= 1/(r1+r2+r3)}
#'          where \code{r1}, \code{r2} and \code{r3} are the aerodynamic resistances of the inertial
#'          sublayer, the roughness sublayer and the top layer of the canopy respectively. Because
#'          wind speed measurements are more often made directly in the roughness sublayer, the
#'          resistance in the inertial sublayer \code{r1} is set to \code{0} though. \code{r2} and
#'          \code{r3} are computed using the equation 43 of Van de Griend and Van Boxel (refer to
#'          the pdf version of the help file for Latex rendering) :
#'          \deqn{r2=\int_{zh}^{zw}\frac{1}{K''}}{r2= Integral{zh,zw}(1/K'')dz}
#'          with \deqn{K''= kU_*(z_w-d)}{K''= k x Ustar x (zw-d)}.
#'          And:
#'          \deqn{r3=\int_{(z2+z1)/2}^{zh}\frac{1}{K'''}\mathrm{d}z}{r3= Integral{z2+z1)/2,zh}(1/K''')dz}
#'          with \deqn{K'''= U_z\frac{K_h}{U_h}}{K'''= Uz x (Kh/Uh)}.
#'
#'          Integration of \code{r2} and \code{r3} integral equations give:
#'          \deqn{\frac{(\ln(ZPD-ZW)^2-\ln(ZPD-Z2)^2)}{(2kU_*)}}{
#'          r2= (log((ZPD-ZW)^2)-log((ZPD-Z2)^2))/(2*vonkarman*Ustar)}
#'          simplified in:
#'          \deqn{r2= \frac{1}{kU_*}\ln(\frac{ZPD-ZW}{ZPD-Z2})}{r2= (1/(vonkarman*Ustar))*log((ZPD-ZW)/(ZPD-Z2))}
#'          and   \deqn{r3= \frac{Uh}{Kh}\ln(\frac{Uh}{U_{interlayer}})}{r3= (Uh/Kh)*log(Uh/U_interlayer)}
#'
#' @return \item{G_bulk}{The bulk aerodynamic conductance (m s-1)}
#'
#' @references Van de Griend, A.A. and J.H. Van Boxel, Water and surface energy balance model
#'             with a multilayer canopy representation for remote sensing purposes. Water
#'             Resources Research, 1989. 25(5): p. 949-971.
#'
#' @seealso \code{\link{G_interlay}} and \code{\link{GetWind}}, which is used internaly.
#'
#' @examples
#' # The bulk aerodynamic conductance for a coffee plantation managed in agroforestry system:
#' G_bulk(Wind=3,ZHT=25,Z_top=24,LAI = 0.5,extwind = 0.58)
#'
#' @export
G_bulk= function(Wind,ZHT,Z_top,Z0=Z_top*0.1,ZPD=Z_top*0.75,alpha=1.5,ZW=ZPD+alpha*(Z_top-ZPD),
                 LAI,extwind=0,vonkarman=Constants()$vonkarman){

  Ustar = Wind*vonkarman/log((ZHT-ZPD)/Z0) # by inverting eq.41 from Van de Griend
  Kh= alpha*vonkarman*Ustar*(Z_top-ZPD)
  Uw= (Ustar/vonkarman)*log((ZW-ZPD)/Z0)
  Uh= Uw-(Ustar/vonkarman)*(1-((Z_top-ZPD)/(ZW-ZPD)))
  r1= 0
  # r2= (log((ZPD-ZW)^2)-log((ZPD-Z_top)^2))/(2*vonkarman*Ustar)
  r2= (1/(vonkarman*Ustar))*log((ZPD-ZW)/(ZPD-Z_top))
  # r2= (1/(vonkarman*Ustar))*((ZW-Z_top)/(ZW-ZPD)) # this equation is found in Van de Griend
  # but it is wrong.
  U_inter= GetWind(Wind= Wind,LAI_lay= 0, LAI_abv= LAI/2,extwind= extwind,
                   Z_top= Z_top, ZHT= ZHT, Z0= Z0, ZPD= ZPD,alpha= alpha, ZW= ZW)

  r3= (Uh/Kh)*log(Uh/U_inter)
  ga_bulk= 1/(r1 + r2 +r3)

  return(ga_bulk)
}



#' Canopy layer to canopy layer aerodynamic conductance
#'
#' @description Compute the aerodynamic conductance for sensible and
#'              latent heat between canopy layers following Van de Griend
#'              and Van Boxel (1989).
#'
#' @param Wind      Average daily wind speed above canopy (m s-1)
#' @param ZHT       Wind measurement height (m)
#' @param Z_top     Average canopy height of the taller crop (m)
#' @param Z0        Roughness length (m). Default: \code{0.1*Z_top}
#' @param ZPD       Zero-plane displacement (m), Default: \code{0.75*Z_top}
#' @param alpha     Constant for diffusivity at top canopy. Default: \code{1.5} following
#'                  Van de Griend et al (1989).
#' @param ZW        Top height of the roughness sublayer (m). Default: \code{ZPD+alpha*(Z_top-ZPD)}
#' @param LAI_top   Leaf area index of the upper layer (m2 leaf m-2 soil).
#' @param LAI_bot   Leaf area index of the layer below the upper layer (m2 leaf m-2 soil).
#' @param extwind   Extinction coefficient. Default: \code{0}, no extinction.
#' @param vonkarman Von Karman constant, default to \code{Constants()$vonkarman}, 0.41.

#' @details \code{alpha} can also be computed as:
#'          \deqn{alpha=\frac{zw-d}{Z_top-d}}{alpha= (zw-d)/(Z_top-d)}
#'          The aerodynamic conductance between canopy layers is computed as:
#'          \deqn{g_{af}= \frac{1}{\frac{U_h}{K_h}\ln(U_{mid}/U_{inter})}}{g_af= 1/((Uh/Kh)*log(U_mid/U_inter))}
#'          where usually \eqn{U_mid} is the wind speed at median cumulated LAI between
#'          the top and the soil, and \eqn{U_inter} the wind speed at height between
#'          the two canopy layers. In this function, \eqn{U_mid} and \eqn{U_inter} are computed
#'          relative to the leaf area instead of the height of the vegetation layers.
#'
#' @return \item{g_af}{The aerodynamic conductance of the air between two canopy layers (m s-1)}
#'
#' @references Van de Griend, A.A. and J.H. Van Boxel, Water and surface energy balance model
#'             with a multilayer canopy representation for remote sensing purposes. Water
#'             Resources Research, 1989. 25(5): p. 949-971.
#'
#' @seealso \code{\link{G_bulk}} and \code{\link{GetWind}}, which is used internaly.
#'
#' @examples
#' # G_af for a coffee plantation managed in agroforestry system:
#' G_interlay(Wind = 3,ZHT = 25,Z_top = 2,LAI_top = 0.5,LAI_bot = 4)
#'
#'
#' @export
G_interlay= function(Wind,ZHT,Z_top,Z0=Z_top*0.1,ZPD=Z_top*0.75,alpha=1.5,ZW=ZPD+alpha*(Z_top-ZPD),
                     LAI_top,LAI_bot,extwind=0,vonkarman=Constants()$vonkarman){

  Ustar = Wind*vonkarman/log((ZHT-ZPD)/Z0) # by inverting eq.41 from Van de Griend
  Kh= alpha*vonkarman*Ustar*(Z_top-ZPD)
  Uw= (Ustar/vonkarman)*log((ZW-ZPD)/Z0)
  Uh= Uw-(Ustar/vonkarman)*(1-((Z_top-ZPD)/(ZW-ZPD)))
  # U_inter is computed using LAI instead of height, so (z1+Z_top)/2 become
  # LAI_top/2
  U_inter= GetWind(Wind= Wind,LAI_lay= 0, LAI_abv= LAI_top/2,extwind= extwind,
                   Z_top= Z_top, ZHT= ZHT, Z0= Z0, ZPD= ZPD,alpha= alpha, ZW= ZW)
  # U_mid is computed using LAI instead of height, so Z_top/2 become
  # (LAI_top+LAI_bot)/2, (LAI_top+LAI_bot) for top layer.
  U_mid= GetWind(Wind= Wind,LAI_lay= 0, LAI_abv= (LAI_top+LAI_bot)/2,extwind= extwind,
                 Z_top= Z_top, ZHT= ZHT, Z0= Z0, ZPD= ZPD,alpha= alpha, ZW= ZW)

  g_af= 1/((Uh/Kh)*log(U_inter/U_mid))

  return(g_af)
}


#' Canopy to soil aerodynamic conductance
#'
#' @description Compute the aerodynamic conductance for sensible and
#'              latent heat between the center of the lowest canopy layer
#'              and the soil surface following Van de Griend and Van Boxel (1989).
#'
#' @param Wind      Average daily wind speed above canopy (m s-1)
#' @param ZHT       Wind measurement height (m)
#' @param Z_top        Average canopy height of the taller crop (m)
#' @param Z0        Roughness length (m). Default: \code{0.1*Z_top}
#' @param ZPD       Zero-plane displacement (m), Default: \code{0.75*Z_top}
#' @param alpha     Constant for diffusivity at top canopy. Default: \code{1.5} following
#'                  Van de Griend et al (1989).
#' @param ZW        Top height of the roughness sublayer (m). Default: \code{ZPD+alpha*(Z_top-ZPD)}
#' @param LAI_top   Leaf area index of the upper layer (m2 leaf m-2 soil).
#' @param LAI_bot   Leaf area index of the layer below the upper layer (m2 leaf m-2 soil).
#' @param extwind   Extinction coefficient. Default: \code{0}, no extinction.
#' @param vonkarman Von Karman constant, default to \code{Constants()$vonkarman}, 0.41.

#' @details \code{alpha} can also be computed as:
#'          \deqn{alpha=\frac{zw-d}{Z_top-d}}{alpha= (zw-d)/(Z_top-d)}
#'          The aerodynamic conductance between the lowest canopy layer and the soil
#'          is computed as:
#'          \deqn{g_{a0}= \frac{1}{\frac{U_h}{K_h}\ln(U_{mid}/U_{0})}}{g_a0= 1/((Uh/Kh)*log(U_mid/U_0))}
#'          where \eqn{U_mid} is the wind speed at median cumulated LAI between the top and the soil, and
#'          \eqn{U_0} the wind speed at soil surface.
#' @return \item{g_a0}{The aerodynamic conductance of the air between the lowest canopy layer
#'                     and the soil surface (m s-1)}
#'
#' @references Van de Griend, A.A. and J.H. Van Boxel, Water and surface energy balance model
#'             with a multilayer canopy representation for remote sensing purposes. Water
#'             Resources Research, 1989. 25(5): p. 949-971.
#'
#' @seealso \code{\link{G_bulk}} and \code{\link{GetWind}}, which is used internaly.
#'
#' @examples
#' # G_a0 for a coffee plantation managed in agroforestry system:
#' G_soilcan(Wind=1,ZHT=25,Z1= site()$Height_Coffee,Z_top=24,
#'  LAI_top = 0.5,LAI_bot = 4,extwind=0.58)
#'
#' @export
G_soilcan= function(Wind,ZHT,Z_top,Z0=Z_top*0.1,ZPD=Z_top*0.75,alpha=1.5,ZW=ZPD+alpha*(Z_top-ZPD),
                     LAI_top,LAI_bot,extwind=0,vonkarman=Constants()$vonkarman){
  Ustar = Wind*vonkarman/log((ZHT-ZPD)/Z0) # by inverting eq.41 from Van de Griend
  Kh= alpha*vonkarman*Ustar*(Z_top-ZPD)
  Uw= (Ustar/vonkarman)*log((ZW-ZPD)/Z0)
  Uh= Uw-(Ustar/vonkarman)*(1-((Z_top-ZPD)/(ZW-ZPD)))
  U_mid= GetWind(Wind= Wind,LAI_lay= 0, LAI_abv= (LAI_top+LAI_bot)/2,extwind= extwind,
                 Z_top= Z_top, ZHT= ZHT, Z0= Z0, ZPD= ZPD,alpha= alpha, ZW= ZW)
  U_0= GetWind(Wind= Wind,LAI_lay= 0, LAI_abv= LAI_top+LAI_bot,extwind= extwind,
               Z_top= Z_top, ZHT= ZHT, Z0= Z0, ZPD= ZPD,alpha= alpha, ZW= ZW)

  g_a0= 1/((Uh/Kh)*log(U_mid/U_0))

  return(g_a0)
}



#' Leaf boundary layer conductance for heat
#'
#' @description Compute the bulk leaf boundary layer conductance for heat using
#'              the wind speed, the leaf dimension, and leaf area distribution
#'              following Jones (1992) or Leuning et al. (1995).
#'
#' @param Wind        Average daily wind speed above canopy (m s-1)
#' @param wleaf       Average leaf width (m)
#' @param LAI_lay     Leaf area index of the layer (m2 leaves m-2 soil)
#' @param LAI_abv     Cumulated leaf area index above the layer (m2 leaves m-2 soil)
#' @param extwind     Extinction coefficient. Default: \code{0}, no extinction.
#' @param Z_top          Average canopy height of the taller crop (m)
#' @param ZHT         Wind measurement height (m)
#' @param Z0          Roughness length (m). Default: \code{0.1*Z_top}
#' @param ZPD         Zero-plane displacement (m), Default: \code{0.75*Z_top}
#' @param alpha       Constant for diffusivity at top canopy. Default: \code{1.5} following
#'                    Van de Griend et al (1989).
#' @param ZW          Top height of the roughness sublayer (m). Default: \code{ZPD+alpha*(Z_top-ZPD)}
#' @param Tleaf       Leaf temperature (deg C). Only needed if \code{formulation="Leuning_1995"}
#' @param Tair        Canopy air temperature (deg C). Only needed if \code{formulation="Leuning_1995"}
#' @param Dheat       Molecular diffusivity for heat (m2 s-1). Default to \code{Constants()$Dheat}.
#'                    Only needed if \code{formulation="Leuning_1995"}
#' @param formulation The formulation used to compute \eqn{Gb_h}

#' @details The leaf boundary layer conductance for heat can be transformed into leaf boundary
#'          layer conductance for water vapour as follow:
#'          \deqn{Gb_w= 1.075*gb_h}
#'          Note that \eqn{Gb_w} should be doubled for amphistomatous plants (stomata on
#'          both sides of the leaves).
#'
#' @return \item{Gb}{The leaf boundary layer conductance for heat (m s-1)}
#'
#' @references \itemize{
#'   \item Leuning, R., et al., Leaf nitrogen, photosynthesis, conductance and transpiration:
#'         scaling from leaves to canopies. Plant, Cell & Environment, 1995. 18(10): p. 1183-1200.
#'   \item Mahat, V., D.G. Tarboton, and N.P. Molotch, Testing above‐ and below‐canopy represetations
#'         of turbulent fluxes in an energy balance snowmelt model. Water Resources Research, 2013.
#'         49(2): p. 1107-1122.
#' }
#'
#' @seealso \code{\link{G_bulk}}, \code{\link{G_soilcan}}, \code{\link{G_interlay}} and
#'          \code{\link{GetWind}}, which is used internaly.
#'
#' @examples
#' # Gb for a coffee plantation managed in agroforestry system:
#' Gb_h(Wind=3,wleaf=0.068,LAI_lay=4,LAI_abv=0.5,ZHT=25,Z_top=24,extwind=0.58)
#'
#' @export
Gb_h= function(Wind,wleaf=0.068,LAI_lay,LAI_abv,extwind=0,Z_top,ZHT,
               Z0=Z_top*0.1,ZPD=Z_top*0.75,alpha=1.5,ZW=ZPD+alpha*(Z_top-ZPD),
               Tleaf=NULL,Tair=NULL,Dheat=Constants()$Dheat,
               formulation=c("Jones_1992","Leuning_1995")){
  formulation= match.arg(formulation)
  U_z= GetWind(Wind= Wind,LAI_lay= LAI_lay, LAI_abv= LAI_abv,extwind= extwind,
               Z_top= Z_top, ZHT= ZHT, Z0= Z0, ZPD= ZPD,alpha= alpha, ZW= ZW)

  if(formulation=="Jones_1992"){
    Gb= 0.01*sqrt(U_z/wleaf)
    # Gb= (0.02/extwind)*sqrt(U_h/wleaf)*(1-exp(-extwind/2)) # integrated over the profile
  }else{
    Gb= 1/(1/Gb_h_Free(Tair= Tair, Tleaf= Tleaf, wleaf= wleaf, Dheat= Dheat))+
      (1/Gb_h_Forced(Wind = Wind, wleaf = wleaf))
  }

  return(Gb)
}



#' Leaf boundary layer conductance for heat under forced and free convection
#'
#' @description Compute the boundary layer conductance for heat - single sided, forced or
#'              free convection using the Leuning et al. (1995) equations.
#'
#' @param Tair   Average daily air temperature (deg C)
#' @param Tleaf  Average daily leaf temperature (deg C)
#' @param Wind   Wind speed (m s-1). Default to \code{0.068}
#' @param wleaf  Leaf width (m)
#' @param Dheat  Molecular diffusivity for heat (m2 s-1). Default to: \code{Constants()$Dheat}.
#'
#' @aliases Gb_h_Forced Gb_h_Free
#'
#' @details
#'
#' @return \item{\eqn{Gb_h_Forced}}{Leaf boundary layer conductance for heat under forced convection (m s-1)}
#'         \item{\eqn{Gb_h_Free}}{Leaf boundary layer conductance for heat under free convection (m s-1)}
#' @references Leuning, R., et al., Leaf nitrogen, photosynthesis, conductance and transpiration: scaling from
#'             leaves to canopies. Plant, Cell & Environment, 1995. 18(10): p. 1183-1200.
#'
#' @seealso The \href{https://maespa.github.io/}{MAESPA model}, from which both functions are taken (FORTRAN code).
#'          The function from which both are usually called internally: \code{\link{Gb_h}}
#'
#' @examples
#' Gb_h_Forced(Wind=3)
#' Gb_h_Free(Tair= 25,Tleaf= 26)
#'
#' @export
Gb_h_Forced= function(Wind,wleaf= 0.068){
  GBHFORCED = 0.003 * sqrt(Wind/wleaf)

  return(GBHFORCED)
}

#' @rdname Gb_h_Forced
#' @export
Gb_h_Free= function(Tair,Tleaf,wleaf= 0.068,Dheat=Constants()$Dheat){
  GRASHOF= 1.6E8 * abs(Tleaf-Tair) * (wleaf**3.)
  GBHFREE= 0.5*Dheat*(GRASHOF**0.25)/wleaf
  GBHFREE[(Tleaf-Tair)==0]=0

  return(GBHFREE)
}




#' Logistic function helpers
#'
#' @description Compute the distribution, density and delta of the
#'              logistic function
#'
#' @param xi     X vector
#' @param u_log  Inflexion point (x-value of the sigmoid's midpoint)
#' @param s_log  Steepness of the curve
#' @param index  Index at which to compute the delta between x's
#'
#' @aliases F_densite F_Integ_Dens
#'
#' @details
#'
#' @return \item{F_repartition}{Logistic function distibution}
#'         \item{F_densite}{Logistic function density}
#'         \item{F_Integ_Dens}{Delta between logistic function x's}
#'
#' @seealso More informations can be found in \href{https://en.wikipedia.org/wiki/Logistic_function}{
#'             the wikipedia page}
#'
#' @keywords internal
#'
#' @examples
#' F_repartition(1:10,5,0.1)
#' F_densite(1:10,5,0.1)
#' F_Integ_Dens(1:10,10,5,0.1)
#'
#' @export
F_repartition= function(xi,u_log,s_log){1/(1+exp(-((xi-u_log)/s_log)))}

#' @rdname F_repartition
#' @export
F_densite= function(xi,u_log,s_log){
  exp(-((xi-u_log)/s_log))/(s_log*(1+exp(-((xi-u_log)/s_log)))^2)
}

#' @rdname F_repartition
#' @export
F_Integ_Dens= function(x,index,u_log,s_log){
  F_repartition(x[seq_along(index)+1],u_log,s_log)-
    F_repartition(x[seq_along(index)],u_log,s_log)
}


#' Fruit sucrose accumulation
#'
#' @description Logistic sucrose accumulation into coffee fruits through time
#'
#' @param x      Cumulated degree days
#' @param a      Parameters
#' @param b      Parameters
#' @param x0     Mid-maturation (logistic function inflexion point)
#' @param y0     Sucrose content at the beginning (in \%, 1-100)
#'
#' @return \item{Sucrose content}{(In \% of fruit total dry mass)}
#'
#' @references Pezzopane et al. (2012) :
#' Pezzopane, J., et al., Agrometeorological parameters for prediction of the maturation period
#' of Arabica coffee cultivars. International Journal of Biometeorology, 2012. 56(5): p. 843-851.
#'
#' @examples
#' Sucrose_cont_perc(1:10,5.3207,-28.5561,191,3.5)
#'
#' @export
Sucrose_cont_perc= function(x,a,b,x0,y0){
  (y0+a/(1+(x/x0)^b))/100
}




#' Continuous percentage of living tissue distribution
#'
#' @description Help compute the percentage of living tissue in organs according to age
#'
#' @param Age_Max Maximum age of the organ (year)
#' @param P_Start Percentage of living tissue at first age (\% of dry mass)
#' @param P_End   Percentage of living tissue at last age (\% of dry mass)
#' @param k       Rate between P_Start and P_End
#'
#' @return \item{Living tissue at each age}{(In \% of organ dry mass)}
#'
#' @details The percentage of living tissue is computed as follows:
#'  \deqn{P_End+((P_Start-P_End)*exp(seq(0,-k,length.out = Age_Max)))}
#'
#' @examples
#' Paliv_dis(40,1,0.05,5)
#'
#' @export
Paliv_dis= function(Age_Max,P_Start,P_End,k){
  data.frame(Age= 1:Age_Max,
             PaliveStem_Tree= round(P_End+((P_Start-P_End)*exp(seq(0,-k,length.out = Age_Max))),3))
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

