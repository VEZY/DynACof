#' Import Meteorology for model input
#'
#' @description Import the meteorology data, check its format, and eventually compute missing variables.
#'
#' @param file        Either the file name to read, a shell command that preprocesses the file (e.g. fread("grep filename"))
#'                    or the input itself as a string, see [data.table::fread()]. In both cases, a length 1 character string.
#'                    A filename input is passed through path.expand for convenience and may be a URL starting http:// or file://.
#'                    Default to `NULL` to return the [Aquiares()] example data from the package.
#' @param Period      A vector of two POSIX dates that correspond to the min and max dates for the desired time period to be returned.
#'
#' @param Parameters  A list of parameters:
#' \itemize{
#'   \item Start_Date: optional, the Posixct date of the first meteo file record. Only needed if the Date column is missing.
#'   \item FPAR      : Fraction of global radiation corresponding to PAR radiation, only needed if either RAD or PAR is missing.
#'   \item Elevation : elevation of the site (m), only needed if atmospheric pressure is missing
#'   \item Latitude  : latitude of the site (degree), only needed if the diffuse fraction of light is missing
#'   \item WindSpeed : constant wind speed (m s-1), only needed if windspeed is missing
#'   \item CO2       : constant atmospheric \eqn{CO_2} concentration (ppm), only needed if \eqn{CO_2} is missing
#'   \item MinTT     : minimum temperature threshold for degree days computing (Celsius), see [GDD()]
#'   \item MaxTT     : maximum temperature threshold for degree days computing (Celsius), see [GDD()]
#'   \item albedo    : site shortwave surface albedo, only needed if net radiation is missing, see [Rad_net()]
#' }
#'
#'
#' @details The imported file is expected to be at daily time-step.
#'          The albedo is used to compute the system net radiation that is then used to compute the soil net radiation using an
#'          extinction coefficient with the plot LAI following the Shuttleworth & Wallace (1985) formulation. This computation is
#'          likely to be depreciated in the near future as the computation has been replaced by a metamodel. It is kept for
#'          information for the moment.
#' \tabular{llll}{
#' \strong{Var}    \tab \strong{unit} \tab \strong{Definition}                        \tab \strong{If missing} \cr
#' Date            \tab POSIXct     \tab Date in POSIXct format                       \tab Computed from start date parameter, or set a dummy date if missing \cr
#' year            \tab year        \tab Year of the simulation                       \tab Computed from Date \cr
#' DOY             \tab day         \tab day of the year                              \tab Computed from Date \cr
#' Rain            \tab mm          \tab Rainfall                                     \tab Assume no rain \cr
#' Tair            \tab Celsius     \tab Air temperature (above canopy)               \tab Computed from Tmax and Tmin \cr
#' Tmax            \tab Celsius     \tab Maximum air temperature during the day       \tab Required (error) \cr
#' Tmin            \tab Celsius     \tab Minimum air temperature during the day       \tab Required (error) \cr
#' RH              \tab \%          \tab Relative humidity                            \tab Not used, but prefered over VPD for Rn computation \cr
#' RAD             \tab MJ m-2 d-1  \tab Incident shortwave radiation                 \tab Computed from PAR \cr
#' Pressure        \tab hPa         \tab Atmospheric pressure                         \tab Computed from VPD, Tair and Elevation, or alternatively from Tair and Elevation. \cr
#' WindSpeed       \tab m s-1       \tab Wind speed                                   \tab Taken as constant: `Parameters$WindSpeed` \cr
#' CO2             \tab ppm         \tab Atmospheric CO2 concentration                \tab Taken as constant: `Parameters$CO2` \cr
#' DegreeDays      \tab Celsius     \tab Growing degree days                          \tab Computed using [GDD()] \cr
#' PAR             \tab MJ m-2 d-1  \tab Incident photosynthetically active radiation \tab Computed from RAD \cr
#' FDiff           \tab Fraction    \tab Diffuse light fraction                       \tab Computed using [Diffuse_d()] using Spitters et al. (1986) formula \cr
#' VPD             \tab hPa         \tab Vapor pressure deficit                       \tab Computed from RH \cr
#' Rn              \tab MJ m-2 d-1  \tab Net radiation (will be depreciated)          \tab Computed using [Rad_net()] with RH, or VPD \cr
#' DaysWithoutRain \tab day         \tab Number of consecutive days with no rainfall  \tab Computed from Rain \cr
#' Air_Density     \tab kg m-3      \tab Air density of moist air (\eqn{\rho}) above canopy \tab Computed using [bigleaf::air.density()] \cr
#' ZEN             \tab radian      \tab Solar zenithal angle at noon                 \tab Computed from Date, Latitude, Longitude and Timezone
#'
#' }
#'
#' @note It is highly recommended to set the system environment timezone to the one from the meteorology file. If not, the function try to use the Timezone
#' from the parameter files to set it. When in doubt, set it to UTC (`Sys.setenv(TZ="UTC")`), as for [Aquiares()].
#'
#' @return A daily meteorology data.frame (invisibly).
#'
#' @seealso [DynACof()]
#'
#' @author R. Vezy; O. Roupsard
#'
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom dplyr group_by summarise mutate ungroup transmute
#' @importFrom utils data tail
#' @importFrom bigleaf air.density
#' @importFrom data.table as.data.table ":=" ".N"
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  Sys.setenv(TZ="UTC")
#'  Met_c= Meteorology()
#'
#'  # Get the units of the output variables:
#'  attr(Met_c,"unit")
#'  }
#' }
#'
#' @export
Meteorology= function(file=NULL, Period=NULL,Parameters= Import_Parameters()){
  .= NULL # for R CMD check

  if(is.null(file)){
    Aquiares= NULL
    data("Aquiares", envir = environment())
    MetData= Aquiares
  }else{
    MetData= data.table::fread(file,data.table = F)
  }
  # Missing Date:
  if(is.null(MetData$Date)){
    if(!is.null(Parameters$Start_Date)){
      MetData$Date= seq(from=lubridate::ymd(Parameters$Start_Date),
                        length.out= nrow(MetData), by="day")
      warn.var(Var= "Date","Parameters$Start_Date",type='warn')
    }else{
      MetData$Date= seq(from=lubridate::ymd("2000/01/01"),
                        length.out= nrow(MetData), by="day")
      warn.var(Var= "Date","dummy 2000/01/01",type='warn')
    }
  }

  MetData$Date= lubridate::fast_strptime(MetData$Date, "%Y-%m-%d",lt=F)
  MetData$year= lubridate::year(MetData$Date)
  MetData$DOY= lubridate::yday(MetData$Date)

  if(!is.null(Period)){
    if(Period[1]<min(MetData$Date)|Period[2]>max(MetData$Date)){
      if(Period[2]>max(MetData$Date)){
        warning(paste("Meteo file do not cover the given period", "\n",
                      "Max date in meteo file= ",as.character(format(max(MetData$Date), "%Y-%m-%d")),
                      " ; max given period= ", as.character(Period[2]), "\n",
                      "setting the maximum date of simulation to the one from the meteo file"))
      }
      if(Period[1]<min(MetData$Date)){
        warning(paste("Meteo file do not cover the given period", "\n",
                      "Min date in meteo file= ",as.character(format(min(MetData$Date), "%Y-%m-%d")),
                      " ; min given period= ", as.character(Period[1]), "\n",
                      "setting the minimum date of simulation to the one from the meteo file"))
      }
    }
    MetData= MetData[MetData$Date>=Period[1]&MetData$Date<=(Period[2]),]
  }

  # Missing RAD:
  if(is.null(MetData$RAD)){
    if(!is.null(MetData$PAR)){
      MetData$RAD= MetData$PAR/Parameters$FPAR
      warn.var(Var= "RAD", replacement= "PAR",type='warn')
    }else{
      warn.var(Var= "RAD", replacement= "PAR",type='error')
    }
  }

  # Missing PAR:
  if(is.null(MetData$PAR)){
    MetData$PAR= MetData$RAD*Parameters$FPAR
    warn.var(Var= "PAR",replacement= "RAD",type='warn')
  }
  MetData$PAR[MetData$PAR<0.1]= 0.1

  # Missing Tmax and/or Tmin Temperature:
  if(is.null(MetData$Tmin)|is.null(MetData$Tmax)){
    warn.var(Var= "Tmin and/or Tmax",type='error')
  }

  # Missing air temperature:
  if(is.null(MetData$Tair)){
    MetData$Tair= (MetData$Tmax-MetData$Tmin)/2
    warn.var(Var= "Tair",replacement= "the equation (MetData$Tmax-MetData$Tmin)/2",type='warn')
  }

  # Missing VPD:
  if(is.null(MetData$VPD)){
    if(!is.null(MetData$RH)){
      MetData$VPD= bigleaf::rH.to.VPD(rH = MetData$RH/100, Tair = MetData$Tair)*10 # hPa
      warn.var(Var= "VPD","RH and Tair using bigleaf::rH.to.VPD",type='warn')
    }else{
      warn.var(Var= "VPD", replacement= "RH",type='error')
    }
  }

  # Missing air pressure:
  if(is.null(MetData$Pressure)){
    if(!is.null(Parameters$Elevation)){
      if(!is.null(MetData$VPD)){
        MetData$Pressure= bigleaf::pressure.from.elevation(elev = Parameters$Elevation,
                                                           Tair = MetData$Tair,
                                                           VPD = MetData$VPD)*10
        # Return in kPa
        warn.var(Var= "Pressure",
                 replacement=paste("Elevation, Tair and VPD",
                                   "using bigleaf::pressure.from.elevation"),
                 type='warn')
      }else{
        MetData$Pressure= bigleaf::pressure.from.elevation(elev = Parameters$Elevation,
                                                           Tair = MetData$Tair)*10
        # Return in kPa
        warn.var(Var= "Pressure",
                 replacement=paste("Elevation and Tair",
                                   "using bigleaf::pressure.from.elevation"),
                 type='warn')
      }
    }else{
      warn.var(Var= "Pressure",replacement="Elevation",type='error')
    }
  }

  # Missing rain:
  if(is.null(MetData$Rain)){
    MetData$Rain= 0 # assume no rain
    warn.var(Var= "Rain","constant (= 0, assuming no rain)",type='warn')
  }

  # Missing wind speed:
  if(is.null(MetData$WindSpeed)){
    if(!is.null(Parameters$WindSpeed)){
      MetData$WindSpeed= Parameters$WindSpeed # assume constant windspeed
      warn.var(Var= "WindSpeed","constant (= Parameters$WindSpeed)",type='warn')
    }else{
      warn.var(Var= "WindSpeed", replacement= "Parameters$WindSpeed (constant value)",type='error')
    }
  }
  MetData$WindSpeed[MetData$WindSpeed<0.01]= 0.01
  # Missing atmospheric CO2 concentration:
  if(is.null(MetData$CO2)){
    if(!is.null(Parameters$CO2)){
      MetData$CO2= Parameters$CO2 # assume constant windspeed
      warn.var(Var= "CO2","constant (= Parameters$CO2)",type='warn')
    }else{
      warn.var(Var= "CO2", replacement= "Parameters$CO2 (constant value)",type='error')
    }
  }

  # Missing DegreeDays:
  if(is.null(MetData$DegreeDays)){
    MetData$DegreeDays=
      GDD(Tmax= MetData$Tmax,Tmin= MetData$Tmin, MinTT= Parameters$MinTT,
          MaxTT = Parameters$MaxTT)
    warn.var(Var= "DegreeDays","Tmax, Tmin and MinTT",type='warn')
  }

  # Missing diffuse fraction:
  if(is.null(MetData$FDiff)){
    MetData$FDiff=
      Diffuse_d(DOY = MetData$DOY, RAD = MetData$RAD,
                Latitude = Parameters$Latitude,type = "Spitters")
    warn.var(Var= "FDiff","DOY, RAD and Latitude using Diffuse_d()",type='warn')
  }

  # Correct the noon hour by the Timezone if the user use TZ="UTC":
  if(Sys.timezone()=="UTC"|Sys.timezone()=="GMT"){
    cor_tz= Parameters$TimezoneCF*60*60
  }else{
    # Else R use the user time zone (with warning).
    warning("Meteo file uses this time-zone: ",Sys.timezone(),". Set it to \"UTC\" if you want to use ",
            "the timezone from your parameter file")
    cor_tz= 1
  }

  # Solar zenithal angle at noon (radian):
  MetData$ZEN=
    solartime::computeSunPosition(timestamp = MetData$Date+60*60*12+cor_tz,
                                  latDeg = Parameters$Latitude,
                                  longDeg = Parameters$Longitude)%>%
    as.data.frame()%>%{sin(.$elevation)}%>%acos(.)

  # Compute net radiation using the Allen et al. (1998) equation :

  if(!is.null(MetData$RH)){
    MetData$Rn= Rad_net(DOY = MetData$DOY,RAD = MetData$RAD,Tmax = MetData$Tmax,
                        Tmin = MetData$Tmin, Rh =  MetData$RH,
                        Elevation = Parameters$Elevation,Latitude = Parameters$Latitude,
                        albedo = Parameters$albedo)
  }else if(!is.null(MetData$VPD)){
    MetData$Rn= Rad_net(DOY = MetData$DOY,RAD = MetData$RAD,Tmax = MetData$Tmax,
                        Tmin = MetData$Tmin, VPD =  MetData$VPD,
                        Elevation = Parameters$Elevation,Latitude = Parameters$Latitude,
                        albedo = Parameters$albedo)
  }

  DaysWithoutRain= Rain= NULL # To avoid notes by check
  MetData= data.table::as.data.table(MetData)
  MetData[, DaysWithoutRain := 0]; MetData[Rain > 0, DaysWithoutRain := 1]
  MetData$DaysWithoutRain= sequence(MetData[,.N,cumsum(DaysWithoutRain)]$N)-1
  MetData= as.data.frame(MetData)

  MetData$Air_Density= bigleaf::air.density(MetData$Tair,MetData$Pressure/10)

  # Force to keep only the input variable the model need to avoid any issues:
  Varnames= c('year','DOY','Date','Rain','Tair','RH','RAD','Pressure',
              'WindSpeed','CO2','DegreeDays','PAR','FDiff',
              'VPD','Rn','Tmax','Tmin','DaysWithoutRain','Air_Density','ZEN')
  MetData= MetData[colnames(MetData)%in%Varnames]
  MetData[,-c(1:3)]= round(MetData[,-c(1:3)],4)

  attr(MetData,"unit")=
    data.frame(Var= Varnames,
               unit=c("year","day","POSIXct date","mm","Celsius","%","MJ m-2 d-1","hPa",
                      "m s-1","ppm","Celsius","MJ m-2 d-1","Fraction","hPa","MJ m-2 d-1",
                      "Celsius","Celsius","day","kg m-3","rad"))

  message("Meteo computation done\n")
  message(paste("\n", crayon::green$bold$underline("Meteo computation done\n")))
  invisible(MetData)
}
