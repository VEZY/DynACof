#' Import Meteorology for model input
#'
#' @description This function aims to import the meteorology data, check it, and compute missing variables.
#'
#' @param file        Either the file name to read, a shell command that preprocesses the file (e.g. fread("grep blah filename"))
#'                    or the input itself as a string, see \code{\link[data.table]{fread}}. In both cases, a length 1 character string.
#'                    A filename input is passed through path.expand for convenience and may be a URL starting http:// or file://.
#'                    Default to \code{NULL}, which make the function return the \code{Aquiares} package example data.
#' @param Period      The desired time period to be returned in the form of a vector of two POSIX dates (min and max).
#'
#' @param Parameters  A list of parameters:
#' \itemize{
#'   \item Start_Date: optional, the date of the first meteo file record. Only needed if the Date column is missing (Posixct).
#'   \item FPAR      : Fraction of global radiation that is PAR (source: MAESPA model), only needed if either RAD or PAR is missing (fraction).
#'   \item Elevation : elevation of the site, only needed if atmospheric pressure is missing (m)
#'   \item Latitude  : latitude of the site, only needed if the diffuse fraction of light is missing (degree)
#'   \item WindSpeed : constant wind speed, only needed if windspeed is missing (m s-1)
#'   \item CO2       : constant atmospheric \eqn{CO_2} concentration, only needed if \eqn{CO_2} is missing (ppm)
#'   \item MinTT     : minimum temperature threshold for degree days computing, see \code{\link{GDD}}
#'   \item MaxTT     : maximum temperature threshold for degree days computing, see \code{\link{GDD}}
#'   \item albedo    : site shortwave surface albedo, only needed if net radiation is missing, see \code{\link{Rad_net}}
#' }
#'
#'
#' @details The imported file is expected to be at daily time-step.
#'          The albedo is used to compute the system net radiation that is then used to compute the soil net radiation using an
#'          extinction coefficient with the plot LAI following the Shuttleworth & Wallace (1985) formulation. This computation is
#'          likely to change in the near future to add a more uniform process-based formulation (such as Choudhury & Monteith 1988).
#' \tabular{llll}{\strong{Var} \tab \strong{unit} \tab \strong{Definition} \tab \strong{If missing} \cr
#' Date            \tab POSIXct date\tab Date in POSICct format                       \tab Computed from start date parameter, or set a dummy date if missing\cr
#' year            \tab year        \tab Year of the simulation                       \tab Computed from Date \cr
#' DOY             \tab day         \tab day of the year                              \tab Computed from Date \cr
#' Rain            \tab mm          \tab Rainfall                                     \tab Assume no rain \cr
#' Tair            \tab deg C       \tab Air temperature (above canopy)               \tab Computed from Tmax and Tmin \cr
#' Tmax            \tab deg C       \tab Maximum air temperature durnig the day       \tab Required (error) \cr
#' Tmin            \tab deg C       \tab Minimum air temperature durnig the day       \tab Required (error) \cr
#' RH              \tab \%          \tab Relative humidity                            \tab Not used, but prefered over VPD for Rn computation \cr
#' RAD             \tab MJ m-2 d-1  \tab Incident shortwave radiation                 \tab Computed from PAR \cr
#' Pressure        \tab hPa         \tab Atmospheric pressure                         \tab Try to compute from VPD, Tair and Elevation, or Tair and Elevation. \cr
#' WindSpeed       \tab m s-1       \tab Wind speed                                   \tab Try to set it to constant: \code{Parameters$WindSpeed} \cr
#' CO2             \tab ppm         \tab Atmospheric CO2 concentration                \tab Try to set it to constant: \code{Parameters$CO2}\cr
#' DegreeDays      \tab deg C       \tab Growing degrre days                          \tab Computed using \code{\link{GDD}} \cr
#' PAR             \tab MJ m-2 d-1  \tab Incident photosynthetically active radiation \tab Computed from RAD \cr
#' FDiff           \tab Fraction    \tab Diffuse light fraction                       \tab Computed using \code{\link{Diffuse_d}} usinf Spitters formula \cr
#' VPD             \tab hPa         \tab Vapor pressure deficit                       \tab Computed from RH \cr
#' Rn              \tab MJ m-2 d-1  \tab Net radiation (will soon be depreciated)     \tab Computed using \code{\link{Rad_net}} with RH, or VPD \cr
#' DaysWithoutRain \tab day         \tab Number of consecutive days with no rainfall  \tab Computed from Rain \cr
#' Air_Density     \tab kg m-3      \tab Air density of moist air (\eqn{\rho}) above canopy \tab Computed using \code{\link[bigleaf]{air.density}}}
#'          It is highly recommended to set the system environment timezone to the one from the meteorology file.
#'          For example the default meteorology file (\code{\link{Aquiares}}) has to be set to \code{Sys.setenv(TZ="UTC")}.
#'
#' @return A daily timestep meteorology data.frame (invisibly) with either the data read above or computed.
#'
#' @seealso \code{\link{DynACof}}
#'
#' @author R. Vezy; O. Roupsard
#'
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom dplyr group_by summarise mutate ungroup transmute
#' @importFrom utils data tail
#' @importFrom bigleaf air.density
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
  if(is.null(file)){
    data("Aquiares", envir = environment())
    MetData= Aquiares
  }else{
    MetData= data.table::fread(file,data.table = F)
  }

  MetData$Date= lubridate::fast_strptime(MetData$Date, "%Y-%m-%d",lt=F)


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
      MetData$VPD= rH.to.VPD(rH = MetData$RH/100, Tair = MetData$Tair)*10 # hPa
      # NB : add "bigleaf::" as soon as the issue is
      warn.var(Var= "VPD","RH and Tair using bigleaf::rH.to.VPD",type='warn')
    }else{
      warn.var(Var= "VPD", replacement= "RH",type='error')
    }
  }

  # Missing air pressure:
  if(is.null(MetData$Pressure)){
    if(!is.null(Parameters$Elevation)){
      if(!is.null(MetData$VPD)){
        bigleaf::pressure.from.elevation(elev = Parameters$Elevation,
                                         Tair = MetData$Tair,
                                         VPD = MetData$VPD)
        warn.var(Var= "Pressure",
                 replacement=paste("Elevation, Tair and VPD",
                                   "using bigleaf::pressure.from.elevation"),
                 type='warn')
      }else{
        bigleaf::pressure.from.elevation(elev = Parameters$Elevation,
                                         Tair = MetData$Tair)
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


  MetData$year= lubridate::year(MetData$Date)
  MetData$DOY= lubridate::yday(MetData$Date)

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

  MetData$DaysWithoutRain= MetData$Rain
  MetData$DaysWithoutRain[MetData$Rain>0]= 1
  MetData$DaysWithoutRain[MetData$Rain==0]= 0

  daysnorain=
    MetData%>%
    group_by(cumsum(MetData$DaysWithoutRain))%>%
    summarise(DaysWithoutRain= n())
  MetData$DaysWithoutRain= sequence(daysnorain$DaysWithoutRain)-1

  MetData$Air_Density= bigleaf::air.density(MetData$Tair,MetData$Pressure/10)

  # Force to keep only the input variable the model need to avoid any issues:
  Varnames= c('year','DOY','Date','Rain','Tair','RH','RAD','Pressure',
              'WindSpeed','CO2','DegreeDays','PAR','FDiff',
              'VPD','Rn','Tmax','Tmin','DaysWithoutRain','Air_Density')
  MetData= MetData[colnames(MetData)%in%Varnames]
  MetData[,-c(1:3)]= round(MetData[,-c(1:3)],3)

  attr(MetData,"unit")=
    data.frame(Var= Varnames,
               unit=c("year","day","POSIXct date","mm","deg C","%","MJ m-2 d-1","hPa",
                      "m s-1","ppm","deg C","MJ m-2 d-1","Fraction","hPa","MJ m-2 d-1",
                      "deg C","deg C","day","kg m-3"))

  message("Meteo computation done\n")
  invisible(MetData)
}
