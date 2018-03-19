#' Import Meteorology for model input
#'
#' @description This function aims to import the meteorology data, check it, and compute missing variables.
#'
#' @param file        Either the file name to read, a shell command that preprocesses the file (e.g. fread("grep blah filename"))
#'                    or the input itself as a string, see \code{\link[data.table]{fread}}. In both cases, a length 1 character string.
#'                    A filename input is passed through path.expand for convenience and may be a URL starting http:// or file://.
#'                    Default to \code{data/Meteorology.RData}, which is the package example data.
#' @param Period      The desired time period to be returned inthe form of a vector of two POSIX dates (min and max).
#' @param WriteIt     If TRUE, will write the output of the function in the same path.
#' @param ...         Additional parameters to pass to the \code{\link[data.table]{fwrite}} function (sep is fixed to \code{;}
#'                    and colnames to \code{T}).
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
#'         \tabular{ll}{\strong{Var} \tab \strong{unit} \tab \strong{Definition}\cr
#'                     year            \tab year        \tab Year of the simulation                       \cr
#'                     DOY             \tab day         \tab day of the year                              \cr
#'                     Date            \tab POSIXct date\tab Date in POSICct format                       \cr
#'                     Rain            \tab mm          \tab Rainfall                                     \cr
#'                     Tair            \tab deg C       \tab Air temperature (above canopy)               \cr
#'                     RH              \tab \%          \tab Relative humidity                            \cr
#'                     RAD             \tab MJ m-2 d-1  \tab Incident shortwave radiation                 \cr
#'                     Pressure        \tab hPa         \tab Atmospheic pressure                          \cr
#'                     WindSpeed       \tab m s-1       \tab Wind speed                                   \cr
#'                     CO2             \tab ppm         \tab Atmospheric CO2 concentration                \cr
#'                     DegreeDays      \tab deg C       \tab Growing degrre days                          \cr
#'                     PAR             \tab MJ m-2 d-1  \tab Incident photosynthetically active radiation \cr
#'                     FDiff           \tab Fraction    \tab Diffuse light fraction                       \cr
#'                     VPD             \tab hPa         \tab Vapor pressure deficit                       \cr
#'                     Rn              \tab MJ m-2 d-1  \tab Net radiation (will be removed further)       \cr
#'                     Tmax            \tab deg C       \tab Maximum air temperature durnig the day       \cr
#'                     Tmin            \tab deg C       \tab Minimum air temperature durnig the day       \cr
#'                     rho             \tab kg m-3      \tab Air density of moist air                     \cr
#'                     DaysWithoutRain \tab day         \tab Number of consecutive days with no rainfall}
#'
#' @return A daily timestep meteorology data.frame with different columns
#'
#' @seealso \code{\link{DynACof}}
#'
#' @author R. Vezy; O. Roupsard
#'
#' @export
Meteorology= function(file=NULL, Period=NULL,
                      Parameters= Import_Parameters()){
  if(is.null(file)){MetData= Aquiares}else{
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
                      "Min date in meteo file= ",as.character(format(max(MetData$Date), "%Y-%m-%d")),
                      " ; min given period= ", as.character(Period[2]), "\n",
                      "setting the minimum date of simulation to the one from the meteo file"))
      }
    }
  }

  # Missing RAD:
  if(is.null(MetData$RAD)){
    if(!is.null(MetData$PAR)){
      MetData$RAD= MetData$PAR/Parameters$FPAR
      warn.var(Var= "RAD", replace= "PAR",type='warn')
    }else{
      warn.var(Var= "RAD", replace= "PAR",type='error')
    }
  }

  # Missing PAR:
  if(is.null(MetData$PAR)){
    MetData$PAR= MetData$RAD*Parameters$FPAR
    warn.var(Var= "PAR",replace= "RAD",type='warn')
  }


  # Missing Tmax and/or Tmin Temperature:
  if(is.null(MetData$Tmin)|is.null(MetData$Tmax)){
    warn.var(Var= "Tmin and/or Tmax",type='error')
  }

  # Missing air temperature:
  if(is.null(MetData$Tair)){
    MetData$Tair= (MetData$Tmax-MetData$Tmin)/2
    warn.var(Var= "Tair",replace= "the equation (MetData$Tmax-MetData$Tmin)/2",type='warn')
  }

  # Missing VPD:
  if(is.null(MetData$VPD)){
    if(!is.null(MetData$RH)){
      MetData$VPD= rH.to.VPD(rH = MetData$RH/100, Tair = MetData$Tair)*10 # hPa
      # NB : add "bigleaf::" as soon as the issue is
      warn.var(Var= "VPD","RH and Tair using bigleaf::rH.to.VPD",type='warn')
    }else{
      warn.var(Var= "VPD", replace= "RH",type='error')
    }
  }

  # Missing air pressure:
  if(is.null(MetData$Pressure)){
    if(!is.null(MetData$VPD)){
      bigleaf::pressure.from.elevation(elev = Parameters$Elevation,
                                       Tair = MetData$Tair,
                                       VPD = MetData$VPD)
      warn.var(Var= "Pressure",
               replace=paste("Elevation, Tair and VPD",
                             "using bigleaf::pressure.from.elevation"),
               type='warn')
    }else{
      bigleaf::pressure.from.elevation(elev = Parameters$Elevation,
                                       Tair = MetData$Tair)
      warn.var(Var= "Pressure",
               replace=paste("Elevation and Tair",
                             "using bigleaf::pressure.from.elevation"),
               type='warn')
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
      warn.var(Var= "WindSpeed", replace= "Parameters$WindSpeed (constant value)",type='error')
    }
  }

  # Missing atmospheric CO2 concentration:
  if(is.null(MetData$CO2)){
    if(!is.null(Parameters$CO2)){
      MetData$CO2= Parameters$CO2 # assume constant windspeed
      warn.var(Var= "CO2","constant (= Parameters$CO2)",type='warn')
    }else{
      warn.var(Var= "CO2", replace= "Parameters$CO2 (constant value)",type='error')
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

  MetData$rho= bigleaf::air.density(MetData$Tair,MetData$Pressure/10) # (kg m-3)

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

  # Force to keep only the input variable the model need to avoid any issues:
  Varnames= c('year','DOY','Date','Rain','Tair','RH','RAD','Pressure',
              'WindSpeed','CO2','DegreeDays','PAR','FDiff',
              'VPD','Rn','Tmax','Tmin','rho','DaysWithoutRain')
  MetData= MetData[colnames(MetData)%in%Varnames]

  attr(MetData,"unit")=
    data.frame(Var= Varnames,
               unit=c("year","day","POSIXct date","mm","deg C","%","MJ m-2 d-1","hPa",
                      "m s-1","ppm","deg C","MJ m-2 d-1","Fraction","hPa","MJ m-2 d-1",
                      "deg C","deg C","kg m-3","day"))

  cat("Meteo computation done")
  return(MetData)
}
