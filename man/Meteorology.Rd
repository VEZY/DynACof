% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1-Meteo.R
\name{Meteorology}
\alias{Meteorology}
\title{Import Meteorology for model input}
\usage{
Meteorology(file = "1-Input/Default/2-Meteorology.txt", Period = NULL,
  Parameters = Import_Parameters())
}
\arguments{
\item{file}{Either the file name to read, a shell command that preprocesses the file (e.g. fread("grep blah filename"))
or the input itself as a string, see \code{\link[data.table]{fread}}. In both cases, a length 1 character string.
A filename input is passed through path.expand for convenience and may be a URL starting http:// or file://}

\item{Period}{The desired time period to be returned inthe form of a vector of two POSIX dates (min and max).}

\item{Parameters}{A list of parameters:
\itemize{
  \item Start_Date: optional, the date of the first meteo file record. Only needed if the Date column is missing (Posixct).
  \item FPAR      : Fraction of global radiation that is PAR (source: MAESPA model), only needed if either RAD or PAR is missing (fraction).
  \item Elevation : elevation of the site, only needed if atmospheric pressure is missing (m)
  \item Latitude  : latitude of the site, only needed if the diffuse fraction of light is missing (degree)
  \item WindSpeed : constant wind speed, only needed if windspeed is missing (m s-1)
  \item CO2       : constant atmospheric \eqn{CO_2} concentration, only needed if \eqn{CO_2} is missing (ppm)
  \item MinTT     : minimum temperature threshold for degree days computing, see \code{\link{GDD}}
  \item MaxTT     : maximum temperature threshold for degree days computing, see \code{\link{GDD}}
  \item albedo    : site shortwave surface albedo, only needed if net radiation is missing, see \code{\link{Rad_net}}
}}

\item{WriteIt}{If TRUE, will write the output of the function in the same path.}

\item{...}{Additional parameters to pass to the \code{\link[data.table]{fwrite}} function (sep is fixed to \code{;}
and colnames to \code{T}).}
}
\value{
A daily timestep meteorology data.frame with different columns
}
\description{
This function aims to import the meteorology data, check it, and compute missing variables.
}
\details{
The imported file is expected to be at daily time-step.
         The albedo is used to compute the system net radiation that is then used to compute the soil net radiation using an
         extinction coefficient with the plot LAI following the Shuttleworth & Wallace (1985) formulation. This computation is
         likely to change in the near future to add a more uniform process-based formulation (such as Choudhury & Monteith 1988).
        \tabular{ll}{\strong{Var} \tab \strong{unit}\cr
                   year            \tab year        \cr
                    DOY             \tab day         \cr
                    Date            \tab POSIXct date\cr
                    Rain            \tab mm          \cr
                    Tair            \tab deg C       \cr
                    RH              \tab \%           \cr
                    RAD             \tab MJ m-2 d-1  \cr
                    Pressure        \tab hPa         \cr
                    WindSpeed       \tab m s-1       \cr
                    CO2             \tab ppm         \cr
                    DegreeDays      \tab deg C       \cr
                    PAR             \tab MJ m-2 d-1  \cr
                    FDiff           \tab Fraction    \cr
                    VPD             \tab hPa         \cr
                    Rn              \tab MJ m-2 d-1  \cr
                    Tmax            \tab deg C       \cr
                    Tmin            \tab deg C       \cr
                    rho             \tab kg m-3      \cr
                    DaysWithoutRain \tab day}
}
\author{
R. Vezy; O. Roupsard
}