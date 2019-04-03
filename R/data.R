#' Aquiares meteorology.
#'
#' Aquiares meteorology data at daily time-step
#'
#' @format A data frame with sixteen variables: `year`, `DOY`,
#'   `Date`, `Rain`, `Tair`, `RH`, `RAD`, `Pressure`,
#'   `WindSpeed`, `CO2`, `DegreeDays`, `Rain`, `PAR`,
#'   `FDiff`, `VPD`, `Tmax` and `Tmin`. All units corresponds to
#'   the [Meteorology()] function recommendantions.
#'
"Aquiares"

#' Output variables
#'
#' Generic output variables from DynACof, with their units and brief description
#'
#' @format A data frame with three columns:
#' \describe{
#'   \item{Type}{Variable name}
#'   \item{Unit}{unit of the variable}
#'   \item{Definition}{a brief definition of the variable}
#' }
#' For the moment the model has 246 output variables (without meteorological variables).
#'
"varnames"
