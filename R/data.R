#' Aquiares meteorology.
#'
#' Aquiares meteorology data at daily time-step
#'
#' @format A data frame with sixteen variables: \code{year}, \code{DOY},
#'   \code{Date}, \code{Rain}, \code{Tair}, \code{RH}, \code{RAD}, \code{Pressure},
#'   \code{WindSpeed}, \code{CO2}, \code{DegreeDays}, \code{Rain}, \code{PAR},
#'   \code{FDiff}, \code{VPD}, \code{Tmax} and \code{Tmin}. All units corresponds to
#'   the \code{\link{Meteorology}} function recommendantions.
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
#' For the moment the model has 106 output variables.
#'
"varnames"
