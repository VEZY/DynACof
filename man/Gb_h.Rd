% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0-Functions.R
\name{Gb_h}
\alias{Gb_h}
\title{Leaf boundary layer conductance for heat}
\usage{
Gb_h(
  Wind,
  wleaf = 0.068,
  LAI_lay,
  LAI_abv,
  extwind = 0,
  Z_top,
  ZHT,
  Z0 = Z_top * 0.1,
  ZPD = Z_top * 0.75,
  alpha = 1.5,
  ZW = ZPD + alpha * (Z_top - ZPD),
  Tleaf = NULL,
  Tair = NULL,
  Dheat = Constants()$Dheat,
  formulation = c("Jones_1992", "Leuning_1995")
)
}
\arguments{
\item{Wind}{Average daily wind speed above canopy (m s-1)}

\item{wleaf}{Average leaf width (m)}

\item{LAI_lay}{Leaf area index of the layer (m2 leaves m-2 soil)}

\item{LAI_abv}{Cumulated leaf area index above the layer (m2 leaves m-2 soil)}

\item{extwind}{Extinction coefficient. Default: \code{0}, no extinction.}

\item{Z_top}{Average canopy height of the taller crop (m)}

\item{ZHT}{Wind measurement height (m)}

\item{Z0}{Roughness length (m). Default: \code{0.1*Z_top}}

\item{ZPD}{Zero-plane displacement (m), Default: \code{0.75*Z_top}}

\item{alpha}{Constant for diffusivity at top canopy. Default: \code{1.5} following
Van de Griend et al (1989).}

\item{ZW}{Top height of the roughness sublayer (m). Default: \code{ZPD+alpha*(Z_top-ZPD)}}

\item{Tleaf}{Leaf temperature (deg C). Only needed if \code{formulation="Leuning_1995"}}

\item{Tair}{Canopy air temperature (deg C). Only needed if \code{formulation="Leuning_1995"}}

\item{Dheat}{Molecular diffusivity for heat (m2 s-1). Default to \code{Constants()$Dheat}.
Only needed if \code{formulation="Leuning_1995"}}

\item{formulation}{The formulation used to compute \eqn{Gb_h}}
}
\value{
\item{Gb}{The leaf boundary layer conductance for heat (m s-1)}
}
\description{
Compute the bulk leaf boundary layer conductance for heat using
the wind speed, the leaf dimension, and leaf area distribution
following Jones (1992) or Leuning et al. (1995).
}
\details{
The leaf boundary layer conductance for heat can be transformed into leaf boundary
layer conductance for water vapour as follow:
\deqn{Gb_w= 1.075*gb_h}
Note that \eqn{Gb_w} should be doubled for amphistomatous plants (stomata on
both sides of the leaves).
}
\examples{
# Gb for a coffee plantation managed in agroforestry system:
Gb_h(Wind=3,wleaf=0.068,LAI_lay=4,LAI_abv=0.5,ZHT=25,Z_top=24,extwind=0.58)

}
\references{
\itemize{
\item Leuning, R., et al., Leaf nitrogen, photosynthesis, conductance and transpiration:
scaling from leaves to canopies. Plant, Cell & Environment, 1995. 18(10): p. 1183-1200.
\item Mahat, V., D.G. Tarboton, and N.P. Molotch, Testing above‐ and below‐canopy represetations
of turbulent fluxes in an energy balance snowmelt model. Water Resources Research, 2013.
49(2): p. 1107-1122.
}
}
\seealso{
\code{\link[=G_bulk]{G_bulk()}}, \code{\link[=G_soilcan]{G_soilcan()}}, \code{\link[=G_interlay]{G_interlay()}} and
\code{\link[=GetWind]{GetWind()}}, which is used internaly.
}
