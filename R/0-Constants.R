#' Constants used in the DynACof package
#'
#' @return This function defines the following constants:
#' \item{Cp}{specific heat of air for constant pressure (\eqn{MJ\ K^{-1}\ kg^{-1}}{MJ K-1 kg-1}), Source: Allen 1998 FAO Eq. 8 p. 32}
#' \item{pressure0}{reference atmospheric pressure at sea level (\eqn{Pa}{Pa})}
#' \item{FPAR}{Fraction of global radiation that is PAR (source: MAESPA model)}
#' \item{g}{gravitational acceleration (\eqn{m\ s^{-2}}{m s-2})}
#' \item{Rd}{gas constant of dry air (\eqn{J\ kg^{-1}\ K^{-1}}{J kg-1 K-1}), source : Foken p. 245}
#' \item{Rgas}{universal gas constant (\eqn{J\ mol^{-1}\ K^{-1}}{J mol-1 K-1})}
#' \item{Kelvin}{conversion degree Celsius to Kelvin}
#' \item{vonkarman}{von Karman constant (-)}
#' \item{MJ_to_W}{coefficient to convert MJ into W (\eqn{W\ MJ^{-1}}{W MJ-1})}
#' \item{Gsc}{solar constant (\eqn{W\ m^{-2}=J\ m^{-2}\ s^{-1}}{W m-2 = J m-2 s-1}, ), source : Khorasanizadeh and Mohammadi (2016)}
#' \item{sigma}{Stefan-Boltzmann constant (\eqn{W\ m^{-2}\ K^{-4}}{W m-2 K-4})}
#' \item{H2OMW}{Conversion factor from kg to mol for H2O (\eqn{kg\ mol^{-1}}{kg mol-1})}
#' \item{W_umol}{Conversion factor from watt to micromole for H2O (\eqn{W\ \mu mol^{-1}}{W µmol-1})}
#' \item{\eqn{\lambda} (lambda)}{Latent heat of vaporization (\eqn{MJ\ kg_{H2O}^{-1}}{MJ kgH2O-1})}
#' \item{cl}{Drag coefficient per unit leaf area (\eqn{m\ s^{-1}}{m s-1})}
#' \item{Dheat}{Molecular diffusivity for heat (\eqn{m\ s^{-1}}{m s-1})}
#'
#' @note Values are partly burrowed from \code{\link[bigleaf]{bigleaf.constants}}
#'
#' @seealso \code{\link{DynACof}}
#'
#' @references \itemize{
#'   \item Allen, R. G., et al. (1998). "Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56."  300(9): D05109.
#'   \item Foken, T, 2008: Micrometeorology. Springer, Berlin, Germany.
#'   \item Khorasanizadeh, H. and K. Mohammadi (2016). "Diffuse solar radiation on a horizontal surface: Reviewing and categorizing the empirical models." Renewable and Sustainable Energy Reviews 53: 338-362.
#' }
#'
#' @export
Constants= function(){
    list(
    Cp        = 1013*10^-6,
    pressure0 = 101325,
    FPAR      = 0.5,
    g         = 9.81,
    Rd        = 287.0586,
    Rgas      = 8.314,
    Kelvin    = 273.15,
    vonkarman = 0.41,
    MJ_to_W   = 10^-6,
    Gsc       = 1367,            # also found 1366 in Kalogirou (2013)
    sigma     = 5.670367e-08,
    H2OMW     = 18.e-3,
    W_umol    = 4.57,
    lambda    = 2.45,
    cl        = 0.4,
    Dheat     = 21.5e-6
    )
}
