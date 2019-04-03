#' Default (example) parameter functions
#'
#' @description These functions return the default parameter values for DynACof.
#' They are mainly used invisibly as defaults from [Import_Parameters()], but can still be
#' called by the user for conveniance (but not needed for a model run). The parameters are divided into
#' four functions: `site()`, `soil()`, `coffee()`, and `Tree()`.
#'
#' @section site:
#' The default site is a stand from the Aquiares farm, located in Costa Rica. It is a *Coffea arabica*
#' plantation in agroforestry management under *Erythrina poeppigiana* shade trees. The plot is visible
#' at this \href{https://goo.gl/7FRNXg}{address}, and a full desciption is available
#' \href{https://www.researchgate.net/publication/323398728_Measuring_and_modelling_energy_partitioning_in_canopies_of_varying_complexity_using_MAESPA_model}{here}
#' and \href{https://www.researchgate.net/publication/323469257_Simulation_de_pratiques_de_gestion_alternatives_pour_l'adaptation_des_plantations_perennes_aux_changements_globaux}{here}.
#'
#' @section soil:
#' The default soil parameters.
#'
#' @section coffee:
#' The default coffee parameters. It is essentially a high density plantation (6666 coffee plants
#'  per hectares) of *Coffea arabica var. Caturra* that are pruned every year to sustain
#'  three resprouts per stump in average.
#' @section Tree:
#' The default shade tree parameters. The shade trees in Aquiares were  *Erythrina poeppigiana*
#' planted at high density (250 trees ha-1) and pruned to optimize light transmitted to the *Coffea*,
#' and were depressed in 2000 to a low density of ~7.4 trees ha-1.
#' From 2000, these trees made a relatively large crown with an average height of 26 m in 2018
#' on this site. NB: the Tree parameter file is optional, and not needed
#' for monospecific coffee plantations.
#'
#' @return A list of parameters needed for a DynACof simulation.
#'
#' @note The Tree parameter file is optional. All other parameter files should have the same structure as
#' the one from the default parameter files. Other parameters can be added to the files though.
#'
#' @details These input functions are stored into R-script that can be customized at will by the user to fit
#' new conditions. In that case, the user have to write the resulting functions to separate files
#' corresponding to the site, the soil, the coffee layer and the tree layer.
#' Then [DynACof()] read them using the `Inpath` and `FileName` parameters.
#' File templates with default parameter values and an example use are available from this separate
#' \href{https://github.com/VEZY/DynACof_inputs}{github repository}
#'
#' @aliases soil coffee Tree
#'
#' @seealso [DynACof()]
#'
#' @export
site= function(){
  list(
    Location      = "Aquiares",     # Site location name (optional)
    Start_Date    = "1979/01/01",   # Start date of the meteo data, only used if "Date" is missing from input Meteorology (optionnal)
    Latitude      = 9.93833,        # Latitude (degree)
    Longitude     = -83.72861,      # Longitude (degredd)
    TimezoneCF    = 6,              # Time Zone
    Elevation     = 1040,           # Elevation (m)
    ZHT           = 25,             # Measurment height (m)
    extwind       = 0.58,           # Wind extinction coefficient (-), used to compute the wind speed in the considered layer. Measured on site.
    albedo        = 0.144           # Site albedo, computed using MAESPA.
  )
}
