#' Default (example) parameter functions
#'
#' @description These input parameters for the model are imported as functions that are executed and return
#' the list of parameters. It is strongly recommended for clarity to write the parameters into separate
#' files corresponding to the site, the soil, the coffee layer and the tree layer.
#' These functions are provided to define the default parameters for DynACof, and are used as example.
#' They are mainly used invisibly as defaults from \code{\link{Import_Parameters}}, but can still be
#' called by the user for conveniance (but not needed for a model run). The paraeters are divided into
#' four functions:
#'
#' @section site:
#' The default site is a stand from the Aquiares farm, located in Costa Rica. It is a \emph{Coffea arabica}
#' plantation in agroforestry management under \emph{Erythrina poeppigiana} shade trees. The plot is visible
#' at this \href{https://goo.gl/7FRNXg}{adress}, and a full desciption is available
#' \href{https://www.researchgate.net/publication/323398728_Measuring_and_modelling_energy_partitioning_in_canopies_of_varying_complexity_using_MAESPA_model}{here}
#' and \href{https://www.researchgate.net/publication/323469257_Simulation_de_pratiques_de_gestion_alternatives_pour_l'adaptation_des_plantations_perennes_aux_changements_globaux}{here}.
#'
#' @section soil:
#' The default soil parameters.
#'
#' @section coffee:
#' The default coffee parameters. It is essentially a high density plantation (6666 coffee plants
#'  per hectares) of \emph{Coffea arabica var. Caturra} that are pruned every year to sustain
#'  three resprouts per cep in average.
#' @section Tree:
#' The default shade tree parameters. The shade trees in Aquiares are  \emph{Erythrina poeppigiana}
#' planted at a low density of ~7.4 trees ha-1. These tree make a relatively large crown and have an
#' average height of 26 m in 2018 on this site. NB: the Tree parameter file is optional, and not needed
#' for monospecific coffee plantations.
#'
#' @return A list of parameters needed for a DynACof simulation. The full list (and example files)
#'  is available on the \href{https://github.com/VEZY/DynACof/tree/master/data}{github repository } in the
#'   \code{data/} folder.
#'
#'
#' @note The Tree parameter file is optional. All other parameter files should have the same structure as
#' the one from the default parameter files. Other parameters can be added to the files though.
#'
#' @aliases soil coffee Tree
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
    Height_Coffee = 2,              # Coffee canopy height (m), used for GBCANMS, if shade tree is less than this.
    ZHT           = 25,             # Measurment height (m)
    albedo= 0.144                   # Site albedo, computed using MAESPA.
  )
}
