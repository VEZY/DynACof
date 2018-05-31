#' MAESPA metamodels
#'
#' @description Several variables can be highly impacted by the canopy structure. Hence, some variables are
#'              computed using metamodels from the MAESPA model (see details). These functions are helper
#'              functions that are made available to the user for the possibility to change the equations easily,
#'              but not to call them directly.
#'
#' @param S  The global list used within the DynACof model.
#' @param i  The day of interest.
#' @details  The purpose of using metamodels in DynACof is to make this two-layer dynamic crop model able to consider
#'           the 3D canopy heterogeneity effect on fundamental processes, as if it was with a MAESPA-growth model
#'           coupling. Firstly, the main process impacted by canopy complexity is the light absorbed by the plants
#'           (Charbonnier et al., 2013). Indeed, in dynamic crop models, the absorbed photosynthetically active
#'           radiation (APAR) by the canopy is often computed using the simple Beer-Lambert’s law or a derivative,
#'           with a variable leaf area index in time, but a constant extinction coefficient (Van Oijen et al., 2010b).
#'           However, heterogeneous canopies such as the shade trees in AFS coffee plantations tends to violate the
#'           assumption of a constant value for the diffuse (\eqn{K_dif_Tree} the direct (\eqn{K_dir_Tree} light
#'           extinction coefficient because the spatial distribution of the leaf area is not uniform (high gap fraction)
#'           and because the leaf area density can change with time through foliage aggregation (Sampson and Smith,
#'            1993;Sinoquet et al., 2007). Secondly, a comparison between coffee planted in monoculture and under
#'            agroforestry system showed that canopy complexity affected canopy temperature, water, and energy
#'            partitioning (Vezy et al., 2018), and probably photosynthesis because it is related to light interception
#'            and transpiration through stomatal conductance. Therefore, we derived metamodels from MAESPA for the
#'            diffuse (\eqn{K_dif_Tree} the direct (\eqn{K_dir_Tree} shade tree light extinction coefficients, the light
#'            use efficiency (LUE, gC MJ-1), the coffee canopy temperature (Tcan, deg Celsius) and leaf water
#'            potential (MPa), the transpiration (T_\*, mm) and plant sensible heat flux (H_\*). The coffee layer was
#'            considered homogeneous enough to compute constant extinction coefficients derived from the MAESPA simulation,
#'            and the partitioning parameter between soil sensible and latent flux was also adjusted using MAESPA outputs.
#'            MAESPA is a 3D explicit model for energy, carbon and water fluxes simulation, for further details, see:
#'            \href{https://goo.gl/Z4ehi8}{Vezy et al. (2018)}, or the \href{https://maespa.github.io/}{MAESPA website}
#' @return \item{\eqn{K_dif_Tree}}{Shade tree diffuse light coefficient}
#'         \item{\eqn{K_dir_Tree}}{Shade tree direct light coefficient}
#'         \item{\eqn{lue_Tree}}{Light use efficiency (gC MJ-1)}
#'         \item{\eqn{T_Tree}}{Transpiration (mm d-1)}
#'         \item{\eqn{H_Tree}}{Sensible heat (MJ m-2 d-1)}
#'
#' @references See \href{https://goo.gl/NVxcVp}{Vezy (2017)}
#'
#' @examples
#' # Creating a dummy list for use:
#' S= list(Sim= data.frame(K_Dif_Tree= rep(NA_real_,10),
#'         K_Dir_Tree= rep(NA_real_,10),LAD_Tree= rnorm(10,3,0.5)))
#' # Calling the function:
#' Light_extinction_K(S,1:10)
#'
#' @aliases Metamodels
#'
#' @seealso \code{\link{DynACof}}
#'
#' @export
Light_extinction_K= function(S,i){
  # See MAESPA_Validation project, script 4-Aquiares_Metamodels.R
  # Source for non-constant k: Sinoquet et al. 2007
  # DOI: 10.1111/j.1469-8137.2007.02088.x
  S$Sim$K_Dif_Tree[i]= 0.6161 - 0.5354*S$Sim$LAD_Tree[previous_i(i,1)]
  S$Sim$K_Dir_Tree[i]= 0.4721 - 0.3973*S$Sim$LAD_Tree[previous_i(i,1)]
}


#' @rdname Light_extinction_K
#' @export
Metamodels= function(S,i){
  S$Sim$lue_Tree[i]= 2.87743 + 0.07595*S$Met_c$Tair[i] -
    0.03390*S$Met_c$VPD[i] - 0.24565*S$Met_c$PAR[i]

  S$Sim$T_Tree[i]=
    -0.50236 +  0.02240*S$Met_c$VPD[i] + 0.01321*S$Met_c$Tair[i] +
    0.72191*S$Sim$APAR_Tree[i] - 0.86182*(1-S$Met_c$FDiff[i]) + 0.07139*S$Sim$LAI_Tree[i]
  # 0.021820*S$Met_c$VPD[i] - 0.016112*S$Met_c$Tair[i] + 0.942021*S$Sim$APAR_Tree[i]-
  # 1.397349*(1-S$Met_c$FDiff[i]) + 0.004328*S$Sim$LAI_Tree[i]
  S$Sim$T_Tree[i][S$Sim$T_Tree[i]<0]= 0 #to discard negative values

  S$Sim$H_Tree[i]=
    0.34062 + 0.82001*S$Sim$APAR_Dir_Tree[i] + 0.32883*S$Sim$APAR_Dif_Tree[i]-
    0.75801*S$Sim$LAI_Tree[i] - 0.57135*S$Sim$T_Tree[i] -
    0.03033*S$Met_c$VPD[i]
    # 0.34975 + 0.81448*S$Sim$APAR_Dir_Tree[i] + 0.29321*S$Sim$APAR_Dif_Tree[i]-
    # 0.75987*S$Sim$LAI_Tree[i] - 0.55724*S$Sim$T_Tree[i] -
    # 0.02898*S$Met_c$VPD[i]
}

