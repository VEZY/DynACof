#' MAESPA metamodels
#'
#' @description Several variables can be highly impacted by the 3D canopy structure. Hence, some variables are
#'              computed using metamodels from the MAESPA model (see details). These functions are example functions
#'              for metamodels and allometries. They are imported by the parameter files for a default simulation.
#'              Any simulation on different conditions than the one used as in the package default should
#'              have custom metamodels, and/or allometries. The user can write the customized functions directly
#'              in the parameter files were these functions are called. For example, to replace the light extinction
#'              coefficient computation, the user should write a customized function instead of the call to
#'              `Light_extinction_K()` on the line `k= Light_extinction_K`.
#'              These functions are helper functions available to the user to use as a template,
#'              but not to call them directly.
#'
#' @param S  The global list of class "Simulation" used by the DynACof model.
#' @param i  The day of interest.
#'
#' @details  The purpose of using metamodels in DynACof is to make this two-layer dynamic crop model able to consider
#'           the 3D canopy heterogeneity effect on fundamental processes, as if it was with a MAESPA-growth model
#'           coupling. The main process impacted by canopy complexity is the light absorbed by the plants
#'           (Charbonnier et al., 2013). Indeed, in dynamic crop models, the absorbed photosynthetically active
#'           radiation (APAR) by the canopy is often computed using the simple Beer-Lambert’s law or a derivative,
#'           with a variable leaf area index in time, but a constant extinction coefficient (Van Oijen et al., 2010b).
#'           However, heterogeneous canopies such as the shade trees in AFS coffee plantations tends to violate the
#'           assumption of a constant value for the diffuse (\eqn{K_{dif_{Tree}}}{K_dif_Tree}) and direct (\eqn{K_{dir_{Tree}}})
#'           light extinction coefficients, because the spatial distribution of the leaf area is not uniform (high gap fraction)
#'           and because the leaf area density can change along time with foliage aggregation (Sampson and Smith,
#'           1993;Sinoquet et al., 2007). Secondly, a comparison between coffee planted in monoculture and under
#'           agroforestry system showed that canopy complexity affected canopy temperature, water, and energy
#'           partitioning (Vezy et al., 2018), and probably photosynthesis because it is related to light interception
#'           and transpiration through stomatal conductance. Therefore, we derived metamodels from MAESPA for the
#'           diffuse (\eqn{K_{dif_{Tree}}}{K_dif_Tree}) and the direct (\eqn{K_{dir_{Tree}}}{K_dir_Tree}) shade tree light
#'           extinction coefficients, the light use efficiency (LUE, gC MJ-1), the coffee canopy temperature (Tcan, deg Celsius)
#'           and leaf water potential (MPa), the transpiration (T_x, mm) and plant sensible heat flux (H_x). The coffee layer was
#'           considered homogeneous enough to compute constant extinction coefficients derived from the MAESPA simulation,
#'           and the partitioning parameter between soil sensible and latent flux was also adjusted using MAESPA outputs.
#'           MAESPA is a 3D explicit model for energy, carbon and water fluxes simulation, for further details, see:
#'           \href{https://www.researchgate.net/publication/323398728_Measuring_and_modelling_energy_partitioning_in_canopies_of_varying_complexity_using_MAESPA_model}{Vezy et al. (2018)},
#'           or the \href{https://maespa.github.io/}{MAESPA website}.
#'
#' @return \item{\eqn{K_Dif_Tree}{K_{dif_{Tree}}}}{Shade tree diffuse light coefficient}
#'         \item{\eqn{K_Dir_Tree}{K_{dir_{Tree}}}}{Shade tree direct light coefficient}
#'         \item{\eqn{Rn_Soil}{Rn{Soil}}}{Soil net radiation (MJ m-2 d-1)}
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
#' @seealso [DynACof()]
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
Metamodels_soil= function(S,i){
  S$Sim$Rn_Soil[i]= -1.102 + 1.597*S$Sim$PAR_Trans[i] + 1.391*sqrt(1-S$Met_c$FDiff[i])
}


#' Shade tree allometries (optional)
#'
#' @description Compute shade tree allometries, which are optional in the model.
#'              The function is available to easily change or add equations, not to be called directly.
#'
#'
#' @param S  The list of class "Simulation" used by the DynACof model.
#' @param i  The index of the day.
#' @details  This function is called from the [Tree()] parameter functions, and then by the model.
#'           In-depth details are available in \href{https://goo.gl/NVxcVp}{Vezy (2017)}
#' @return   Any variable that is computed by the function. Default: `DM_Stem_Tree`, `Height_Tree`,
#'           `CrownProj_Tree`, `CrownRad_Tree`, `Crown_H_Tree`, `Trunk_H_Tree`,
#'           `LA_Tree and LAD_Tree`.
#'
#' @references Vezy, R., Simulation de pratiques de gestion alternatives pour l’adaptation des plantations pérennes
#'             aux changements globaux, in École doctorale science de l'environnement, spécialité physique de
#'             l'environnement. 2017, UNIVERSITÉ DE BORDEAUX: Bordeaux. p. 270.\href{https://goo.gl/NVxcVp}{Link}
#'
#' @export
Allometries= function(S,i){
  S$Sim$DBH_Tree[i]=
    ((S$Sim$DM_Stem_Tree[i]/
        (S$Parameters$CC_wood_Tree*1000*S$Sim$Stocking_Tree[i])/0.5)^0.625)/100
  # Source: Rojas-García et al. (2015) DOI: 10.1007/s13595-015-0456-y
  # /!\ DBH is an average DBH among trees.
  #Tree Height. Source:  CAF2007 used in Van Oijen et al. (2011). With no pruning :
  S$Sim$Height_Tree[i]=
    round(S$Parameters$Kh*(((S$Sim$DM_Stem_Tree[i]/1000)/
                              S$Sim$Stocking_Tree[i])^S$Parameters$KhExp),2)

  # Crown projected area:
  S$Sim$CrownProj_Tree[i]=
    round(S$Parameters$Kc*(((S$Sim$DM_Branch_Tree[i]/1000)/
                              S$Sim$Stocking_Tree[i])^S$Parameters$KcExp),2)
  # Source: Van Oijen et al. (2010, I).
  S$Sim$CrownRad_Tree[i]= sqrt(S$Sim$CrownProj_Tree[i]/pi)
  S$Sim$Crown_H_Tree[i]= S$Sim$CrownRad_Tree[i] # See Charbonnier et al. 2013, Table 2.
  S$Sim$Trunk_H_Tree[i]= S$Sim$Height_Tree[i]-S$Sim$Crown_H_Tree[i]

  # If there is a pruning management, change the allometries (mostly derived from Vezy et al. 2018) :
  if(S$Sim$Plot_Age[i]%in%S$Parameters$Pruning_Age_Tree){
    # Pruning : trunk height does not depend on trunk dry mass anymore (pruning effect)
    S$Sim$Trunk_H_Tree[i]= round(3*(1-exp(-0.2-S$Sim$Plot_Age_num[i])),2)
    S$Sim$Height_Tree[i]= S$Sim$Crown_H_Tree[i]+S$Sim$Trunk_H_Tree[i]
    # The equation make it grow fast at the early stages and reach a plateau at the
    # maximum height after ca. few months.
  }else if(any(S$Sim$Plot_Age[i]>S$Parameters$Pruning_Age_Tree)){
    # if there were any pruning before, add the trunk
    Lastheight_Trunk=
      round(3*(1-exp(-0.2-S$Parameters$Pruning_Age_Tree[
        tail(which(S$Sim$Plot_Age[i]>S$Parameters$Pruning_Age_Tree),1)]+1)),2)
    S$Sim$Height_Tree[i]=
      S$Parameters$Kh*(((S$Sim$DM_Stem_Tree[i]/1000)/
                          S$Sim$Stocking_Tree[i])^S$Parameters$KhExp)+Lastheight_Trunk
    S$Sim$Trunk_H_Tree[i]= S$Sim$Height_Tree[i]-S$Sim$Crown_H_Tree[i]
  }
  S$Sim$LA_Tree[i]= S$Sim$LAI_Tree[i]/S$Sim$Stocking_Tree[i]
  S$Sim$LAD_Tree[i]=
    S$Sim$LA_Tree[i]/((S$Sim$CrownRad_Tree[i]^2)*
                        (0.5*S$Sim$Crown_H_Tree[i])*pi*(4/3))
  S$Sim$LAD_Tree[i][is.nan(S$Sim$LAD_Tree[i])]= 0.21
  S$Sim$LAD_Tree[i][S$Sim$LAD_Tree[i]<0.21]= 0.21
  S$Sim$LAD_Tree[i][S$Sim$LAD_Tree[i]>0.76]= 0.76
}
