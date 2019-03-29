#' Tests on input parameters
#'
#' @description Different tests for the user-inputed parameter files:
#' \describe{
#'   \item{Mandatory parameter}{Test if all mandatory parameters are present in the files}
#'   \item{Parameter values}{Some parameter values are checked before running}
#' }
#'
#' @param Parameters The full list of parameter values, generally output from \code{\link{Import_Parameters}}
#' @param isTree       Boolean. Set to \code{TRUE} if shade trees are simulated.
#'
#' @return An error if any issue is encountered
#' @export
#'
#' @examples
#' # No error:
#' test_parameters(Parameters= Import_Parameters())
#'
#' \donttest{
#'  # Removing a mandatory parameter from the soil:
#'  params= Import_Parameters()
#'  params$REWc= NULL
#'  test_parameters(Parameters= params)
#' }
test_parameters= function(Parameters, isTree= FALSE){


  # Testing if mandatory parameters are present:
  # Reference parameters:
  ref_params=
    list(site= DynACof::site(),
         soil= DynACof::soil(),
         coffee= DynACof::coffee())

  missing_params=
    lapply(ref_params, function(ref,act= Parameters){
      ref_names= names(ref)
      act_names= names(act)
      match_params= match(ref_names,act_names)
      if(any(is.na(match_params))){
        return(ref_names[is.na(match_params)])
      }else{
        NULL
      }
    })




  errors=
    mapply(function(x,y){
      if(!is.null(x)){
        paste("Missing parameter in",y,"parameter file: ",paste(x, collapse = ", "))
      }
    },x= missing_params, y= names(missing_params))%>%unlist()

  if(!is.null(errors)){stop(paste(errors, collapse = "\n"),call.= FALSE)}

  # Tests on trees:
  if(isTree){
    # Mandatory parameters (could be more but not less):
    ref_param_names= names(DynACof::Tree())
    # Removing some E. poeppigiana exclusive parameters:
    ref_param_names= ref_param_names[-match(c("Kh", "KhExp", "Kc", "KcExp"),
                                            ref_param_names)]
    Actual_param_names= names(Parameters)
    matching_param= match(ref_param_names,Actual_param_names)
    if(any(is.na(matching_param))){
      stop("Missing parameter in Trees file: ",
           paste(ref_param_names[is.na(matching_param)], collapse = ", "),
           call.= FALSE)
    }

    # Ages requested for simulation:
    sim_ages= seq(Parameters$AgeCoffeeMin,Parameters$AgeCoffeeMax)

    # checking pa length for branch and stem:
    pa_Branch_Tree_ages= match(sim_ages,Parameters$pa_Branch_Tree$Age)
    if(any(is.na(pa_Branch_Tree_ages))){
      stop("Missing age in pa_Branch_Tree in Trees file: ",
           sim_ages[is.na(pa_Branch_Tree_ages)], call.= FALSE)
    }
    pa_Stem_Tree_ages= match(sim_ages,Parameters$pa_Stem_Tree$Age)
    if(any(is.na(pa_Stem_Tree_ages))){
      stop("Missing age in pa_Stem_Tree in Trees file: ",
           sim_ages[is.na(pa_Stem_Tree_ages)], call.= FALSE)
    }
  }

  invisible()
}
