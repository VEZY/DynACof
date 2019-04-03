#' Import model parameters
#'
#' @description Import the parameters from local files, or from default (from package data)
#'
#' @param path   The path to the parameter files folder. If `NULL`, take the default files from the package
#' @param Names  List of the file names. Default: `list(Site="Site.R",Soil="Soil.R",Coffee="Coffee.R",Tree=NULL)`
#'
#' @details For the full list of parameters and the format of the parameter files, see [site()].
#'          The function return the parameter files path in the list, to access it, see example.
#'
#' @return A list of all input parameters for DynACof
#'
#' @examples
#' # Importing defaults:
#' Parameters= Import_Parameters()
#' # Parameters files path:
#' Parameters$files
#'
#' @seealso [DynACof()] [site()]
#'
#' @export
Import_Parameters= function(path= NULL,
                            Names= list(
                              Site="Site.R",
                              Soil="Soil.R",
                              Coffee="Coffee.R",
                              Tree=NULL)){
  Filespath= Names
  Filespath= lapply(Filespath, function(x)x="No parameters")
  if(!is.null(path)){
    for(i in 1:length(Names)){
      if(is.character(Names[[i]])){
        if(is(try(source(file = file.path(path,Names[[i]])),silent = T),"try-error")){
          warning(paste(names(Names[i]),"parameter file not found, taking default package values"))
          Filespath[names(Names[i])]= "Default from package function"
        }else{
          message(paste(names(Names[i]),"parameters taken from",file.path(path,Names[i])))
          Filespath[names(Names[i])]= file.path(path,Names[[i]])
          source(file.path(path,Names[[i]]),local = TRUE)
        }
      }
    }
  }else{
    Filespath= lapply(Filespath, function(x)x="Default from package function")
  }
  Parameters= c(Constants(),site(),coffee(),soil(),if(!is.null(Names$Tree)){Tree()}else{list(Tree_Species= "No_Shade")})
  Parameters$files= Filespath
  return(Parameters)
}
