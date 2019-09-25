#' Setup Julia call
#'
#' @description This function initializes Julia and the [DynACof.jl](https://github.com/VEZY/DynACof.jl) package.
#' It is time-consuming for the first time of execution because of the precompilation.
#'
#' @details This function requires Julia to be installed on your computer, and to be present in your path (i.e.
#' callable from anywhere using a terminal). The function will test if the [DynACof.jl](https://github.com/VEZY/DynACof.jl) package
#' is installed in Julia, and if not, it will install it for you. This process can take some time, so be patient the
#' first time you use it. If you encounter some issue, you can browe the issues from the [JuliaCall](https://github.com/Non-Contradiction/JuliaCall#juliacall-for-r-package-developers)
#' package, or fill one in the [DynACof](https://github.com/VEZY/DynACof.jl/issues) repository.
#'
#'
#' @note If you run into issues at this step, try to install the development version of `JuliaCall`:
#' `remotes::install_github("Non-Contradiction/JuliaCall")`. If it does not work either, try to open
#' Julia from the terminal, and run this command: `use Pkg; Pkg.add(["RCall","Suppressor","DynACof"])`
#' If you are using a new version of R, please use the argument `rebuild= TRUE` (see [JuliaCall::julia_setup()]).
#'
#' @param ... Parameters are passed down to [JuliaCall::julia_setup()]
#' @param dynacof_dev If a dev version of DynACof.jl is to be used and its path is not in `LOAD_PATH`, the path to
#' the `dev` location, *e.g.* "C:/Users/<User>/.julia/dev".
#'
#' @examples
#' \dontrun{
#' # Can be time-consuming the first time. Requires Julia + DynACof.jl
#' DynACof::dynacof.jl_setup()
#' }
#'
#' @export
dynacof.jl_setup= function (..., dynacof_dev= NULL){
  tryCatch(expr = {
    julia= JuliaCall::julia_setup(...)
  },
  error=function(cond) {
    message("Error during julia setup, try to install RCall, Suppressor and DynACof in a Julia terminal:")
    message(crayon::green$bold('using Pkg; Pkg.add(["RCall","Suppressor","DynACof"])'))
    message('Then pre-compile DynACof in Julia using this command:',crayon::green$bold("using DynACof"))
    message("Here's the original error message from JuliaCall::julia_setup:")
    message(cond)
  })

  if(!is.null(dynacof_dev)){
    JuliaCall::julia_command(paste0('push!(LOAD_PATH,"',dynacof_dev,'")'))
    JuliaCall::julia_command("using DynACof")
  }else{
    JuliaCall::julia_install_package_if_needed("DynACof")
    # JuliaCall::julia_command('Pkg.add(PackageSpec(url="https://github.com/VEZY/DynACof.jl"))')
    JuliaCall::julia_library("DynACof")
  }
  JuliaCall::julia_library("NamedTupleTools")
  message(crayon::green$bold$underline('Julia + DynACof.jl successfully instantiated'))
}



#' Update DynACof.jl
#'
#' @description Update the [DynACof.jl](https://github.com/VEZY/DynACof.jl) julia package.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Can be time-consuming.
#' DynACof::dynacof.jl_update()
#' }
dynacof.jl_update= function (){
  JuliaCall::julia_update_package("DynACof")
}


#' Run DynACof.jl (Julia version of DynACof)
#'
#' @description Make a simulation using the julia version of DynACof, [DynACof.jl](https://github.com/VEZY/DynACof.jl).
#' This implementation is much faster, with run-times ~100x faster. Need Julia installed on the computer and in the path.
#'
#' @param Period   Period of time to be simulated, see details. Default: `NULL`
#' @param WriteIt  If `TRUE`, write the outputs to disk using [write.results()], see details. Default: `FALSE`
#' @param output_f Output format. If `output_f = ".RData"`, the output list will be saved as a unique `.RData` file. Any other value:
#'                 write the output list in several `.csv` and `.txt` files. Default: `.RData`
#' @param Inpath   Path to the input parameter list folder, Default: `NULL` (take package values)
#' @param Outpath  Path pointing to the folder were the results will be writen, Default: `Outpath = Inpath`
#' @param Simulation_Name Character name of the simulation file name if `WriteIt = T`. Default: `"DynACof"`
#' @param FileName A list of input file names :
#' \describe{
#'   \item{Site}{Site parameters file name, see details. Default: `'site.jl'`}
#'   \item{Meteo}{Meteo parameters file name, see details. Default: `'meteorology.txt'`}
#'   \item{Soil}{Soil parameters file name, see details. Default: `'soil.jl'`}
#'   \item{Coffee}{Coffee parameters file name, see details. Default: `'coffee.jl'`}
#'   \item{Tree}{Shade tree parameters file name, see details. Default: `NULL`}
#' }
#'
#' @param ... Further arguments to pass to [write.results()].
#'
#' @note If the user needs default values from the package for FileName names, put their values to "package". Careful:
#' Always call [dynacof.jl_setup()] before runnning `dynacof.jl`. See example.
#'
#' @return Return invisibly a list containing three objects (Parameters, Meteo and Sim). See [DynACof()] for more details.
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting up julia + DynACof.jl:
#' dynacof.jl_setup()
#'
#' # Running a simulation:
#' # First, downloading an example meteorology file:
#' met_file= tempfile()
#' m= "https://raw.githubusercontent.com/VEZY/DynACof.jl_inputs/master/meteorology.txt"
#' download.file(m,met_file)
#'
#' S= dynacof.jl(Inpath= dirname(met_file), FileName=list(Site="package",Meteo=basename(met_file),
#'                                                        Soil="package",Coffee="package",Tree=NULL))
#' unlink(met_file)
#' # Plotting the coffee LAI along the simulation:
#' plot(S$Meteo$Date,S$Sim$LAI)
#' }
dynacof.jl= function(Period=NULL,WriteIt=FALSE,Inpath=NULL,output_f=".RData",
                     Outpath=Inpath,Simulation_Name="DynACof",
                     FileName=list(Site="site.jl",Meteo="meteorology.txt",Soil="soil.jl",
                                   Coffee="coffee.jl",Tree=NULL),...){

  if(any(grep(".R",FileName))){
    stop(paste("FileName argument should point to", crayon::green$bold$underline(".jl"),"files not",
               crayon::red$bold$underline(".R"),"files\n"))
  }

  FileName= paste0('(constants="package",site="',FileName$Site,'",meteo="',FileName$Meteo,'",soil="',
                   FileName$Soil,'",coffee="',FileName$Coffee,'",tree="',ifelse(is.null(FileName$Tree),"",FileName$Tree),'")')

  # Match the default values with julia call:
  if(is.null(Period)){
    Period= '["0000-01-01", "0000-01-02"]'
  }else{
    Period= paste0('["',format(Period[1],"%Y-%m-%d"),'","',format(Period[2],"%Y-%m-%d"),'"]')
  }
  if(is.null(Inpath)){Inpath= "package"}

  cmd= paste0('Sim, Meteo, Parameters= dynacof(period=',Period,',input_path="',Inpath,'",file_name=',FileName,");")
  # NB: writting is handle by R to be able to harness .rda capabilities.

  # Running the simulation:
  JuliaCall::julia_command(cmd)
  # Get the values of the outputs:
  Sim= JuliaCall::julia_eval("Sim")
  Meteo= JuliaCall::julia_eval("Meteo")
  Param_names= JuliaCall::julia_eval("String.(collect(keys(Parameters)))",need_return="R")
  Param_names= gsub("cp","Cp",Param_names)
  Parameters= JuliaCall::julia_eval("collect(Parameters)",need_return="R")
  names(Parameters)= Param_names
  Parameters$Bud_T_correction= JuliaCall::julia_eval("Parameters.Bud_T_correction", need_return = "Julia")

  message(paste("\n", crayon::green$bold$underline("Simulation completed successfully"),"\n"))
  FinalList= list(Sim= Sim,Meteo= Meteo,Parameters= Parameters)

  if(WriteIt){
    write.results(FinalList,output_f,Simulation_Name,Outpath,...)
  }

  return(FinalList)
}


#' Step-by-step [dynacof.jl()]
#'
#' @description Using DynACof one iteration after another using the julia version of DynACof, [DynACof.jl](https://github.com/VEZY/DynACof.jl).
#' Allows to run a DynACof simulation with starting at age > 0 with initializations.
#'
#' @field Careful Always call [dynacof.jl_setup()] before runnning `dynacof_i.jl`. See example.
#'
#' @param i Either an integer, or a range giving the day of simulation needed. Match the row index, so `i=1` make
#' a simulation for the first row of Sim and Met.
#' @param S The simulation list, output of [dynacof.jl()]. It is mandatory that `S` come from a [dynacof.jl()] simulation because
#' the parameters linked to a simulation have either `R` or `Julia` code affiliated to the simulation.
#'
#'
#' @return The modified simulation list `S`
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting up julia + DynACof.jl:
#' dynacof.jl_setup()
#'
#' Making a regular simulation using example data:
#' # First, downloading an example meteorology file:
#' met_file= tempfile()
#' m= "https://raw.githubusercontent.com/VEZY/DynACof.jl_inputs/master/meteorology.txt"
#' download.file(m,met_file)
#'
#'
#' S= dynacof.jl(Inpath= dirname(met_file), FileName=list(Site="package",Meteo=basename(met_file),
#'                                                        Soil="package",Coffee="package",Tree=NULL))
#' unlink(met_file)
#'
#' # Value of the maintenance respiration for coffee on day i=100:
#' i= 100
#' S$Sim$Rm[i]
#'
#' # Changing the value of Tair in the meteorology for day 100:
#' S$Meteo$Tair[i]= S$Meteo$Tair[i]+10.0
#'
#' S= dynacof_i(i,S)
#'
#' # New value of the maintenance respiration for coffee:
#' S$Sim$Rm[i]
#'
#' # To re-run DynACof for several days, use a range for `i`:
#' S= dynacof_i(i:(i+10),S)
#' }
dynacof_i.jl= function(i,S){
  # NB: S has to come from a dynacof.jl call because the Parameters contain functions coded either in R (DynACof()) or Julia (dynacof.jl)

  JuliaCall::julia_assign("i", as.integer(i))
  JuliaCall::julia_assign("Sim", S$Sim)
  JuliaCall::julia_assign("Meteo", S$Meteo)
  names(S$Parameters)= gsub("Cp","cp",names(S$Parameters))
  JuliaCall::julia_assign("Parameters", S$Parameters)

  # Making Parameters into a named tuple (using NamedTupleTools):
  JuliaCall::julia_command('Param_names= collect(keys(Parameters));')
  JuliaCall::julia_command("Param_values= collect(values(Parameters));")
  JuliaCall::julia_command("Parameters= namedtuple(Param_names)(Param_values);")

  # Running the simulation:
  JuliaCall::julia_command("dynacof_i!(i,Sim,Meteo,Parameters);")
  # Get the values of the outputs:
  # S$Sim[i,]= JuliaCall::julia_eval("Sim[i,:]")
  S$Sim= JuliaCall::julia_eval("Sim")
  names(S$Parameters)= gsub("cp","Cp",names(S$Parameters))
  S
}
