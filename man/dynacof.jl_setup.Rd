% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/4-dynacof_julia.R
\name{dynacof.jl_setup}
\alias{dynacof.jl_setup}
\title{Setup Julia call}
\usage{
dynacof.jl_setup(..., dev_path = NULL)
}
\arguments{
\item{...}{Parameters are passed down to \code{\link[JuliaCall:julia_setup]{JuliaCall::julia_setup()}}}

\item{dev_path}{the path to the \code{dev} folder location (\emph{e.g.} "C:/Users/{User}/.julia/dev") if a dev version of \code{DynACof.jl} has to be used.}
}
\description{
This function initializes Julia and the \href{https://github.com/VEZY/DynACof.jl}{DynACof.jl} package.
It is time-consuming for the first time of execution because of the precompilation.
}
\details{
This function requires Julia to be installed on your computer, and to be present in your path (i.e.
callable from anywhere using a terminal). The function will test if the \href{https://github.com/VEZY/DynACof.jl}{DynACof.jl} package
is installed in Julia, and if not, it will install it for you. This process can take some time, so be patient the
first time you use it. If you encounter some issue, you can browe the issues from the \href{https://github.com/Non-Contradiction/JuliaCall#juliacall-for-r-package-developers}{JuliaCall}
package, or fill one in the \href{https://github.com/VEZY/DynACof.jl/issues}{DynACof} repository.
}
\note{
If you run into issues at this step, try to install the development version of \code{JuliaCall}:
\code{remotes::install_github("Non-Contradiction/JuliaCall")}. If it does not work either, try to open
Julia from the terminal, and run this command: \verb{using Pkg; Pkg.add(["RCall","Suppressor","DynACof"])}
If you are using a new version of R, please use the argument \code{rebuild= TRUE} (see \code{\link[JuliaCall:julia_setup]{JuliaCall::julia_setup()}}).
}
\examples{
\dontrun{
# Can be time-consuming the first time. Requires Julia + DynACof.jl
DynACof::dynacof.jl_setup()
}

}
