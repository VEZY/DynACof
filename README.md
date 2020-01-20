
<!-- README.md is generated from README.Rmd. Please edit that file -->

# [DynACof](https://vezy.github.io/DynACof): The Dynamic Agroforestry Coffee Crop Model <img src="man/figures/logo.png" alt="" width="300" align="right" />

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Travis Build
Status](https://travis-ci.com/VEZY/DynACof.svg?branch=master)](https://travis-ci.com/VEZY/DynACof)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/VEZY/DynACof?branch=master&svg=true)](https://ci.appveyor.com/project/VEZY/DynACof)
[![Join the chat at
https://gitter.im/DynACof/Lobby\#](https://badges.gitter.im/DynACof/Lobby.svg?token=1d2e733532f5122f05de&branch=master)](https://gitter.im/DynACof/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1256816.svg)](https://doi.org/10.5281/zenodo.1256816)

## Overview

The [DynACof](https://vezy.github.io/DynACof) process-based model
computes plot-scale Net Primary Productivity, carbon allocation, growth,
yield, energy, and water balance of coffee plantations according to
management, while accounting for spatial effects using metamodels from
the 3D process-based [MAESPA](https://maespa.github.io/). The model also
uses coffee bud and fruit cohorts for reproductive development to better
represent fruit carbon demand distribution along the year.

A research article presenting and evaluating the model is published in
Vezy et al. (2020). The official website is available
[here](https://vezy.github.io/DynACof).

DynACof is also available as a [Julia](https://julialang.org/) package.
Use this one for better performance (\~100x). Its repository is
available [here](https://github.com/VEZY/DynACof.jl), and the
documentation is available
[here](https://vezy.github.io/DynACof.jl/dev/). The Julia version is
also available from the R package. See [the
vignette](https://vezy.github.io/DynACof/articles/julia-version.html)
for more details. Note that the input parameter files are different from
the R-version. They are accessible in the
[DynACof.jl\_inputs](https://github.com/VEZY/DynACof.jl_inputs)
repository.

## Installation

The development version from [GitHub](https://github.com/) can be
installed with:

``` r
# install.packages("devtools")
devtools::install_github("VEZY/DynACof")
```

Or using the lightweight
[remotes](https://github.com/r-lib/remotes#readme) package:

``` r
# install.packages("remotes")
remotes::install_github("VEZY/DynACof")
```

The package is tested routinely to pass all
[CRAN](https://CRAN.R-project.org) tests using Travis-CI (linux) and
AppVeyor (Windows), but is not released to the CRAN servers because we
believe DynACof users are not widespread enough to bother CRAN people
and use their free server time.

## Example

This is a basic example using all defaults (parameters and meteorology)
over 2 years :

``` r
rm(list = ls())
library("DynACof")
Sys.setenv(TZ="UTC")
DynACof(Period= as.POSIXct(c("1979-01-01", "1980-12-31")))
```

To use your own data, you have to tell DynACof where to find it using
`Inpath` parameter, and what are the file names with the `FileName`
parameter list. A separate [Github
repository](https://github.com/VEZY/DynACof_inputs) is available for
input files templates, and some help on how to proceed.

Example using custom input parameter files:

``` r
rm(list = ls())
library("DynACof")
Sys.setenv(TZ="UTC")
DynACof(WriteIt = T, Period = as.POSIXct(c("1979-01-01", "1980-12-31")),
        Inpath = "1-Input/Aquiares/", Simulation_Name = "Test1",
        FileName = list(Site = "1-Site.R", Meteo ="2-Meteorology.txt",
                        Soil = "3-Soil.R",Coffee = "4-Coffee.R", Tree = NULL))
```

Note that the Meteo file can be of any regular format because the model
uses the `data.table::fread` function internally.

## Notes

The model first computes the shade tree, then the coffee and then the
soil. So if you need to update the metamodels, please keep in mind that
the state of soil of a given day is only accessible on the next day for
the tree and the coffee, unless the code is updated too. The model is
implemented like this for simplicity, based on the hypothesis that the
soil has a rather slow dynamic compared to plants dynamics.

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

## Acknowledgments

The DynACof model was mainly developed thanks to the MACCAC project,
which was funded by the french ANR (Agence Nationale de la Recherche).
The authors were funded by [CIRAD](https://www.cirad.fr/) and
[INRAE](https://www.inrae.fr/). The authors are grateful for the support
of the [Aquiares farm](https://aquiares.com/) and the
[CATIE](https://www.catie.ac.cr/) for the long-term coffee agroforestry
trial, the SOERE F-ORE-T which is supported annually by Ecofor, Allenvi
and the French national research infrastructure
[ANAEE-F](http://www.anaee-france.fr/fr/); the CIRAD-IRD-SAFSE project
(France) and the PCP platform of CATIE. CoffeeFlux observatory was
supported and managed by CIRAD researchers. We are grateful to the staff
from Costa-Rica, in particular Alvaro Barquero, Alejandra Barquero,
Jenny Barquero, Alexis Perez, Guillermo Ramirez, Rafael Acuna, Manuel
Jara, Alonso Barquero for their technical and field support.

MACACC project: ANR-13-AGRO-0005, Viabilité et Adaptation des
Ecosystèmes Productifs, Territoires et Ressources face aux Changements
Globaux, AGROBIOSPHERE 2013 program.

-----

<sub>The DynACof logo was made using
<a href="http://logomakr.com" title="Logo Makr">LogoMakr.com</a> </sub>

## References

<div id="refs" class="references">

<div id="ref-vezyDynACofProcessbasedModel2020">

Vezy, R., le Maire, G., Christina, M., Georgiou, S., Imbach, P.,
Hidalgo, H.G., Alfaro, E.J., Blitz-Frayret, C., Charbonnier, F., Lehner,
P., Loustau, D., Roupsard, O., 2020. DynACof: A process-based model to
study growth, yield and ecosystem services of coffee agroforestry
systems. Environmental Modelling & Software 124, 104609.
<https://doi.org/10.1016/j.envsoft.2019.104609>

</div>

</div>
