# DynACof 1.1.2

* Externalize coffee metamodels to parameter file + remove Tcan_MAESPA_Coffee as it is computed by the model now
* Add tests for user-defined input parameter  
* Add option for model parallelization over crop rotations  
* Add ZEN to meteo  
* Fix issue on Tleaf_Tree + unit of DELM  
* Simplify writing of equations for Tleaf_*
* Use markdown in package documentation
* Update CB with flowers per inflorescences + its doc (+ doc for CN)
* Several bugs/typos fix: e.g. VPD unit when not read from meteo file + 
* Update parameter values to match those from the article

# DynACof 1.1.1

* Add Temperature-dependent correction for buds (CB) as a function  
* Add Temperature-dependent correction for nodes (CN) as a function  
* Simplify derivative function of the logarithm (logistic_deriv)  
* Add an option for harvest: optimize quantity or quality  
* Rename the parameters as in the scientific article and update the inputs
* Update documentation


# DynACof 1.1.0

* Add soil surface temperature computation
* DynACof now calls mainfun(), which regroups all computations.
* update documentation for metamodels

# DynACof 1.0.2

* Add templates for input parameters
* Simulation saved using saveRDS (better coding practice)
* Clean old comments in the model
* Update documentation
* Update Website

# DynACof 1.0.1

Several imrpovements:

* epsilon parameters are now true growth cost coefficients (1/previous_espilon)
* Renaming : RainForBudBreak -> R_break ; Reserves_Tree -> RE_Tree
* Tree allometries are now into metamodels
* Use Ks from parameter file instead of hard coded values
* Update Coffea sensible heat flux metamodel
* New computation of Rn_Soil using MAESPA metamodels + replace PARcof by PAR_Trans_Tree and add PAR_Trans (PAR transmitted to soil)
* Add website using pkgdown
* Rm now computed using TairCanopy
* Update T_Tree metamodel
* Add WoodPruningRate as a parameter (it was hard coded !)
* Remove unused WoodDensity_kgDMm3


# DynACof 0.1.2

First news. Will be updated from now on.
