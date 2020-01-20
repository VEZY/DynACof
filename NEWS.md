# DynACof 1.2.1

* Tsoil used for Rm roots now  
* Remove `MaxTT` from `GDD`  
* Replace `Tleaf_Coffee` by `TairCanopy` in `DegreeDays_Tcan` and `T_VG` (for `ratioNodestoLAI`) as it should be (see documentation)  
* Put `CM_Fruit_Cohort_remain` to 0 when harvested  
* Add option for no harvest at all  
* `E_Soil` take in priority in surface layer, and then possibly in `W_1`  
* `LeafWaterPotential` is now fully computed  
* Rename `LeafWaterPotential` into `PSIL`  
* Add `PSIL_Tree` computation   
* Update default metamodels and parameters (revision of article)  
* Remove `LAI_max` dependence for leaf C demand computation  
* [DynACof.jl](https://github.com/VEZY/DynACof.jl) related:  
  * Update dynacof.jl_setup for dev versions  
  * Update `dynacof.jl_setup` doc + website  
  * Add Pkg.free when R terminates  
  * Update the code following modifs in julia version (from 69cd69ba69d210e0479b8749deebc9585b58c1c0 to 7239c93bc2c50eb761de330cf4c43429fe18b4ef)  
* Runs shade tree model only if `Stocking_Tree > 0.0` and remove overcomplicated shade tree models (keep only one now), following [DynACof.jl](https://github.com/VEZY/DynACof.jl)  
* Update docs to Roxygen 7.0.0.  
* Add startup message  
* Fix several bugs:  
  * Fix issue in the PENMON implementation  
  * FileName reading  
  * Fix format of simulation Table returned by DynACof with several cycles  
  * Fix error in dynacof_i  
  * Fix issues in the soil module  
  * Fix transpiration metamodel bug  
  * Fix the computation of the overriped fruits (could potentially count the same overriped fruits)  
  * Fix issue in soil RootWaterExtract_*  
  * Fix issue with LeafWaterPotential (was computed before T_Coffee)  

# DynACof 1.2.0

* Reformat the code so the shade tree, coffee, soil and energy balance are made by separate functions sequentially  
* Add dynacof_i to be able to change a simulation from one (or several) time step to another  
* Add DynACof.jl compatibility: now the user can use the Julia version of DynACof directly from the R using the R version of the package. This functionnality require Julia (v > 1.1) to be installed on the computer and available in the path.  

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
