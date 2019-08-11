
#' Bud induction window computation
#'
#' @description Bud induction can start only at F_Tffb degree-days after vegetative growth stops (Rodriguez et al., 2011).
#' The following function finds the vegetative growth end day, and add the F_Tffb parameter
#' (Time of first floral buds, in dd), then find the very first flowering of the year
#' and set the vector BudInitPeriod to TRUE between the two dates. So buds will appear
#' between plant F_Tffb parameter and the first flowering day only.
#'
#' @param S The list of simulation
#'
#' @return Nothing, modify the list of simulation `S` in place. See [DynACof()] for more details.
#'
#' @references Rodríguez, D., Cure, J., Cotes, J., Gutierrez, A. and Cantor, F., 2011. A coffee agroecosystem model: I. Growth and development of
#' the coffee plant. Ecological Modelling, 222(19): 3626-3639.
#'
bud_init_period= function(S){
  # Compute cumulative degree-days based on previous daily DD from semi-hourly data:
  CumulDegreeDays= cumsum(S$Met_c$DegreeDays)

  # Day of vegetative growth end:
  VegetGrowthEndDay= which(S$Met_c$DOY==S$Parameters$DVG2)
  # Temporary variables declaration:
  CumsumRelativeToVeget= CumsumRelativeToBudinit=
    matrix(data = NA, nrow = length(VegetGrowthEndDay), ncol = length(S$Met_c$Date))
  DateBudinit= DateFFlowering= NULL
  for (i in 1:length(VegetGrowthEndDay)){
    CumsumRelativeToVeget[i,]=
      CumulDegreeDays-CumulDegreeDays[VegetGrowthEndDay[i]-1]
    # Date of first bud initialisation:
    DateBudinit[i]= tail(which(CumsumRelativeToVeget[i,]<S$Parameters$F_Tffb),1)
    CumsumRelativeToBudinit[i,]=
      CumulDegreeDays-CumulDegreeDays[DateBudinit[i]-1]
    # Minimum date of first bud development end (i.e. without dormancy):
    BudDevelEnd= tail(which(CumsumRelativeToBudinit[i,]<S$Parameters$F_buds1),1)-1
    # Maximum date of first bud development end (i.e. with maximum dormancy):
    MaxDormancy= tail(which(CumsumRelativeToBudinit[i,]<S$Parameters$F_buds2),1)-1
    # Cumulative rainfall within the period of potential dormancy:
    CumRain= cumsum(S$Met_c$Rain[BudDevelEnd:MaxDormancy])
    # Effective (real) day of first buds breaking dormancy:
    BudDormancyBreakDay= BudDevelEnd + sum(CumRain<S$Parameters$F_rain)-1
    # Effective day of first flowers:
    DateFFlowering[i]=
      tail(which(CumsumRelativeToBudinit[i,]<
                   CumsumRelativeToBudinit[i,BudDormancyBreakDay]+S$Parameters$BudInitEnd),1)
    # Effective dates between which buds can appear
    S$Sim$BudInitPeriod[DateBudinit[i]:DateFFlowering[i]]= TRUE
  }
  S$Sim$BudInitPeriod[CumulDegreeDays<S$Parameters$VF_Flowering]= FALSE
}
