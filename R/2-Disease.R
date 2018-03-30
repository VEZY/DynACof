#' American Leaf Spot
#'
#' @description Compute the percentage of leaves on coffee dying from
#'              American Leaf Spot disease, drought excluded.
#'
#' @param Elevation           Site elevation       (m.a.s.l)
#' @param SlopeAzimut         Slope azimuth        (degree)
#' @param Slope               Slope percentage     (\eqn{%})
#' @param RowDistance         Coffee rows distance (m)
#' @param Shade               Shade percentage     (\eqn{%})
#' @param CanopyHeight.Coffee Coffee Height        (m)
#' @param Fertilization       N fertilization per year
#' @param ShadeType           Shade type:  1= Legume only ; 2= bananas and legume ;
#'                                         3= bananas and other plants ; 4=	fruit and
#'                                         forest tree only ; 5= no shade
#' @param CoffeePruning       Character specifying the pruning management.
#'                            Values: tree, row, block or NULL.
#' @param df_rain             Data frame with DOY, year and Rain (mm) values
#'
#' @details It is good practice to use shade tree transmittance to compute \code{Shade} percentage
#'          (\eqn{Shade= 1-Transmittance}).
#'
#'
#' @return \item{ALS}{Percentage of dead leaves by ALS by day (\eqn{%  day-1})}
#'
#' @references Avelino et al. (2007) Topography and Crop Management Are Key Factors
#' for the Development of American Leaf Spot Epidemics on Coffee in Costa Rica
#' File: "Ia_digitized from Avelino 2007 - JA_lineaire.xlsx"
#' @examples
#' # df_rain has this structure :
#' df_rain= data.frame(DOY= 1:365, year= rep(2018,365), Rain= rnorm(n = 365,mean = 5, sd = 1))
#' ALS(Elevation = 1000, df_rain= df_rain)
#'
#' @export
ALS= function(Elevation, SlopeAzimut= 0, Slope=0, RowDistance= 1.5,
              Shade= 0, CanopyHeight.Coffee=2,Fertilization= 3,
              ShadeType= 5, CoffeePruning= c("tree","row","block",NULL),
              df_rain){

  CoffeePruning= match.arg(CoffeePruning)
  Defol_ALS_pc= rep(0,nrow(df_rain))
  ######### American Leaf Spot (ALS) ####
  ##Key Parameters for American Leaf Spot (ALS)

  Ia_Elevation= -0.000000000080801*Elevation^3 -
    0.0000068050865205*Elevation^2+0.0184187089250643*Elevation-
    11.9175485660755

  Ia_SlopeAzimut= 0.0000000000035758*SlopeAzimut^5 - 0.0000000002237767*
    SlopeAzimut^4 -0.0000013880057625*SlopeAzimut^3 +0.000492076366075*
    SlopeAzimut^2-0.046667060826288*SlopeAzimut + 0.492587332814335

  Ia_Slope= -0.0006135200057836*Slope^2+0.0611762490434003*Slope-
    0.96950860406751

  Ia_RowDistance= -1.67586900817065*RowDistance^2 + 4.61297139285148*
    RowDistance - 2.7957057499133

  Ia_Shade= 0.0180902153201516*Shade - 0.218143985209743
  Ia_CoffeeHeight= 0.734362166489921*CanopyHeight.Coffee - 1.36982218159893
  Ia_NbFertiliz= -0.163949361429501*Fertilization + 0.395095964560203

  Ia_ShadeType<-if(ShadeType==1){-0.3}else{
    if(ShadeType==2){-0.2}else{
      if(ShadeType==3){-0.18}else{
        if(ShadeType==4){0.65}else{
          if(ShadeType==5){0.28}}}}}
  Ia_CoffeePruning<-if(CoffeePruning=="tree"){0.05}else{
    if(CoffeePruning=="row"){0.3}else{
      if(CoffeePruning=="block"){-0.35}else{
        if(is.null(CoffeePruning)){0.65}}}}

  # American Leaf Spot :

  ShortDrought15JuneAugust_mm=
    df_rain%>%
    transmute(year, Rain, mid_june_to_mid_august= ifelse(DOY>=166&DOY<=227,T,F))%>%
    group_by(year)%>%
    transmute(SumRainJunetoAugust_Year = sum(Rain*mid_june_to_mid_august))%>%
    .[["SumRainJunetoAugust_Year"]]

  Ia_ShortDrought15JuneAugust= 0.0012200723943985*ShortDrought15JuneAugust_mm - 0.923932085933056

  Sum_Ia_ALS= Ia_Elevation+Ia_SlopeAzimut+Ia_Slope+Ia_RowDistance+Ia_Shade+
    Ia_CoffeeHeight+Ia_NbFertiliz+Ia_ShadeType+Ia_CoffeePruning+Ia_ShortDrought15JuneAugust
  Sum_Ia_ALS[Sum_Ia_ALS<0]= 0
  Sum_Ia_ALS= 0.2797*Sum_Ia_ALS+0.3202


  Defol_ALS_pc[df_rain$DOY>15&df_rain$DOY<166]= 0
  Defol_ALS_pc[df_rain$DOY>=166&df_rain$DOY<=366]=
    Sum_Ia_ALS[df_rain$DOY>=166&df_rain$DOY<=366]*
    exp(0.0180311*(df_rain$DOY[df_rain$DOY>=166&
                                 df_rain$DOY<=366]-166))
  Defol_ALS_pc[df_rain$DOY<=15]=
    Sum_Ia_ALS[df_rain$DOY<=15]*
    exp(0.0180311*(df_rain$DOY[df_rain$DOY<=15]-166+365))

  Defol_ALS= (Defol_ALS_pc-Defol_ALS_pc[previous_i(1:nrow(df_rain),n_prev = 1)])/100
  Defol_ALS[Defol_ALS<0]= 0

  return(Defol_ALS)
}
