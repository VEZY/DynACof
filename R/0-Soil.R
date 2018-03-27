#' @rdname site
#' @export
soil= function(){
  list(
    TotalDepth      = 3.75,     # Total soil depth (m)
    Wm1             = 210,      # Minimum water content of the first layer (mm)
    Wm2             = 58,       # Minimum water content of the second layer (mm)
    Wm3             = 64,       # Minimum water content of the third layer (mm)
    Wf1             = 290,      # Field capacity of the first layer (mm)
    Wf2             = 66,       # Field capacity of the second layer (mm)
    Wf3             = 69,       # Field capacity of the third layer (mm)
    EWMtot          = 93,       # = (Wf1-Wm1)+(Wf2-Wm2)+(Wf3-Wm3) (mm)
    IntercSlope     = 0.2,      # Rainfall interception coefficient (mm LAI-1)
    WSurfResMax     = 120,      # Maximum soil water level at the surface reservoir. Above this value, excess rainfall runs-off inmediately (mm)
    fc              = 13.4,     # Minimum infiltration capacity (mm d-1)
    alpha           = 101.561,  # Multiplicative coefficient for the maximum infiltration capacity (alpha >= 1)
    fo              = 1360.917, # Maximum infiltration capacity (mmd-1), = fc*alpha
    kB              = 0.038079, # Discharge coefficient for surface runoff from surface reservoir (d-1)
    k_Rn            = 0.283,    # Radiation extinction coefficient. Source: Shuttleworth and wallace, 1985, p. 851
    Soil_LE_p       = 0.66,     # Partitioning of the available energy between LE and H for the soil
    PSIE            = -0.0002580542, # Average PSIE, used for soil water potential through Campbell (1974) equation, (MPa).
    PoreFrac        = 0.4,      # Average pore fraction of the soil, IDEM
    B               = 4.71,     # Average b of the soil, IDEM
    RootFraction1   = 0.87,     # Root fraction in the first layer (compared to total root biomass)
    RootFraction2   = 0.069,    # Root fraction in the second layer
    RootFraction3   = 0.061,    # Root fraction in the third layer
    REWc            = 0.40      # Constant critical relative extractable water. Source Granier et al., 1999 Biljou
  )
}
