# Purpose: To estimate the runoff generated from a land_use type. For non-rice units, a SCS-Curve number approach is used. For rice: a ponding effect is added.
#Inputs:
# CN2: the curve number of the current land use
# precip: the entire time series of rainfall used in the simulation (lazy loading)
# j: the current date of the simulation
########################################
# Debug:
# CN2 = 70
# precip = rain_ts
# j =7
# cur_precip = 100
# doy = 7
# precip5 = 100
runoff_gen<- function(CN2,precip,j){
  # Unit for SCS function needs to be in mm
  cur_precip = precip[j]*1000
  cur_doy = yday(cur_precip)
  #Basically calculate the antecedent conditions
  if (j > 5){
    precip5 = sum(precip[(j-5):(j-1)])*1000
  } else {precip5 = 0}
  
  if (is.numeric(CN2) == TRUE) {
    out_val = SCS_curve(CN2, cur_precip, precip5, cur_doy)
  } else {out_val = 0}
  return(out_val)
}
