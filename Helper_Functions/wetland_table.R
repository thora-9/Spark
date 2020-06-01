# Purpose: To estimate the wetland water level/depth based inputs and outputs at a given timestep
#Parameter Descriptions:
#gwl_ex = The groundwater exchange from the upland water table calculated above
#wet_stage: The wetland_stage from the previous timestep
#Downstream_water: not used currently; potentially useful for tank systems
#yhm2: the high moisture zone boundary; only exists when the wetland is empty
#     and the water level under the wetland is below the critical depth of the wetland
#sm: tracks the water level of the low moisture zone; kicks only when yhm != 0
#leak: the leakage from the sm zone; zero if yhm = 0
#pet: potential evapotranspiration rate forcing data
#rainfall: forcing data

# gwl_ex = (up_water_table$GW_ex_WL_change_wet[i])
# wet_stage = ywet[i-1]
# down_wt = zcl
# yhm2 = ywet_hm[i-1]
# sm = sm_wet$soil_moisture[i]
# leak = sm_wet$leakage_mm[i]
# pet = pet1[i]
# rain = rain1[i]
# 
# (c(up_water_table$GW_ex_WL_change_wet[i],ywet[i-1],zcl,ywet_hm[i-1],sm_wet$soil_moisture[i],sm_wet$leakage_mm[i],pet1[i],cur_rain2) ==
#   c(gwl_ex,wet_stage,down_wt,yhm2,sm,leak,pet,rain))

wetland_table <- function(gwl_ex,wet_stage,down_wt,yhm2,sm,leak,pet,rain) {
  out1 = vector()
  #################################################################################
  if (wet_stage> zwb){   #Basically the condition when wetland has water stored in it
    infil = rain
    et = pet
    wet_change = wet_stage + (1/1)*(rain-gwl_ex-et)
    if(wet_stage>0){
      wet_change = 0
      runoff = wet_change 
    } else {runoff = 0}
    out1 = c(infil,et,gwl_ex,runoff,wet_change)
  #################################################################################
  } else if (wet_stage > (yc_wet)){   #Basically the condition when wetland has no water stored; but wetland level is above the critical depth
    infil = rain
    et = pet
    wet_change = wet_stage + (1/sy)*(rain-gwl_ex-et)
    if(wet_stage>0){
      wet_change = 0
      runoff = wet_change 
    } else {runoff = 0}
    out1 = c(infil,et,gwl_ex,runoff,wet_change)
  #################################################################################
  } else if (wet_stage < (yc_wet)) {
  #ET estimate
  et = pet*exp(yhm2/RD)
  #Basically in the second case, we subtract the amount of water that can be stored in the low moisture zone from the total infiltrated amount available
  #as recharge for the yhm2 and water table --> max storage in low moisture zone = -(yhm2*n)
  #
  sm_avail = ((n*-yhm2) - (n*-yhm2*sm))
  if (sm_avail>rain){
    infil = 0
  } else {infil = rain - sm_avail}
  #This cur_rain value is just rainfall infiltration after taking the SM layer water storage into account
  cur_rain = infil
  R_up = cur_rain
  runoff = 0
  #Water Table change
  wet_change = wet_stage + (1/sy)*((R_up+leak)-gwl_ex-et)
  #Basically add a condition that prevents the water table from falling lower than the confining layer
  if (wet_change < zcl){
    wet_change = zcl
  }
  out1 = c(infil,et,gwl_ex,runoff,wet_change)
  }
  return(out1)
}