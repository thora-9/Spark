# # Purpose: To estimate the water table depth based inputs and outputs at a given timestep
# up_wt = ywt[i-1]
# wet_stage = ywet[i-1]
# down_wt = zcl
# yhm2 = yhm[i-1]
# sm = sm_up$soil_moisture[i]
# leak = sm_up$leakage_mm[i]
# pet = pet1[i]
# rain = rain1[i]
# c(up_wt,wet_stage,down_wt,yhm2,sm,leak,pet,rain) == c(ywt[i-1],ywet[i-1],zcl,yhm[i-1],sm_up$soil_moisture[i],sm_up$leakage_mm[i],pet1[i],cur_rain2)

Water_table <- function(up_wt,wet_stage,down_wt,yhm2,sm,leak,pet,rain) {
  out1 = vector()
  if (up_wt > yc_up){
    GW_l_ex = Ksat*(wet_stage-up_wt)
    Wet_peri = (2*pi*Rad_wetland)*(abs(wet_stage-zwb))
    GW_l_vol = GW_l_ex*Wet_peri
    GW_l = GW_l_vol / Area_nonwet
    GW_l_wet = GW_l_vol/Area_wetland
    #Exchange with downstream aquifer
    GW_bf_ex = Kbf*(zcl - up_wt)
    #Volume of water leaving as baseflow/ 
    #use the perimeter to get exchange rate
    GW_bf_vol = GW_bf_ex*Peri_total 
    GW_bf = GW_bf_vol/Area_nonwet
    #ET estimate
    ET_up = pet
    #Recharge estimate
    cur_storage = n*(-zcl+up_wt)
    max_storage = n*(-zcl)
    avail_storage = max_storage - cur_storage
    if (rain < avail_storage){
      R_up = rain
      runoff = 0
    } else {
      R_up = avail_storage
      runoff = rain - avail_storage
    }
    #Water Table change
    ywt_out = up_wt+(1/sy)*(R_up+GW_l+GW_bf-ET_up)
    out1 = c(GW_l_ex, GW_l_vol,GW_l,GW_l_wet,GW_bf_ex,GW_bf_vol,GW_bf,ET_up,cur_storage,R_up,runoff,ywt_out)
  } else if (up_wt < yc_up){
    #I think we're assuming a 1 meter dl value in darcy's law
    GW_l_ex = Ksat*(wet_stage-up_wt)/1
    #Estimating the volumetric exchange take the area/perimeter of a cylinder going from wetland water level to confining layer
    Wet_peri = (2*pi*Rad_wetland)*(abs(wet_stage-zcl))
    GW_l_vol = GW_l_ex*Wet_peri
    GW_l = GW_l_vol/Area_nonwet
    GW_l_wet = GW_l_vol/Area_wetland
    #Exchange with downstream aquifer
    GW_bf_ex = Kbf*(zcl - up_wt)
    #Volume of water leaving as baseflow/ 
    #use the perimeter to get exchange rate
    GW_bf_vol = GW_bf_ex*Peri_total 
    GW_bf = GW_bf_vol/Area_nonwet
    #ET estimate
    ET_up = pet*exp(yhm2/RD)
    #Recharge estimate
    cur_storage = n*(-zcl+up_wt)
    max_storage = n*(-zcl)
    avail_storage = max_storage - cur_storage
    #Basically in the second case, we subtract the amount of water that can be stored in the low moisture zone from the total infiltrated amount available
    #as recharge for the yhm2 and water table --> max storage in low moisture zone = -(yhm2*n)
    #
    sm_avail = ((n*-yhm2) - (n*-yhm2*sm))
    if (sm_avail>rain){
      infil = 0
    } else {infil = rain - sm_avail}
    #This cur_rain value is just rainfall infiltration after taking the SM layer water storage into account
    cur_rain = infil
    #
    if (rain < avail_storage){
      R_up = cur_rain
      runoff = 0
    } else {
      R_up = avail_storage
      runoff = cur_rain - avail_storage
    }
    #Water Table change
    #Leakage from the low moisture layer is positive; therefore is positive for the water table
    ywt_out = up_wt+(1/sy)*((R_up+leak)+GW_l+GW_bf-ET_up)
    #Basically add a condition that prevents the water table from falling lower than the confining layer
    if (ywt_out < zcl){
      ywt_out = zcl
    }
    out1 = c(GW_l_ex, GW_l_vol,GW_l,GW_l_wet,GW_bf_ex,GW_bf_vol,GW_bf,ET_up,cur_storage,R_up,sm_avail,ywt_out)
  }
  return(out1)
}
  
