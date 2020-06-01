# Purpose: To estimate the soil moisture dynamics in the low moisture zone at a given timestep
# up_wt = ywt[i-1]
# yhm2 = yhm[i-1]
# et_up = up_water_table$ET_from_Up_water_table[i-1]
# sm = sm_up$soil_moisture[i-1]
# pet = pet1[i]
# rain = rain1[i]
# #Basically checks if the debugging input is equal to the input into the function call
# c(ywt[i-1],yhm[i-1],up_water_table$ET_from_Up_water_table[i-1],sm_up$soil_moisture[i-1],pet1[i],cur_rain2) == c(up_wt,yhm2,et_up,sm,pet,rain)

# up_wt = ywet[i-1]
# yhm2 = ywet_hm[i-1]
# et_up = wet_WL$ET_mm[i-1]
# sm = sm_wet$soil_moisture[i-1]
# pet = pet1[i]
# rain = rain1[i]
# #Basically checks if the debugging input is equal to the input into the function call
# c(ywet[i-1],ywet_hm[i-1],wet_WL$ET_mm[i-1],sm_wet$soil_moisture[i-1],pet1[i],cur_rain2) == c(up_wt,yhm2,et_up,sm,pet,rain)


soil_moisture <- function(up_wt,yhm2,et_up,sm,pet,rain) {
  if (yhm2 < 0){
  #print(c(up_wt,yhm2,et_up,sm,pet,rain))  
  out1 = vector()
  #Basically ensure that the water that infiltrates first fills the low moisture zone
  #print(sm_avail)
  vol_avail = ((n*-yhm2) - (n*-yhm2*sm))
  if (vol_avail>rain){
    infil = rain
  } else {infil = vol_avail}
  #print(c(infil,"infil"))
  ##
  #Subtract the ET that takes place from the water table
  pet_lm = pet - et_up
  et = pet_lm*((sm - sw)/(sfc-sw))
  #print(c(pet_lm,pet,et_up))
  #print(c(et,'et'))
  #Estimate beta which is derived from b (obtained from Clapp and Hornberger, 1978)
  #Laio et al (2001) has details on estimating leakage amount from soil layer
  beta = 4+2*b
  if ((sm > sfc) & (sm < 1)){ #Ensures leakage is only when soil moisture exceed field capacity
  ls = (Ksat/((exp(beta*(1-sfc)))-1))*((exp(beta*(sm-sfc)))-1)
  # Basically ensuring that if the leakage estimated based on ksat exceed the amount of water that is available to be drained
  # then the total drained amount is the water available to be drained
  drainable = ((n*-yhm2*sm) - (n*-yhm2*sfc))
    if ((ls > drainable) & (drainable>et)){ #The ET condition makes sure that the water leaves as ET before leaving as ls
      ls = drainable - et
    } else {ls = 0}
  } else{ls = 0}
  #print(c(ls,'ls'))
  ##
  change_sm = (infil-et-ls)
  #print(c(change_sm,'change_sm'))
  vol_new = (n*abs(yhm2)*sm) + change_sm
  sm_new = vol_new/(n*abs(yhm2))
  #Output vector
  out1 = c(infil,et,ls,change_sm,sm_new)
  } else{
    out1 = c(0,0,0,0,sfc)
  }
}