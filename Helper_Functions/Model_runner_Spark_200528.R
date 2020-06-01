require('date')
require('lubridate')
require('xts')
source('myrain.R')
source('myPET.R')
source('myini.R')
source('hm_zone.R')
source('Water_table.R')
source("soil_moisture.R")
source("wetland_table.R")
source('SCS_curve_v4.R')
source('runoff_gen.R')


#Global parameters
years=20
start_year=1995

#Dates
dates1 = seq(ymd("1900-01-01"), length.out = years*365, by = "day")

#Rainfall parameters
alpha= 25 #mean rainfall on days with rainfall (units mm)
lambda=0.1 #Controls the difference between rainfall events
#Generate rainfall as a Poisson Distribution
rain1=myrain(years,alpha,lambda)/1000
rain_ts = xts(rain1, order.by = dates1)

#PET parameters
PET_min=0.5 #0.5mm
beta=-6 #Tweaked to give an annual PET between 600-1600mm 
pet1=myPET(start_year,years,PET_min,beta)/1000

#Soil Parameter Sets 

#Sandy Soil
Ksat=0.6 #m/day
psi_s= -0.12 #m
sfc=0.44
sw=0.17
n=0.4
b=4.05
sy=0.22
yc_up=-0.45
yc_wet=-0.30
Kbf = 8e-7 * 84600 #m/day

#Other constants
zcl=-10 #Confining layer only 2m deep
zwb=-2.25 #The bed of the wetland is 1.25m deep
yc_wet=-0.30+zwb
Area_total = 1000*10000 #m2 (1 ha = 10000m2)
Peri_total = sqrt(Area_total)*4
Area_wetland = 0.1*Area_total
Rad_wetland = sqrt(Area_wetland/pi)
Area_nonwet = 0.9*Area_total


RD=0.4 #Rooting depth value
RD_wet = 0.1

###############Surface Flow Constants
CN1 = 74


#################################################################################
#################################################################################

ysat = myini(0)
#Upland water table
up_water_table = as.data.frame(t(c(0,0,0,0,0,0,0,0,0,0,0,0)))
colnames(up_water_table) = c("GW_Exchange_Wet_Up_m","GW_Exchange_Wet_Up_m3","GW_ex_WL_change_Up", "GW_ex_WL_change_wet",
                             "GW_Exchange_Up_Down_m","GW_Exchange_Up_Down_m3","GW_ex_up_down_WL_change_m","ET_from_Up_water_table","Water_Volume_in_Up",
                             "Infilration_into_Up","Runoff_infil_excess","Up_water_table_change")
ywt = myini(-0.7)
yhm = myini(hm_zone(ywt[1],yc_up,RD,psi_s,sfc,b))
#Soil Moisture vector
sm_up = as.data.frame(t(c(0,0,0,0,sfc)))
colnames(sm_up) = c("Net_infil_mm","ET_mm","leakage_mm","change_sm","soil_moisture")
#sm_up = myini(sfc*yhm[1])
#wetland stage
wet_WL = as.data.frame(t(c(0,0,0,0,0)))
colnames(wet_WL) = c("Net_infil_mm","ET_mm","GW_ex_from_up","runoff","wetland_wl")
sm_wet = as.data.frame(t(c(0,0,0,0,sfc)))
colnames(sm_wet) = c("Net_infil_mm","ET_mm","leakage_mm","change_sm","soil_moisture")


ywet = myini(-0.4)
ywet_hm = myini(hm_zone(ywet[1],yc_wet,RD,psi_s,sfc,b))

runoff_up = myini(0)

i = 2
for (i in 2:2000){
  cur_rain2 = coredata(rain_ts[i])
  #Runoff Generation:
  runoff_up[i] = runoff_gen(CN1,rain_ts,i)
  #Soil moisture dynamics if the low moisture zone exists
  sm_up[i,] = soil_moisture(ywt[i-1],yhm[i-1],up_water_table$ET_from_Up_water_table[i-1],sm_up$soil_moisture[i-1],pet1[i],cur_rain2)    
  #Upland Dynamics  
  ysat[i] = ywt[i-1]+psi_s
  up_water_table[i,] = Water_table(ywt[i-1],ywet[i-1],zcl,yhm[i-1],sm_up$soil_moisture[i],sm_up$leakage_mm[i],pet1[i],cur_rain2)
  #Water Table change
  ywt[i] = up_water_table$Up_water_table_change[i]
  #Low Moisture zone dynamics
  yhm[i] = hm_zone(ywt[i],yc_up,RD,psi_s,sfc,b)
  #Wetland Dynamics
  sm_wet[i,] = soil_moisture(ywet[i-1],ywet_hm[i-1],wet_WL$ET_mm[i-1],sm_wet$soil_moisture[i-1],pet1[i],cur_rain2)
  wet_WL[i,] = wetland_table(up_water_table$GW_ex_WL_change_wet[i],ywet[i-1],zcl,ywet_hm[i-1],sm_wet$soil_moisture[i],sm_wet$leakage_mm[i],pet1[i],cur_rain2)
  ywet[i] = wet_WL$wetland_wl[i]
  ywet_hm[i] = hm_zone(ywet[i],yc_wet,RD,psi_s,sfc,b)
}



plot(ywt,type = 'l',ylim = c(-5,0))
lines(ywet,col = 'green')
lines(yhm,col = 'blue')
