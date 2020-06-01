SCS_curve <- function(cur_CN,cur_P,rain_5,doy){
  #Convert the CN to retention parameter using emperical equation
  S=(25400/cur_CN)-254
  #Change the retnetion parameter based on the antecedent conditions
  #AMC 1
  if (doy > 61 & doy < 182){
    if (rain_5 < 13){
      S_AMC=2.281*S
    } else if (rain_5 > 28){
      S_AMC=0.427*S
    } else {S_AMC=S}
  } else {
    if (rain_5 < 36){
      S_AMC=2.281*S
    } else if (rain_5 > 53){
      S_AMC=0.427*S
    } else {S_AMC=S}
  }
  #Instead of converting the curve number first and then estimating retention parameter; use the direct relations developed by
  # Hawkins 1985 to get the antecedent specific retention parameter
  # if(rain_5<36){#0.96*25.4)){
  #   S_AMC=2.281*S
  # } else if(rain_5>53){#(1.6*25.4)){ #AMC 3
  #   S_AMC=0.427*S
  # } else{S_AMC=S} #AMC 2
  # 
  #Estimate the daily runoff based on the retention parameter value
  if(cur_P>0 & cur_P>(0.2*S_AMC)){
     Qr=(cur_P-0.2*S_AMC)^2/(cur_P+0.8*S_AMC)
  } else {Qr=0}
  
  return(Qr)
  }