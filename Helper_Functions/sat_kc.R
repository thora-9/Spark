sat_kc <- function (min_k, max_k, slope, GW_Irr){
  # min_k = 3e-6
  # max_k = 3e-5
  # slope = 6e-5
  # GW_Irr = 0.2
  # 
  k1 = slope*GW_Irr+min_k
  if(k1>max_k){
    k1 = max_k
  }
  return(k1) 
}