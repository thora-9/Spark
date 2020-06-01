# To take current water level, critical depth, RD, psi_s and psi_fc
 #WL = ywt[i]
# CD = -0.45
# RD = 0.4
# psis = -0.12
# sfc2 = 0.44
# b2 = 4.05
hm_zone <- function(WL, CD, RD, psis, sfc2,b2){
  psis_fc = psis*(sfc2)^(-b2)
  A = (psis_fc - psis - CD)/(psis_fc - psis - CD - (5*RD))
  if (WL>CD){
    out_val = 0
  } else if (WL>=((-5*RD)+(-psis+psis_fc))) {
    temp1 = (1-A^(3/4))*(WL - CD)
    temp2 = ((A^2)*(1-(A^(-1/4))))/(-CD+psis_fc-psis)
    temp3 = (WL-CD)^2
    out_val = temp1 - (temp2*temp3)
  } else if (WL<((-5*RD)+(-psis+psis_fc))){
    out_val = WL-psis_fc+psis
  }
  return(out_val)
}
