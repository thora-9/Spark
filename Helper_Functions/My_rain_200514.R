#Creating a Poisson Distribution

#State the number of days of data required
# days=365*10
# 
# freq_rainfall=rpois(days,1/0.6)
# 
# n=10
# alpha=10
# lambda=0.3

myrain <- function(n,alpha,lambda){
  days=365*n
  freq_rainfall=rpois(days,1/lambda)
  freq_rainfall[1]=0
  out_rain=alpha
  i=2
  for (i in 1:days){
    tau1=freq_rainfall[i]
    if (tau1==0){
      cur_rain=rexp(1,1/10)
    } else {
      no_rain_days=rep(0,tau1)
      next_rain_day=rexp(1,1/10)
      cur_rain=c(no_rain_days,next_rain_day)
      i=i+tau1+1
      }
    out_rain=c(out_rain,cur_rain)
  }
  return(out_rain)
}