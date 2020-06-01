#Calculate the PET using a sine wave function 


# start_year=1995
# n=10
# min=0.5
# beta=-5


myPET<- function (start_year,n,min,beta){
  require('date')
  require('lubridate')
  
  total_days=n*365
  start_date=as.Date(paste(start_year,"1","1",sep = '/'))
  date_total=seq.Date(start_date,by='day',length.out = total_days)
  jules=julian(date_total)
  out=NULL
  i=1
  for (i in 1:total_days){
    cur_date=date_total[i]
    cur_jules=jules[i]
    cur_PET=abs(min+(beta)*sin(pi*(cur_jules/365)))
    out[i]=cur_PET
  }
  return(out)
}
