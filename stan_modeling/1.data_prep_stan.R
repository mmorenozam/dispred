library(tidyverse)

wd <- getwd()
w_data <- paste0(wd,"/working_data/")
w_data2 <- paste0(wd,"/stan_modeling/stan_data/")
r_data <- paste0(wd,"/raw_data/")

df_gf <- function(fwea,fdis,vname){
  w_size <- c(7,14,21,28,35)
  weather <- read.csv(paste0(w_data,fwea))
  weather$date <- as.Date(weather$date)
  weather$cal_week <- as.numeric(strftime(weather$date,format="%V"))
  weather$year <- as.numeric(strftime(weather$date,format="%G")) -2000
  weather <- weather[,c(1,3,6,22,23,24,5,7,8,9,10,11,12,13)]
  dis <- read.csv(paste0(r_data,fdis))
  dis[is.na(dis)]<-0
  dis$week <- as.numeric(substr(dis$date,11,12))
  dis$date <- seq(as.Date("2001-01-07"),as.Date("2022-01-03"),by=7)
  dis_t <- subset(dis,date<as.Date("2017-01-01"))
  dis_tes <- subset(dis,date>=as.Date("2017-01-01")&date<as.Date("2020-01-01"))
  for (i in 1:length(w_size)){
    for (j in 1:4){
      sub_dat <- weather
      sub_dat <- subset(sub_dat,window==w_size[i]&lag==j-1)
      train <- left_join(dis_t,sub_dat,by=c('date'='date','week'='week'))
      write.csv(file=paste0(paste0(w_data2,'/',vname,'/'),vname,'_training_w',w_size[i],'_lag_',j-1,'.csv'),train,row.names=F)
      testing <- left_join(dis_tes,sub_dat,by=c('date'='date','week'='week'))
      write.csv(file=paste0(paste0(w_data2,'/',vname,'/'),vname,'_testing_w',w_size[i],'_lag_',j-1,'.csv'),testing,row.names=F)
    }
  }
}

df_gf('sw_rot.csv',"t_cas_rot.csv","rot")
df_gf('sw_inf.csv',"t_cas_inf.csv","inf")
df_gf('sw_bor.csv',"t_cas_borr.csv","bor")
df_gf('sw_cam.csv',"t_cas_camp.csv","cam")
