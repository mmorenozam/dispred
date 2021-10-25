wd <- getwd()

dis_dat_dic <- paste0(wd,'/raw_data/')
out_dat_dic <- paste0(wd,'/working_data/')


library(tidyverse)
library(stringr)


dp_func <- function(df){
  bdat <- read.csv(df)
  bdat$week <- substr(bdat$date,11,12)
  bdat$week <- as.numeric(bdat$week)
  bdat$date <- seq(as.Date("2001-01-07"), as.Date("2022-01-03"), by="7 days")
  bdat[is.na(bdat)]<-0
  bdat <- subset(bdat,date<as.Date('2017-01-01')) #esta fecha es para que el resto sea para predicciones
  return(bdat)
  
}

rd_fun <- function(dsnm){
  df <- read.csv(dsnm,sep=',')
  df[,2] <- NULL
  df[,1] <- as.Date(df[,1])
  return(df)
}


temp_r <- rd_fun(paste0(dis_dat_dic,'berlin_temp61_mom.csv'))
wind_r <- rd_fun(paste0(dis_dat_dic,'berlin_wind61_mom.csv'))
prec_r <- rd_fun(paste0(dis_dat_dic,'berlin_prec61_mom.csv'))
snow_r <- rd_fun(paste0(dis_dat_dic,'berlin_snow61_mom.csv'))
pres_r <- rd_fun(paste0(dis_dat_dic,'berlin_pres61_mom.csv'))
humi_r <- rd_fun(paste0(dis_dat_dic,'berlin_humi61_mom.csv'))
temx_r <- rd_fun(paste0(dis_dat_dic,'berlin_temx61_mom.csv'))
temm_r <- rd_fun(paste0(dis_dat_dic,'berlin_temm61_mom.csv'))

wea <- left_join(temp_r,wind_r,by=c('MESS_DATUM'='MESS_DATUM','window'='window')) %>%
  left_join(.,prec_r,by=c('MESS_DATUM'='MESS_DATUM','window'='window')) %>%
  left_join(.,temx_r,by=c('MESS_DATUM'='MESS_DATUM','window'='window')) %>%
  left_join(.,temm_r,by=c('MESS_DATUM'='MESS_DATUM','window'='window')) %>%
  left_join(.,snow_r,by=c('MESS_DATUM'='MESS_DATUM','window'='window')) %>%
  left_join(.,pres_r,by=c('MESS_DATUM'='MESS_DATUM','window'='window')) %>%
  left_join(.,humi_r,by=c('MESS_DATUM'='MESS_DATUM','window'='window'))

write.table(wea,paste0(out_dat_dic,'weas3.csv'),row.names = F,sep=';')
