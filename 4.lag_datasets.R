library(dplyr)

wd <- getwd()
r_data <- paste0(wd,'/raw_data/')
w_data <- paste0(wd,'/working_data/')

lagf <- function(wdb,dis,pre,max_date){
  wtv <- read.csv(wdb)
  wtv[,1] <- as.Date(wtv[,1])
  cas <- read.csv(dis)
  cas$week <- substr(cas[,1],11,12)
  cas$week <- as.numeric(cas$week)
  cas[,1] <- seq(as.Date("2001-01-07"), as.Date("2022-01-03"), by="7 days")
  cas[is.na(cas)] <- 0
  cas[,1] <- as.Date(cas[,1])
  cas <- subset(cas,cas[,1]<as.Date('2017-01-01'))
  wnum <- seq(0,357,7)
  pl <- list()
  for (i in 1:length(wnum)){
    tdf <- wtv
    tdf$year <- as.numeric(as.character(strftime(tdf$MESS_DATUM,format= "%Y")))
    tdf[,1] <- tdf[,1] + wnum[i]
    tdf$lag <- i-1
    tdf <- left_join(cas,tdf,by=c('date'='MESS_DATUM'))
    pl[[i+1]]<-tdf
  }
  pl <- bind_rows(pl)
  write.csv(pl,file=paste0(pre,'.csv'),row.names = F)
  #return(pl)
}

lagf(paste0(r_data,'weat4python_2.csv'),paste0(r_data,"t_cas_rot.csv"), 
     paste0(w_data,'ab_rot'))
lagf(paste0(r_data,'weat4python_2.csv'),paste0(r_data,"t_cas_inf.csv"), 
     paste0(w_data,'ab_inf'))
lagf(paste0(r_data,'weat4python_2.csv'),paste0(r_data,"t_cas_camp.csv"),
     paste0(w_data,'ab_cam'))
lagf(paste0(r_data,'weat4python_2.csv'),paste0(r_data,"t_cas_borr.csv"),
     paste0(w_data,'ab_bor'))
