library(dplyr)

wd <- getwd()

r_data <- paste0(wd,'/raw_data/')
w_data <- paste0(wd,'/working_data/')

lagf <- function(wdb,dis,w){
  wtv <- read.csv(wdb,sep=';')
  wtv[,1] <- as.Date(wtv[,1])
  wtv <- subset(wtv,window==w)
  shift <- data.frame(cbind(diff(wtv$TMK),
                            diff(wtv$FM),
                            diff(wtv$RSK),
                            diff(wtv$TXK),
                            diff(wtv$TNK),
                            diff(wtv$SHK_TAG),
                            diff(wtv$PM),
                            diff(wtv$UPM)))
  shift <- data.frame(rbind(shift,0))
  colnames(shift) <- c('d_TMK','d_FM','d_RSK','d_TXK','d_TNK','d_SHK_TAG','d_PM','d_UPM')
  wtv <- cbind(wtv,shift)
  cas <- read.csv(dis)
  cas$week <- substr(cas[,1],11,12)
  cas$week <- as.numeric(cas$week)
  cas[,1] <- seq(as.Date("2001-01-07"), as.Date("2022-01-03"), by="7 days")
  cas[is.na(cas)] <- 0
  cas[,1] <- as.Date(cas[,1])
  cas <- subset(cas,cas[,1]<as.Date('2017-01-01'))
  d_cases <- as.data.frame(diff(cas$cases))
  colnames(d_cases) <- 'd_cases'
  cas <- cbind(cas,rbind(d_cases,0))
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
  return(pl)
}

out_fun <- function(wd,dd,prx,wv){
  out_df <- data.frame()
  for (k in 1:length(wv)){
    dd_out <- lagf(paste0(w_data,wd),paste0(r_data,dd),wv[k])
    out_df <- rbind(out_df,dd_out)
  }
  write.csv(out_df,file=paste0(w_data,prx,'.csv'),row.names = F)
  #return(out_df)
}

w_size <- c(7,14,21,28,35)

out_fun('weas3.csv',"t_cas_rot.csv",'sw_rot',w_size)
out_fun('weas3.csv',"t_cas_inf.csv",'sw_inf',w_size)
out_fun('weas3.csv',"t_cas_camp.csv",'sw_cam',w_size)
out_fun('weas3.csv',"t_cas_borr.csv",'sw_bor',w_size)

