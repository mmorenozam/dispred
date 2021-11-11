library(dplyr)

wd <- getwd()

r_data <- paste0(wd,'/raw_data/')
w_data <- paste0(wd,'/working_data/')
all_weat <- read.csv(paste0(w_data,'weas3.csv'),sep=';')


lagf <- function(wdb,dis,w){
  wtv <- wdb
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
  #cas <- subset(cas,cas[,1]<as.Date("2021-05-02")) #this filter is for the maximum date of cases in disease data
  cas <- subset(cas,cas[,1]<as.Date("2020-12-31")) #this filter is because weather time series are available only until this day
  cas[is.na(cas)] <- 0
  d_cases <- as.data.frame(diff(cas$cases))
  colnames(d_cases) <- 'd_cases'
  cas <- cbind(cas,rbind(d_cases,0))
  lag_c <- c(0:20)
  l_out <- list()
  wtv$year <- as.numeric(as.character(strftime(wtv$MESS_DATUM,format= "%Y")))
  for (i in 1:length(lag_c)){
    wtv1 <- wtv
    wtv1$new_date <- wtv1[,1] + lag_c[i]*7
    wtv1$lag <- lag_c[i]
    df_out <- left_join(cas,wtv1,by=c('date'='new_date'))
    l_out[[i]] <- df_out
  }
  return(bind_rows(l_out))
  #return(l_out)
}

out_fun <- function(wd,dd,prx,wv){
  out_df <- data.frame()
  for (k in 1:length(wv)){
    dd_out <- lagf(wd,paste0(r_data,dd),wv[k])
    out_df <- rbind(out_df,dd_out)
  }
  write.csv(out_df,file=paste0(w_data,prx,'.csv'),row.names = F)
  #return(out_df)
}

w_size <- c(7:56)

out_fun(all_weat,"t_cas_rot.csv",'sw_rot',w_size)
out_fun(all_weat,"t_cas_inf.csv",'sw_inf',w_size)
out_fun(all_weat,"t_cas_camp.csv",'sw_cam',w_size)
out_fun(all_weat,"t_cas_borr.csv",'sw_bor',w_size)
