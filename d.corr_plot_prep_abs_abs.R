library(tidyverse)

w_dic <- getwd()
w_data <- paste0(w_dic,'/working_data/')

cam <- read.csv(paste0(w_data,"sw_cam.csv"))
bor <- read.csv(paste0(w_data,"sw_bor.csv"))
inf <- read.csv(paste0(w_data,"sw_inf.csv"))
rot <- read.csv(paste0(w_data,"sw_rot.csv"))

ddi <- function(df){
  df$hdd <- ifelse(df$TMK <= 10,19-df$TMK,0)
  df$cdd <- ifelse(df$TMK >= 22,df$TMK-19,0)
  return(df)
}

cam <- ddi(cam)
bor <- ddi(bor)
inf <- ddi(inf)
rot <- ddi(rot)
# 
# jums<-subset(rot,window==7)
# 
# ggplot(subset(rot,window==7),aes(x=as.Date(date),y=TMK))+geom_line()+facet_wrap(~lag)

# ggplot(cam,aes(x=as.Date(date),y=TMK,colour=as.factor(window)))+geom_line()+facet_wrap(~lag)


cof <- function(df,depend,prx){
  dff <- df
  dff[dff<0.00001] <- 0 # to avoid meaningless correlations
  dff$date <- as.Date(dff$date)
  dff <- subset(dff,date<as.Date('2017-01-01'))
  # dff$d_date <- c(diff(dff$date),500) #this would be necessary if working with differences
  # dff <- subset(dff,d_date==7)
  dff$d_date <- NULL
  wd <- as.numeric(names(table(dff[,'window'])))
  s <- seq(0,20,1) #lag
  b <- c('TMK','FM','RSK','SHK_TAG','PM','UPM','TXK','TNK','hdd','cdd')
  m <- c('spearman')
  df_out <- data.frame()
  for (u in 1:length(wd)){
    for (j in 1:length(s)){
      for (k in 1:length(b)){
        for (l in 1:length(m)){
          df <- subset(dff,window==wd[u])
          ct <- tryCatch(cor.test(df[df$lag==s[j],b[k]],
                                    df[df$lag==s[j],depend],
                                    method=m[l],exact = F),
                           error = function(e) NA)

          rtb <- data.frame(cbind(s[j],b[k],m[l],as.numeric(ct[3]),as.numeric(ct[4])
                                    ,wd[u]))
            #column order: lag, week, weather,method, pvalue,correlation,r.squared
          df_out <- rbind(df_out,rtb)
        }
      }
    }
  }
  colnames(df_out) <- c('lag','wc','method','val','corr','window')
  df_out$lag <- as.numeric(as.character(df_out$lag))
  df_out$val <- as.numeric(as.character(df_out$val))
  df_out$corr <- as.numeric(df_out$corr)
  df_out$method <- as.character(df_out$method)
  df_out$wc <- as.character(df_out$wc)
    df_out <- df_out %>%  mutate(wc = recode(wc, 
                                             `TMK` = 'Temp',
                                             `FM` = 'Wind',
                                             `RSK` = 'Prec',
                                             `SHK_TAG` = 'Snow',
                                             `PM` = 'Press',
                                             `UPM` = 'Humid',
                                             `TXK` = 'Max. Temp',
                                             `TNK` = 'Min. Temp'))
    
    write.csv(df_out,paste0(w_data,prx,"no_weeks.csv"),row.names = F)
    #return(df_out)
}

cof(bor,"cases","sw_corr_bor_")
cof(cam,"cases","sw_corr_cam_")
cof(inf,"cases","sw_corr_inf_")
cof(rot,"cases","sw_corr_rot_")