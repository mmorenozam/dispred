library(tidyverse)

wd <- getwd()
w_data <- paste0(wd,'/working_data/')

cam <- read.csv(paste0(w_data,"ab_cam.csv"))
bor <- read.csv(paste0(w_data,"ab_bor.csv"))
inf <- read.csv(paste0(w_data,"ab_inf.csv"))
rot <- read.csv(paste0(w_data,"ab_rot.csv"))

cof <- function(df,depend){
  w <- seq(1,53,1) #week
  s <- seq(0,51,1) #lag
  b <- c('TMK','FM','RSK','SHK_TAG','PM','UPM','TXK','TNK')
  m <- c('spearman')
  df_out <- data.frame()
  df$d_diff <- c(diff(as.Date(df$date)),7)
  df <- subset(df,d_diff==7)
  for (i in 1:length(w)){
    for (j in 1:length(s)){
      for (k in 1:length(b)){
        for (l in 1:length(m)){
          ct <- tryCatch(cor.test(df[df$lag==s[j]&df$week==w[i],b[k]],
                                  df[df$lag==s[j]&df$week==w[i],depend],
                                  method=m[l],exact = F),
                         error = function(e) NA)
          lr <- tryCatch(lm(df[df$lag==s[j]&df$week==w[i],depend]~df[df$lag==s[j]&df$week==w[i],b[k]]),
                         error = function(e) NA)
          rtb <- data.frame(cbind(s[j],w[i],b[k],m[l],as.numeric(ct[3]),as.numeric(ct[4]),
                                  summary(lr)$r.squared))
          #column order: lag, week, weather,method, pvalue,correlation,r.squared
          df_out <- rbind(df_out,rtb)
        }
      }
    }
  }
  colnames(df_out) <- c('lag','week','wc','method','val','corr','rsq')
  df_out$lag <- as.numeric(as.character(df_out$lag))
  df_out$val <- as.numeric(as.character(df_out$val))
  df_out$week <- as.numeric(as.character(df_out$week))
  df_out$corr <- as.numeric(df_out$corr)
  df_out$method <- as.character(df_out$method)
  df_out$wc <- as.character(df_out$wc)
  df_out$rsq <- as.numeric(as.character(df_out$rsq))
  df_out <- df_out %>%  mutate(wc = recode(wc, 
                                           `TMK` = 'Temp',
                                           `FM` = 'Wind',
                                           `RSK` = 'Prec',
                                           `SHK_TAG` = 'Snow',
                                           `PM` = 'Press',
                                           `UPM` = 'Humid',
                                           `TXK` = 'Max. Temp',
                                           `TNK` = 'Min. Temp'))
  return(df_out)
}

corr_inf<-cof(inf,"incidence")
corr_cam<-cof(cam,"incidence")
corr_rot<-cof(rot,"incidence")
corr_bor<-cof(bor,"incidence")

write.csv(corr_inf,paste0(w_data,"corr_inf_weekly.csv"),row.names = F)
write.csv(corr_cam,paste0(w_data,"corr_cam_weekly.csv"),row.names = F)
write.csv(corr_rot,paste0(w_data,"corr_rot_weekly.csv"),row.names = F)
write.csv(corr_bor,paste0(w_data,"corr_bor_weekly.csv"),row.names = F)

ggplot(subset(corr_inf,method=='spearman'),aes(week,lag))+
  geom_raster(aes(fill=log(val)))+
  scale_fill_gradientn(colours=c("#0000FFFF","#FF0000FF"))+
  facet_wrap(~wc)
