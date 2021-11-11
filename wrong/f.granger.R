library(tidyverse)
library(lmtest)

w_dic <- getwd()
w_data <- paste0(w_dic,"/working_data/")

rot <- read.csv(paste0(w_data,"sw_rot_gran_.csv"))

df_pre1 <- function(df,ord){
  df$date <- as.Date(df$date)
  df <- subset(df,date<as.Date('2017-01-01'))
  df$d_date <- c(diff(df$date),500) #this would be necessary if working with differences
  df <- subset(df,d_date==7)
  df$d_date <- NULL
  #df <- df[,c(7,3,2,6,8,9,10,11,12,13,14)] #selecting columns with weather variables
  df <- df[,c(7,3,2,4,15,16,17,18,19,20,21,22)]
  df[df<0.00001] <- 0 # to avoid meaningless correlations
  cnams <- colnames(df)[5:ncol(df)] #names of wv
  df_out<-data.frame() #empty data frame
  ww <- unique(df$window) #vector with unique window sizes
  lg <- c(1:ord) #lag
  for(i in 1:length(ww)){#counter for the window sizes
    for (j in 1:length(cnams)){#counter for the number of wv
      for (k in 1:length(lg)){#counter for the number of lags
          s_df <- subset(df,window==ww[i])
          r_out <- data.frame(cbind(ww[i],
                                    cnams[j],
                                    lg[k],
                                    tryCatch(grangertest(s_df[,"cases"]~s_df[,cnams[j]],order=lg[k])$`Pr(>F)`[2],
                                             error = function(e) NA)))
          df_out <- rbind(df_out,r_out)
          # print(paste0("week=",i," window=",ww[k]," pval=",grangertest(s_df[,"cases"]~s_df[,cnams[j]],order=lg[l])$`Pr(>F)`[2]))
      }
    }
  }
  
  
  colnames(df_out) <- c("window","wc","lag","val")
  df_out$lag <- as.numeric(as.character(df_out$lag))
  df_out$val <- as.numeric(as.character(df_out$val))
  df_out$week <- as.numeric(as.character(df_out$window))
  df_out$wc <- as.character(df_out$wc)
  df_out <- df_out %>%  mutate(wc = recode(wc, 
                                           `d_TMK` = 'Temp',
                                           `d_FM` = 'Wind',
                                           `d_RSK` = 'Prec',
                                           `d_SHK_TAG` = 'Snow',
                                           `d_PM` = 'Press',
                                           `d_UPM` = 'Humid',
                                           `d_TXK` = 'Max. Temp',
                                           `d_TNK` = 'Min. Temp'))
  return(df_out)
}

df_j <- subset(rot,lag==0&week<53)

mdf <- df_pre1(df_j,20)

write.csv(mdf,file=paste0(w_data,'rot_granger_plot.csv'),row.names = F)

######## confirmation for granger causality

rotc <- read.csv(paste0(w_data,'sw_rot.csv'))

df_pre_cn <- function(df){
  df$date <- as.Date(df$date)
  df <- subset(df,date<as.Date('2017-01-01'))
  df$d_date <- c(diff(df$date),500) #this would be necessary if working with differences
  df <- subset(df,d_date==7&lag!=0)
  df$d_date <- NULL
  df <- df[,c(7,3,2,4,24,6,8,9,10,11,12,13,14)] #selecting columns with weather variables
  #df <- df[,c(7,3,2,4,24,15,16,17,18,19,20,21,22)] #for the differenceseses
  df[df<0.00001] <- 0 # to avoid meaningless correlations
  cnams <- colnames(df)[6:ncol(df)] #names of wv
  df_out<-data.frame() #empty data frame
  ww <- unique(df$window) #vector with unique window sizes
  lg <- c(1:max(df$lag)) #lag
  for(i in 1:length(ww)){#counter for the window sizes
    for (j in 1:length(cnams)){#counter for the number of wv
      for (k in 1:length(lg)){#counter for the number of lags
        s_df <- subset(df,window==ww[i]&lag==lg[k])
        r_out <- data.frame(cbind(ww[i],
                                  cnams[j],
                                  lg[k],
                                  tryCatch(grangertest(s_df[,"cases"]~s_df[,cnams[j]],order=1)$`Pr(>F)`[2],
                                           error = function(e) NA)))
        df_out <- rbind(df_out,r_out)
        # print(paste0("week=",i," window=",ww[k]," pval=",grangertest(s_df[,"cases"]~s_df[,cnams[j]],order=lg[l])$`Pr(>F)`[2]))
      }
    }
  }
  
  
  colnames(df_out) <- c("window","wc","lag","val")
  df_out$lag <- as.numeric(as.character(df_out$lag))
  df_out$val <- as.numeric(as.character(df_out$val))
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
  return(df_out)
}

rotc[,c(7,3,2,4,24,6,8,9,10,11,12,13,14)]

gran_con <- df_pre_cn(rotc)

write.csv(gran_con,file=paste0(w_data,'rot_granger_plot_conf_abs.csv'),row.names = F)
