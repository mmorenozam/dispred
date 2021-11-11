library(tidyverse)
library(lmtest)

w_dic <- getwd()
w_data <- paste0(w_dic,"/working_data/")

rot <- read.csv(paste0(w_data,"sw_rot_gran_.csv"))
colnames(rot)
df_pre1 <- function(df,ord){
  #df <- df[,c(7,3,2,6,8,9,10,11,12,13,14)] #selecting columns with weather variables
  df <- df[,c(7,3,2,4,15,16,17,18,19,20,21,22)]
  df[df<0.00001] <- 0 # to avoid meaningless correlations
  cnams <- colnames(df)[5:ncol(df)] #names of wv
  df_out<-data.frame() #empty data frame
  ww <- seq(7,49,2)
  #ww <- unique(df$window) #vector with unique window sizes
  lg <- c(1:ord) #lag
  for(i in 1:length(ww)){#counter for the window sizes
    for (j in 1:length(cnams)){#counter for the number of wv
      for (k in 1:length(lg)){#counter for the number of lags
          s_df <- subset(df,window==ww[i])
          r_out <- data.frame(cbind(ww[i],
                                    cnams[j],
                                    lg[k],
                                    tryCatch(grangertest(s_df[,"d_cases"]~s_df[,cnams[j]],order=lg[k])$`Pr(>F)`[2],
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

df_j <- subset(rot,lag==0&week<53)

mdf <- df_pre1(df_j,52)

write.csv(mdf,file=paste0(w_data,'grange_plots_rot.csv'),row.names = F)


############################### lo de abajo no se si endesirva #############################

# head(rot)
# rot$date <- as.Date(rot$date)
# as.numeric(format(rot$date,"%m"))

df_prep <- function(df,ord){
  df$date <- as.Date(df$date)
  df <- df[,c(7,3,2,4,15,16,17,18,19,20,21,22)]
  df[df<0.00001] <- 0 # to avoid meaningless correlations
  cnams <- colnames(df)[5:ncol(df)] #names of wv
  #cnams <- c("UPM","TXK","SHK_TAG","FM")
  df_out<-data.frame() #empty data frame
  #ww <- unique(df$window) #vector with unique window sizes
  ww <- seq(7,49,7)
  lg <- c(1:ord) #lag
  for(i in 1:53){#counter for the months
    for (j in 1:length(cnams)){#counter for the number of wv
      for (k in 1:length(ww)){#counter for the number of window sizes
        for (l in 1:length(lg)){#counter for the number of lags
          s_df <- subset(df,week==i&window==ww[k])
          r_out <- data.frame(cbind(i,
                                    cnams[j],
                                    ww[k],
                                    lg[l],
                                    tryCatch(grangertest(s_df[,"d_cases"]~s_df[,cnams[j]],order=lg[l])$`Pr(>F)`[2],
                                             error = function(e) NA)))
          df_out <- rbind(df_out,r_out)
          # print(paste0("week=",i," window=",ww[k]," pval=",grangertest(s_df[,"cases"]~s_df[,cnams[j]],order=lg[l])$`Pr(>F)`[2]))
        }
      }
    }
  }
  
  colnames(df_out) <- c("week","wc","window","lag","val")
  df_out$lag <- as.numeric(as.character(df_out$lag))
  df_out$val <- as.numeric(as.character(df_out$val))
  df_out$week <- as.numeric(as.character(df_out$week))
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

df_j <- subset(rot,lag==0&week<=53)


gdm <- df_prep(df_j,6)

write.csv(gdm,file=paste0(w_data,'grange_plots_rot_week.csv'),row.names = F)
