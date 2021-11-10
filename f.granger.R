library(tidyverse)
library(lmtest)

w_dic <- getwd()
w_data <- paste0(w_dic,"/working_data/")

rot <- read.csv(paste0(w_data,"sw_rot_gran_.csv"))

df_pre1 <- function(df,ord){
  df <- df[,c(7,3,2,6,8,9,10,11,12,13,14)] #selecting columns with weather variables
  df[df<0.00001] <- 0 # to avoid meaningless correlations
  cnams <- colnames(df)[4:ncol(df)] #names of wv
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

jums1 <- df_pre1(df_j,6)

jums_s <- jums1
#jums_s$val[jums_s$val>0.05] <- NA

ggplot(jums_s,aes(as.numeric(window),as.factor(lag)))+
  geom_raster(aes(fill=log(as.numeric(val))),hjust=0,vjust=0)+
  scale_fill_gradientn(colours=c("#FF0000FF","blue"))+
  facet_wrap(~wc)+
  theme_bw()+
  theme(legend.position = "bottom")

ggplot(jums_s)+
  stat_density_2d(aes(x=window,y=lag))+
  facet_wrap(~wc)

ggdiamonds = ggplot(diamonds) +
  stat_density_2d(aes(x = x, y = depth, fill = stat(nlevel)),
                  geom = "polygon", n = 100, bins = 10, contour = TRUE) +
  facet_wrap(clarity~.) +
  scale_fill_viridis_c(option = "A")

############################### lo de abajo no se si endesirva #############################

df_prep <- function(df,ord){
  df <- df[,c(7,3,2,6,8,9,10,11,12,13,14)] #selecting columns with weather variables
  df[df<0.00001] <- 0 # to avoid meaningless correlations
  #cnams <- colnames(df)[4:ncol(df)] #names of wv
  cnams <- c("UPM","TXK","SHK_TAG","FM")
  df_out<-data.frame() #empty data frame
  ww <- unique(df$window) #vector with unique window sizes
  lg <- c(1:ord) #lag
  for(i in 1:52){#counter for the weeks
    for (j in 1:length(cnams)){#counter for the number of wv
      for (k in 1:length(ww)){#counter for the number of window sizes
        for (l in 1:length(lg)){#counter for the number of lags
          s_df <- subset(df,week==i&window==ww[k])
          r_out <- data.frame(cbind(i,
                                    cnams[j],
                                    ww[k],
                                    lg[l],
                                    tryCatch(grangertest(s_df[,"cases"]~s_df[,cnams[j]],order=lg[l])$`Pr(>F)`[2],
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

df_j <- subset(rot,lag==0&week<53)

jums <- df_prep(df_j,6)

colnames(jums)
jums_s <- subset(jums,window==7)
#jums_s$val[jums_s$val>0.05] <- NA

ggplot(jums_s,aes(as.numeric(week),as.factor(lag)))+
  geom_raster(aes(fill=log(as.numeric(val))),hjust=0,vjust=0)+
  scale_fill_gradientn(colours=c("#FF0000FF","#0000FFFF"))+
  facet_wrap(~wc)+
  theme_bw()+
  theme(legend.position = "bottom")


