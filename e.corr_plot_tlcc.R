library(tidyverse)
library(cowplot)
library(viridis)

wd <- getwd()
out_plots <- paste0(wd,'/figures/')
w_data <- paste0(wd,'/working_data/')



cor_plt_fun <- function(nam){
  df <- read.csv(paste0(w_data,nam))
  p1 <- ggplot(df,aes(x=lag,y=window))+
    geom_raster(aes(fill=log(as.numeric(val))))+
    scale_fill_viridis_c(option="C")+
    facet_wrap(~wc)+
    theme_bw()+
    theme(legend.position = "bottom")
  save_plot(paste0(out_plots,substr(nam,1,nchar(nam)-4),"2.pdf"),p1,base_height = 9,base_width = 12)
}

cor_plt_fun("sw_corr_rot_diffs2.csv")
cor_plt_fun("sw_corr_rot_abs.csv")

p1 <- ggplot(rot1,aes(x=lag,y=window))+
  geom_raster(aes(fill=log(as.numeric(val))))+
  scale_fill_viridis_c(option="C")+
  facet_wrap(~wc)+
  theme_bw()+
  theme(legend.position="bottom")

save_plot(paste0(out_plots,"rot_pval_abs.pdf"),p1,base_height = 9,base_width = 12)


rot <- read.csv(paste0(w_data,'sw_corr_rot_diffs.csv'))
rot1 <- rot
rot1$val[rot1$val>0.05]<-NA
head(rot)
jums <- "rot_pval_abs.pdf"


p1<-ggplot(rot1,aes(x=lag,y=log(val)))+
  #geom_raster(aes(fill=corr))+
  #geom_point(aes(colour=as.factor(window)),alpha=0.3)+
  geom_line(aes(colour=window,group=window))+
  scale_colour_viridis_c(option="C",direction=-1)+
  #scale_fill_gradientn(colours=c("#0000FFFF","white","#FF0000FF"))+
  facet_wrap(~wc)+
  theme_bw()+
  theme(legend.position="bottom")

save_plot(paste0(out_plots,"whats1.pdf"),p1,base_height = 9,base_width = 12)

cor_plt_fun1 <- function(nam){
  df <- read.csv(paste0(w_data,nam))
  df <- subset(df,lag<5&window<=28)
  p1 <- ggplot(df,aes(x=lag,y=window))+
    geom_raster(aes(fill=log(as.numeric(val))))+
    scale_fill_viridis_c(option="C")+
    facet_wrap(~wc)+
    theme_bw()+
    theme(legend.position = "bottom")
  save_plot(paste0(out_plots,substr(nam,1,nchar(nam)-4),"2.pdf"),p1,base_height = 9,base_width = 12)
}

cor_plt_fun1("sw_corr_rot_diffs.csv")
cor_plt_fun1("sw_corr_rot_abs.csv")

rot <- read.csv(paste0(w_data,'sw_corr_rot_diffs.csv'))
rot1 <- rot
rot1 <- subset(rot1,lag<5&window<=28)
head(rot)
jums <- "rot_pval_abs.pdf"


p1<-ggplot(rot1,aes(x=lag,y=log(val)))+
  #geom_raster(aes(fill=corr))+
  #geom_point(aes(colour=as.factor(window)),alpha=0.3)+
  geom_line(aes(colour=window,group=window))+
  scale_colour_viridis_c(option="C",direction=-1)+
  #scale_fill_gradientn(colours=c("#0000FFFF","white","#FF0000FF"))+
  facet_wrap(~wc)+
  theme_bw()+
  theme(legend.position="bottom")

###check this paper at home
# Temperature-dependent transmission
# of rotavirus in Great Britain
# and The Netherlands
