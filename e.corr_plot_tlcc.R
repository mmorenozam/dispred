library(tidyverse)
library(cowplot)
library(viridis)

wd <- getwd()
out_plots <- paste0(wd,'/figures_sliding/')
w_data <- paste0(wd,'/working_data/')

rot <- read.csv(paste0(w_data,'sw_corr_rot_no_weeks.csv'))
rot1 <- rot
rot1$val[rot1$val>0.05]<-NA
head(rot)

p1 <- ggplot(rot1,aes(x=lag,y=window))+
  geom_raster(aes(fill=log(as.numeric(val))))+
  scale_fill_viridis_c(option="C")+
  facet_wrap(~wc)+
  theme_bw()+
  theme(legend.position="bottom")

save_plot(paste0(out_plots,"rot_pval_abs.pdf"),p1,base_height = 9,base_width = 12)

ggplot(rot1,aes(x=lag,y=log(val)))+
  #geom_raster(aes(fill=corr))+
  geom_point(aes(colour=as.factor(window)),alpha=0.3)+
  geom_line(aes(colour=as.factor(window)))+
  scale_colour_viridis_d(option="C")+
  #scale_fill_gradientn(colours=c("#0000FFFF","white","#FF0000FF"))+
  facet_wrap(~wc)+
  theme_bw()+
  theme(legend.position="bottom")

rot2 <- read.csv(paste0(w_data,'rot_granger_plot.csv'))
rot3 <- rot2
rot3$val[rot3$val>0.05]<-NA

p2<-ggplot(rot3,aes(x=lag,y=window))+
  geom_raster(aes(fill=log(as.numeric(val))))+
  scale_fill_viridis_c(option="C")+
  facet_wrap(~wc)+
  theme_bw()+
  theme(legend.position="bottom")
save_plot(paste0(out_plots,"rot_pval_abs_diff_granger.pdf"),p2,base_height = 9,base_width = 12)

rot4 <- read.csv(paste0(w_data,'rot_granger_plot_conf_abs.csv'))
rot5 <- rot4
rot5$val[rot5$val>0.05]<-NA

p3<-ggplot(rot5,aes(x=lag,y=window))+
  geom_raster(aes(fill=log(as.numeric(val))))+
  scale_fill_viridis_c(option="C")+
  facet_wrap(~wc)+
  theme_bw()+
  theme(legend.position="bottom")
save_plot(paste0(out_plots,"rot_pval_abs_diff_granger_conf.pdf"),p3,base_height = 9,base_width = 12)


###check this paper at home
# Temperature-dependent transmission
# of rotavirus in Great Britain
# and The Netherlands
