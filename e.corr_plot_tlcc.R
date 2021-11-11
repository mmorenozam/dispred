library(tidyverse)
library(cowplot)
library(viridis)

wd <- getwd()
out_plots <- paste0(wd,'/figures_sliding/')
out_plots_2 <- paste0(wd,'/figures_boxes/')
w_data <- paste0(wd,'/working_data/')

rot <- read.csv(paste0(w_data,'sw_corr_inf_tlcc.csv'))
rot1 <- rot
rot1$val[rot1$val>0.05]<-NA
head(rot)

ggplot(rot1,aes(x=lag,y=window))+
  geom_raster(aes(fill=log(as.numeric(val))))+
  scale_fill_viridis_c(option="C")+
  facet_wrap(~wc)+
  theme_bw()+
  theme(legend.position="bottom")
