library(tidyverse)
library(cowplot)
library(viridis)

wd <- getwd()
out_plots <- paste0(wd,'/figures/')
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


###check this paper at home
# Temperature-dependent transmission
# of rotavirus in Great Britain
# and The Netherlands
