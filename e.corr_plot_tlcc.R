library(tidyverse)
library(cowplot)
library(viridis)

wd <- getwd()
out_plots <- paste0(wd,'/figures/')
w_data <- paste0(wd,'/working_data/')

rot <- read.csv(paste0(w_data,'sw_corr_rot_no_weeks.csv'))
cam <- read.csv(paste0(w_data,'sw_corr_cam_no_weeks.csv'))
inf <- read.csv(paste0(w_data,'sw_corr_inf_no_weeks.csv'))
bor <- read.csv(paste0(w_data,'sw_corr_bor_no_weeks.csv'))

sub_fun <- function(df){
  df <- subset(df,wc!="Max. Temp"&wc!="Min. Temp"&wc!="hdd"&wc!="cdd"&lag<=10)
  return(df)
}

rot <- sub_fun(rot)
cam <- sub_fun(cam)
inf <- sub_fun(inf)
bor <- sub_fun(bor)


plt_fun <- function(df,th,n){
  df$val[df$val>th] <- NA
  p1 <- ggplot(df,aes(x=lag,y=window/7))+
    geom_raster(aes(fill=log(as.numeric(val))))+
    scale_fill_viridis_c(option="C")+
    facet_wrap(~wc)+
    theme_bw()+
    theme(legend.position="bottom")+
    ylab("window (weeks)")+
    xlab("lag (weeks)")+
    guides(fill=guide_legend(title="log(p-value)"))
  
  save_plot(paste0(out_plots,n,"_pval_abs.pdf"),p1,base_height = 9,base_width = 12) 
}

plt_fun(rot,0.005,'rot')
plt_fun(cam,0.005,'cam')
plt_fun(inf,0.005,'inf')
plt_fun(bor,0.005,'bor')

plt_fun_corr <- function(df,th,n){
  df$corr[df$val>th] <- NA
  p1 <- ggplot(df,aes(x=lag,y=window))+
    geom_raster(aes(fill=corr))+
    scale_fill_viridis_c(option="C")+
    facet_wrap(~wc)+
    theme_bw()+
    theme(legend.position="bottom")+
    ylab("window (weeks)")+
    xlab("lag (weeks)")+
    guides(fill=guide_legend(title="correlation"))
  
  save_plot(paste0(out_plots,n,"_corrs_abs.pdf"),p1,base_height = 9,base_width = 12) 
}

plt_fun_corr(rot,0.005,'rot')
plt_fun_corr(cam,0.005,'cam')
plt_fun_corr(inf,0.005,'inf')
plt_fun_corr(bor,0.005,'bor')

###check this paper at home
# Temperature-dependent transmission
# of rotavirus in Great Britain
# and The Netherlands
