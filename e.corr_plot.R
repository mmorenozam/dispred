library(tidyverse)
library(cowplot)
library(viridis)

wd <- getwd()
out_plots <- paste0(wd,'/figures_sliding/')
out_plots_2 <- paste0(wd,'/figures_boxes/')
w_data <- paste0(wd,'/working_data/')

ppf <- function(trs,tp,cod){
  fils <- list.files(path=w_data,pattern=cod)
  for (i in 1:length(fils)){
    df <- read.csv(paste0(w_data,fils[i]))
    df$wc <- factor(df$wc,
                    levels=c("Temp","Max. Temp","Min. Temp","Prec","Snow","Wind","Humid","Press"),
                    labels=c("Temp","Max. Temp","Min. Temp","Prec","Snow","Wind","Humid","Press"))
    df$val[df$val>=trs] <- NA
    df$val <- log(df$val)
    # df$val[df$val>=log(trs)] <- NA
    df <- subset(df,lag<=5)
    df <- df %>%
      mutate(corr = ifelse(is.na(val)|is.infinite(val),NA,corr)) 
    
    if (tp=='pvals'){
      p1 <- ggplot(subset(df,method=='spearman'),aes(week,as.factor(lag)))+
        geom_raster(aes(fill=val),hjust = 0,vjust=0)+
        scale_fill_viridis_c(option="C")+
        facet_wrap(~wc)+
        theme_bw()+
        theme(legend.position = "bottom")
      save_plot(paste0(out_plots,"pvals_",cod,unique(df$window),".pdf"),p1,base_height = 9,base_width = 12)
    } 
    
   

    if (tp=='corr'){
      p1 <- ggplot(subset(df,method=='spearman'),aes(week,as.factor(lag)))+
        geom_raster(aes(fill=corr),hjust = 0,vjust=0)+
        scale_fill_gradientn(colours=c("#0000FFFF","white","#FF0000FF"))+
        facet_wrap(~wc)+
        theme_bw()+
        theme(legend.position = "bottom")
      save_plot(paste0(out_plots,"corrs_",cod,unique(df$window),".pdf"),p1,base_height = 9,base_width = 12)
    }
  }
}

ppf(0.05,'pvals','sw_corr_bor_')
ppf(0.05,'pvals','sw_corr_rot_')
ppf(0.05,'pvals','sw_corr_inf_')
ppf(0.05,'pvals','sw_corr_cam_')

ppf(0.05,'corr','sw_corr_bor_')
ppf(0.05,'corr','sw_corr_rot_')
ppf(0.05,'corr','sw_corr_inf_')
ppf(0.05,'corr','sw_corr_cam_')


ot_plt <- function(dis,cod){
  fils <- list.files(path=w_data,pattern=cod)
  df_m <- data.frame()
  for (i in 1:length(fils)){
    df_m <- rbind(df_m,read.csv(paste0(w_data,fils[i])))
  }
  wc_n <- unique(df_m$wc)
  for (j in 1:length(wc_n)){
    df_p <- subset(df_m,wc==wc_n[j]&lag<=5)
    p1 <- ggplot(df_p,aes(y=log(val),x=as.factor(week),group=as.factor(week)))+
      geom_boxplot(outlier.shape = NA,lwd=0.05)+
      geom_jitter(aes(colour=as.factor(window)),alpha=0.5,width = 0.25, height = 0.25,size=0.75)+
      facet_wrap(~lag,ncol=1)+
      geom_hline(yintercept = log(0.05),colour='red',linetype='dashed')
    save_plot(paste0(out_plots_2,dis,"_boxplt_",wc_n[j],".pdf"),p1,base_height = 14,base_width = 12)
    
    p2<- ggplot(df_p,aes(x=corr,y=log(val)))+
      geom_point(aes(colour=week))+
      facet_wrap(window~lag)+
      scale_color_gradientn(colours = c('#461863','#404E88','#2A8A8C','#7FD157','#F9E53F')
                            ,values = scales::rescale(c(10,20,30,40,50))
                            ,breaks = c(10,20,30,40,50))+
      guides(fill = guide_legend(reverse = T))+
      geom_hline(yintercept = log(0.05),colour='red',linetype='dashed')
    save_plot(paste0(out_plots_2,dis,"_vul_",wc_n[j],".pdf"),p2,base_height = 12,base_width = 12)
  }
}

ot_plt('rot', 'sw_corr_rot_')
ot_plt('bor', 'sw_corr_bor_')
ot_plt('cam', 'sw_corr_cam_')
ot_plt('inf', 'sw_corr_inf_')

############### analysis of the weather windows and lags

head(bor)

p_abs <- ggplot(bor,aes(x=window,y=lag))+
  geom_raster(aes(fill=TMK))+
  scale_fill_viridis_c(option="C")+
  theme_bw()+
  theme(legend.position = "bottom")

  
p_dif <- ggplot(bor,aes(x=window,y=lag))+
  geom_raster(aes(fill=d_TMK))+
  scale_fill_viridis_c(option="C")+
  theme_bw()+
  theme(legend.position = "bottom")

pg<-plot_grid(p_abs,p_dif,labels=c('A',"B"))

save_plot(paste0(out_plots,"lag_vs_abs.pdf"),pg,base_height = 5,base_width = 10)
