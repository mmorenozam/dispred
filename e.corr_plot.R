library(tidyverse)
library(cowplot)

wd <- getwd()
out_plots <- paste0(wd,'/figures_sliding/')
setwd(paste0(wd,'/working_data/'))


ppf <- function(trs,tp,cod){
  fils <- list.files(pattern=cod)
  for (i in 1:length(fils)){
    df <- read.csv(fils[i])
    df$wc <- factor(df$wc,
                    levels=c("Temp","Max. Temp","Min. Temp","Prec","Snow","Wind","Humid","Press"),
                    labels=c("Temp","Max. Temp","Min. Temp","Prec","Snow","Wind","Humid","Press"))
    df$val <- log(df$val)
    df$val[df$val>=log(trs)] <- NA
    df <- df %>%
      mutate(corr = ifelse(is.na(val)|is.infinite(val),NA,corr)) %>%
      mutate(rsq = ifelse(is.na(val)|is.infinite(val),NA,rsq)) 
    
    if (tp=='pvals'){
      p1 <- ggplot(subset(df,method=='spearman'),aes(week,as.factor(lag)))+
        geom_raster(aes(fill=val),hjust = 0,vjust=0)+
        scale_fill_gradientn(colours=c("#FF0000FF","#0000FFFF"))+
        facet_wrap(~wc)+
        theme_bw()+
        theme(legend.position = "bottom")
      save_plot(paste0(out_plots,"pvals_",cod,unique(df$window),".pdf"),p1,base_height = 9,base_width = 12)
    } 
    
    if (tp=='rsq'){
      p1 <- ggplot(subset(df,method=='spearman'),aes(week,as.factor(lag)))+
        geom_raster(aes(fill=val),hjust = 0,vjust=0)+
        scale_fill_gradientn(colours=c("#0000FFFF","#FF0000FF"))+
        facet_wrap(~wc)+
        theme_bw()+
        theme(legend.position = "bottom")
      save_plot(paste0(out_plots,"rsqd_",cod,unique(df$window),".pdf"),p1,base_height = 9,base_width = 12)
    }  
    
    if (tp=='aic'){
      p1 <- ggplot(subset(df,method=='spearman'),aes(week,as.factor(lag)))+
        geom_raster(aes(fill=val),hjust = 0,vjust=0)+
        scale_fill_gradientn(colours=c("#0000FFFF","white","#FF0000FF"))+
        facet_wrap(~wc)+
        theme_bw()+
        theme(legend.position = "bottom")
      save_plot(paste0(out_plots,"aic_",cod,unique(df$window),".pdf"),p1,base_height = 9,base_width = 12)
    } 
    if (tp=='jums'){
      p1 <- ggplot(subset(df,method=='spearman'),aes(week,as.factor(lag)))+
        geom_raster(aes(fill=val),hjust = 0,vjust=0)+
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