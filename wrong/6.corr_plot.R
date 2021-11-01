library(tidyverse)
library(cowplot)

wd <- getwd()
out_plots <- paste0(wd,'/figures/')
w_data <- paste0(wd,'/working_data/')

cinf<-read.csv(paste0(w_data,"corr_inf_weekly.csv"))
ccam<-read.csv(paste0(w_data,"corr_cam_weekly.csv"))
crot<-read.csv(paste0(w_data,"corr_rot_weekly.csv"))
cbor<-read.csv(paste0(w_data,"corr_bor_weekly.csv"))


ppf <- function(df,trs,tp,cod,l_val){
  df <- subset(df,lag<=l_val)
  df$wc <- factor(df$wc,
                  levels=c("Temp","Max. Temp","Min. Temp","Prec","Snow","Wind","Humid","Press"),
                  labels=c("Temp","Max. Temp","Min. Temp","Prec","Snow","Wind","Humid","Press"))
  df$val <- log(df$val)
  df$val[df$val>=log(trs)] <- NA
  df <- df %>%
    mutate(corr = ifelse(is.na(val)|is.infinite(val),NA,corr))
  
  if (tp=='pvals'){
    p1 <- ggplot(subset(df,method=='spearman'),aes(week,as.factor(lag)))+
      geom_raster(aes(fill=val),hjust = 0,vjust=0)+
      scale_fill_gradientn(colours=c("#FF0000FF","#0000FFFF"))+
      facet_wrap(~wc)+
      theme_bw()+
      theme(legend.position = "bottom")
    save_plot(paste0(out_plots,"pvals_",cod,".pdf"),p1,base_height = 9,base_width = 12)
  } 
  
  if (tp=='rsq'){
    p1 <- ggplot(subset(df,method=='spearman'),aes(week,as.factor(lag)))+
      geom_raster(aes(fill=rsq),hjust = 0,vjust=0)+
      scale_fill_gradientn(colours=c("#0000FFFF","#FF0000FF"))+
      facet_wrap(~wc)+
      theme_bw()+
      theme(legend.position = "bottom")
    save_plot(paste0(out_plots,"rsqd_",cod,".pdf"),p1,base_height = 9,base_width = 12)
  }  else {
    p1 <- ggplot(subset(df,method=='spearman'),aes(week,as.factor(lag)))+
      geom_raster(aes(fill=corr),hjust = 0,vjust=0)+
      scale_fill_gradientn(colours=c("#0000FFFF","#FF0000FF"))+
      facet_wrap(~wc)+
      theme_bw()+
      theme(legend.position = "bottom")
    save_plot(paste0(out_plots,"corrs_",cod,".pdf"),p1,base_height = 9,base_width = 12)
  }
}

ppf(cinf,0.05,'pvals','infl',10)
ppf(crot,0.05,'pvals','rotv',10)
ppf(cbor,0.05,'pvals','borr',10)
ppf(ccam,0.05,'pvals','camp',10)

ppf(cinf,0.04,'jums','infl',10)
ppf(crot,0.04,'jums','rotv',10)
ppf(cbor,0.04,'jums','borr',10)
ppf(ccam,0.04,'jums','camp',10)

ppf(cinf,0.04,'rsq','infl',10)
ppf(ccam,0.04,'rsq','camp',10)
ppf(crot,0.04,'rsq','rotv',10)
ppf(cbor,0.04,'rsq','borr',10)


