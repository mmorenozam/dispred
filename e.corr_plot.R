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
    df$val <- log(df$val)
    df$val[df$val>=log(trs)] <- NA
    df <- subset(df,lag<=3)
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


ot_plt <- function(cod){
  fils <- list.files(path=w_data,pattern=cod)
  df_m <- data.frame()
  for (i in 1:length(fils)){
    df_m <- rbind(df_m,read.csv(paste0(w_data,fils[i])))
  }
  wc_n <- unique(df_m$wc)
  p1 <- list()
  for (j in 1:length(wc)){
    df_p <- subset(df_m,wc==wc_n[i]&lag<=5)
    p1[[i]] <- ggplot(df_p,aes(y=log(val),x=as.factor(week),group=as.factor(week)))+
      geom_boxplot(outlier.shape = NA)+
      geom_jitter(aes(colour=as.factor(window)),alpha=0.5,width = 0.25, height = 0.25)+
      facet_wrap(~lag,ncol=1)+
      geom_hline(yintercept = log(0.05),colour='red',linetype='dashed')
    #save_plot(paste0(out_plots_2,"boxplt_",wc_n[i],".pdf"),p1[[i]],base_height = 9,base_width = 12)
  }
  return(wc_n)
}

jums <- ot_plt('sw_corr_rot_')



jums_mom <- data.frame(rbind(read.csv(paste0(w_data,'sw_corr_rot_7window.csv')),
                         read.csv(paste0(w_data,'sw_corr_rot_14window.csv')),
                         read.csv(paste0(w_data,'sw_corr_rot_21window.csv')),
                         read.csv(paste0(w_data,'sw_corr_rot_28window.csv'))))

jums <- subset(jums_mom,wc=='Snow'&lag<=5)
# jums$window1 <- ifelse(jums$window==7,1,
#                        ifelse(jums$window==14,2,
#                               ifelse(jums$window==21,3,4)))

# jums <- jums[complete.cases(jums),]

# ggplot(jums,aes(x=corr,y=log(val)))+
#   geom_point(aes(colour=as.factor(lag)),alpha=0.3)+
#   geom_hline(yintercept = log(0.05),colour='red',linetype='dashed')+
#   facet_wrap(~week)
ggplot(jums,aes(y=log(val),x=as.factor(week),group=as.factor(week)))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(colour=as.factor(window)),alpha=0.5,width = 0.25, height = 0.25)+
  facet_wrap(~lag,ncol=1)+
  geom_hline(yintercept = log(0.05),colour='red',linetype='dashed')


ggplot(jums,aes(x=corr,y=log(val)))+
  geom_point(aes(colour=week))+
  facet_wrap(window~lag)+
  scale_color_gradientn(colours = c('#461863','#404E88','#2A8A8C','#7FD157','#F9E53F')
                      ,values = scales::rescale(c(10,20,30,40,50))
                      ,breaks = c(10,20,30,40,50))+
  guides(fill = guide_legend(reverse = T))+
  geom_hline(yintercept = log(0.05),colour='red',linetype='dashed')



ggplot(jums,aes(y=corr,x=as.factor(week),group=week))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(colour=as.factor(window)),alpha=0.5,width = 0.25, height = 0.25)+
  facet_wrap(~lag,ncol=1)+
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_hline(yintercept =0,colour='red',linetype='dashed')

ggplot(jums,aes(x=corr,y=log(val)))+
  geom_point(aes(colour=as.factor(lag)),alpha=0.3)+
  geom_hline(yintercept = log(0.05),colour='red',linetype='dashed')+
  scale_fill_viridis(discrete = T, alpha=0.6)+
  facet_wrap(~week)

ggplot(jums,aes(x=as.factor(week),y=corr))+
  geom_point()

ggplot(jums,aes(x=as.factor(week),y=log(val)))+
  geom_point()
