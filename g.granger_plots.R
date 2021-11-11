library(tidyverse)

w_dic <- getwd()
w_data <- paste0(w_dic,"/working_data/")

dfp <- read.csv(paste0(w_data,'grange_plots_rot.csv'))


dfp_s <- dfp
dfp_s$val[dfp_s$val>0.05] <- NA

ggplot(dfp_s,aes(y=as.numeric(window),x=as.factor(lag)))+
  geom_raster(aes(fill=log(as.numeric(val))))+
  #scale_fill_gradientn(colours=c("#FF0000FF","blue"))+
  scale_fill_viridis_c(option="C")+
  facet_wrap(~wc)+
  theme_bw()+
  theme(legend.position = "bottom")


wrot <- read.csv(paste0(w_data,'grange_plots_rot_week.csv'))

plt_fun <- function(df,ws,trs){
  df <- subset(df,window==ws)
  df$val[df$val>trs] <- NA
  p1 <- ggplot(df,aes(x=week,y=as.factor(lag)))+
    geom_raster(aes(fill=log(as.numeric(val))))+
    scale_fill_viridis_c(option="C")+
    facet_wrap(~wc)+
    theme_bw()+
    theme(legend.position="bottom")
  return(p1)
}

dfp1 <- subset(dfp1,wc!="month")

plt_fun(wrot,7,1)





