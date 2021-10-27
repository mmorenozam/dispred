library(tidyverse)

wd <- getwd()
r_data <- paste0(wd,'/raw_data/')

cam <- read.csv(paste0(r_data,'t_cas_camp.csv'))
bor <- read.csv(paste0(r_data,'t_cas_borr.csv'))
inf <- read.csv(paste0(r_data,'t_cas_inf.csv'))
rot <- read.csv(paste0(r_data,'t_cas_rot.csv'))

out_class <- function(df){
  df$week <- substr(df[,1],11,12)
  df$week <- as.numeric(df$week)
  df[,1] <- seq(as.Date("2001-01-07"), as.Date("2022-01-03"), by="7 days")
  df[is.na(df)] <- 0
  df[,1] <- as.Date(df[,1])
  df$year <- strftime(df$date,format='%Y')
  qt <- as.numeric(quantile(df$cases,probs=c(0.50,0.75,0.9)))
  print(qt)
  df$outb <- ifelse(df$case<qt[1],'NO',
                    ifelse(df$cases>=qt[1]&df$cases<qt[2],'Small',
                           ifelse(df$cases>=qt[2]&df$cases<qt[3],'Medium','Large')))
  
  df$outb <- factor(df$outb,levels=c('Large','Medium','Small','NO'))
  return(df)
}

cam_o <- out_class(cam) #54.00 71.25 85.50
bor_o <- out_class(bor) #2.00  7.25 22.00
inf_o <- out_class(inf) #1.00 11.25 95.50
rot_o <- out_class(rot) #16 51 99

ggplot(data=cam_o,aes(x=week,y=cases,group=year))+
  geom_point(aes(colour=outb))+
  geom_line(alpha=0.1)

ggplot(data=bor_o,aes(x=week,y=cases,group=year))+
  geom_point(aes(colour=outb))+
  geom_line(alpha=0.1)

ggplot(data=inf_o,aes(x=week,y=cases,group=year))+
  geom_point(aes(colour=outb))+
  geom_line(alpha=0.1)

ggplot(data=rot_o,aes(x=week,y=cases,group=year))+
  geom_point(aes(colour=outb))+
  geom_line(alpha=0.1)

ggplot(data=cam_o,aes(x=week,y=cases,group=week))+
  geom_boxplot()+
  geom_hline(yintercept=71)

ggplot(data=bor_o,aes(x=week,y=cases,group=week))+
  geom_boxplot()+
  geom_hline(yintercept=7)

ggplot(data=inf_o,aes(x=week,y=cases,group=week))+
  geom_boxplot()+
  geom_hline(yintercept=11)

ggplot(data=rot_o,aes(x=week,y=cases,group=week))+
  geom_boxplot()+
  geom_hline(yintercept=51)
