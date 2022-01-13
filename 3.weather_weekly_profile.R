packages <- c("tidyverse","cowplot")

lapply(packages,require,character.only=T)

wd <- getwd()
out <- paste0(wd,'/figures/')
w_data <- paste0(wd,'/working_data/')
r_data <- paste0(wd,'/raw_data/')

dp_func <- function(df){
  bdat <- df
  bdat$week <- substr(bdat$date,11,12)
  bdat$week <- as.numeric(bdat$week)
  bdat$date <- seq(as.Date("2001-01-07"), as.Date("2022-01-03"), by="7 days")
  bdat[is.na(bdat)]<-0
  bdat <- subset(bdat,date<as.Date('2017-01-01')) #imp, esta fecha es para que el resto sea para predicciones
  return(bdat)
  
}

dates <- read.csv(paste0(r_data,"t_cas_rot.csv"))
dates <- dp_func(dates)
wdata <- read.csv(paste0(w_data,"wweather.csv"))
wdata$MESS_DATUM <- as.Date(wdata$MESS_DATUM)+49
wdata <- left_join(wdata,dates,by=c("MESS_DATUM"="date"))
wdata$year <- as.numeric(as.character(strftime(wdata$MESS_DATUM,format= "%Y")))

head(wdata)

ggplot(wdata,aes(x=week,y=TMK,group=year))+
  geom_line(alpha=0.3)+
  geom_line(aes(x=week,y=cases/10,group=year),color='red')
