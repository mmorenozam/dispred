packages <- c("rdwd","tidyverse","ggcorrplot","cowplot")

lapply(packages,require,character.only=T)

wd <- getwd()

data(metaIndex)
out <- paste0(wd,'/figures/')
rd_out <- paste0(wd,'/raw_data/')
############################ weather stations coordinates ############
res <- "daily"
var <- "kl"
per <- "historical"
df.station <- metaIndex[
  metaIndex$res==res &
    metaIndex$per==per &
    metaIndex$var==var &
    metaIndex$hasfile==TRUE,
]

#sample.station <- df.station[df.station$Bundesland=='Berlin',]
sample.station <- df.station[df.station$geoBreite>=52.35 &
                               df.station$geoBreite<=52.74 &
                               df.station$geoLaenge>=13&
                               df.station$geoLaenge<=13.8,]
sample.station <- subset(sample.station,bis_datum>as.Date('1998-12-31'))


link <- selectDWD(id = sample.station$Stations_id,
                  res = sample.station$res,
                  var = sample.station$var,
                  per = sample.station$per)

file <- c()
for (i in 1:length(link)){
  file[i] <- dataDWD(link[[i]], read = F, quiet = TRUE, force = F)
}

file
length(link)

clim <- data.frame()
for (i in 1:length(link)){
  df <- readDWD(file[i])
  df <-subset(df,MESS_DATUM>=as.Date('1998-01-01'))
  clim <- rbind(clim,df)
}

########################### correlation analyses #####################

canal <-clim %>% group_by(MESS_DATUM) %>%
  summarise_at(c("FX","FM","RSK","SHK_TAG","NM","VPM","PM","TMK","SDK",
                 "UPM","TXK","TNK","TGK"), mean, na.rm = TRUE)


cfun <- function(df,ttl){
  #df <- subset(df,STATIONS_ID==ttl)
  corm <- round(cor(df[ , sort(c("FX","FM","RSK","SHK_TAG","NM","VPM","PM","TMK","SDK",
                                 "UPM","TXK","TNK","TGK"))],
                    method = "pearson", use = "pairwise.complete.obs"), 2)
  plt <- ggcorrplot::ggcorrplot(corm,lab=T,title=ttl)
  return(plt)
}


cpl<-cfun(canal,'Correlation averaged time series')

save_plot(paste0(out,'1.corrs.pdf'),cpl,base_height = 7.5,base_width = 7.5)

############################# do the average of the time series #####################


sm_clim<-clim %>% group_by(MESS_DATUM) %>%
  summarise_at(c("FM","RSK","SHK_TAG","PM","TMK","TXK","TNK",
                 "UPM"), mean, na.rm = TRUE) %>% 
  gather("varb","value",-MESS_DATUM)

sd_clim <- clim%>% group_by(MESS_DATUM) %>%
  summarise_at(c("FM","RSK","SHK_TAG","PM","TMK","TXK","TNK",
                 "UPM"), sd, na.rm = TRUE) %>%
  gather("varb","sd",-MESS_DATUM)

sm_clim <- left_join(sm_clim,sd_clim,by=c("MESS_DATUM"="MESS_DATUM","varb"="varb"))

prof<- ggplot(subset(sm_clim,MESS_DATUM>=as.Date('2015-01-01')),aes(x=MESS_DATUM,y=value,group=varb))+
  geom_ribbon(aes(ymin=value-sd,ymax=value+sd), fill = "pink")+
  geom_point(size=0.001,alpha=0.5)+
  facet_wrap(~varb,ncol=2,scales='free_y')

save_plot(paste0(out,'2.profiles_peep2.pdf'),prof,base_height = 12, base_width = 12)




######################### csv for python resampling ###############

out.csv <- clim %>% group_by(MESS_DATUM) %>%
  summarise_at(c("FM","RSK","SHK_TAG","PM","TMK","TXK","TNK",
                 "UPM"), mean, na.rm = TRUE)

write.csv(out.csv,paste0(rd_out,'weat4python_2.csv'),row.names = F)

=======
require(rdwd)

data(metaIndex)
>>>>>>> 98616a8498011ddf189e4b1467316d1459fd38f9
