library(tidyverse)
library(factoextra)

w_dic <- getwd()
w_data <- paste0(w_dic,'/working_data/')

rot <- read.csv(paste0(w_data,"sw_rot.csv"))

qt <- as.numeric(quantile(rot$cases,probs=c(0.5,0.75,0.9)))
rot$outb <- ifelse(rot$case<qt[1],'NO',
                   ifelse(rot$cases>=qt[1]&rot$cases<qt[2],'Small',
                          ifelse(rot$cases>=qt[2]&rot$cases<qt[3],'Medium','Large')))


rpca_dat <- subset(rot,window==21&lag==1)
rpca_out <- rpca_dat$outb
rpca_dat <- rpca_dat[,c(5,7,8,9,10,11,12,13)]
# rpca_dat <- rpca_dat[,c(5,7,8,12,13)]

pca_df <- prcomp(rpca_dat,retx = T,center = T,scale=T)
eig.val <- get_eigenvalue(pca_df)
PC1.expl <- round(eig.val[1,2],2)
PC2.expl <- round(eig.val[2,2],2)
res.ind <- get_pca_ind(pca_df)
PC1.ind <- res.ind$coord[,1]
PC2.ind <- res.ind$coord[,2]
res.var <- get_pca_var(pca_df)
PC1.var <- res.var$coord[,1]
PC2.var <- res.var$coord[,2]
labs.var <- rownames(res.var$coord)

dfp <- data.frame(PC1.ind,PC2.ind,rpca_out)
colnames(dfp)[3] <- 'feat'

ggplot(data=dfp,aes(x=PC1.ind,PC2.ind,colour=feat))+
  geom_point()+
  stat_ellipse(aes(fill=feat),type='norm',level=0.90,geom='polygon',alpha=0.2,linetype='blank')+
  theme_bw() +
  theme(strip.text.x = element_text(hjust = -0.01),
        panel.grid.major = element_line(colour = "grey90",linetype='dashed'),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5),'lines'),
        strip.background = element_blank(),
        strip.text = element_text(size=16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.position = "bottom")+
  xlab(paste('Scores PC1 (',PC1.expl,'%',')', sep ='')) +
  ylab(paste('Scores PC2 (',PC2.expl,'%',')', sep ='')) 
    
