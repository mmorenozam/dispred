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
    

df_prep <- function(df){
  dff <- df
  dff <- subset(dff,window==7&lag==0)
  qt <- as.numeric(quantile(rot$cases,probs=c(0.5,0.75,0.9)))
  outb <- ifelse(dff$case<qt[1],'NO',
                 ifelse(dff$cases>=qt[1]&dff$cases<qt[2],'Small',
                        ifelse(dff$cases>=qt[2]&dff$cases<qt[3],'Medium','Large')))
  df_out <- data.frame(c(outb))
  df <- df[,c(7,24,6,8,9,10,11,12,13,14)]
  i_con <- c(0:3)
  j_con <- c(7,14,21,28)
  for (i in 1:length(i_con)){
    for (j in 1:length(j_con)){
      df_s <-subset(df,lag==i_con[i]&window==j_con[j])
      df_s$window<-NULL
      df_s$lag <- NULL
      colnames(df_s) <- paste0(colnames(df_s),'_',i_con[i],'_',j_con[j])
      df_out <- cbind(df_out,df_s)
    }
  }
  return(df_out)
}

rot_pca <- df_prep(rot)

var_lab <- rot_pca$c.outb.
rot_pca$c.outb. <- NULL

my.pca <- prcomp(rot_pca,retx = T,center = T,scale=T)
pca.ind <- get_pca_ind(my.pca)
df.sc <- data.frame(cbind(pca.ind$coord[,c(1,2)]))
md <-  pairwise.mahalanobis(df.sc[,1:2],grouping=var_lab,cov=cov(df.sc[,1:2]))
mdd <- md$distance
fmd <- mdd[upper.tri(mdd,diag=F)]


eig.val <- get_eigenvalue(my.pca)
PC1.expl <- round(eig.val[1,2],2)
PC2.expl <- round(eig.val[2,2],2)
res.ind <- get_pca_ind(my.pca)
PC1.ind <- res.ind$coord[,1]
PC2.ind <- res.ind$coord[,2]
res.var <- get_pca_var(my.pca)
PC1.var <- res.var$coord[,1]
PC2.var <- res.var$coord[,2]
labs.var <- rownames(res.var$coord)

dfp <- data.frame(PC1.ind,PC2.ind,var_lab)
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