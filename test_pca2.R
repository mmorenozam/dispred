library(tidyverse)
library(factoextra)
library(HDMD)

w_dic <- getwd()
w_data <- paste0(w_dic,"/working_data/")

rot <- read.csv(paste0(w_data,"sw_rot.csv"))

qt <- as.numeric(quantile(rot$cases,probs=c(0.5,0.75,0.9)))
rot$outb <- ifelse(rot$case<qt[1],'NO',
                   ifelse(rot$cases>=qt[1]&rot$cases<qt[2],'Small',
                          ifelse(rot$cases>=qt[2]&rot$cases<qt[3],'Medium','Large')))


df_prep <- function(df){
  df_b <- subset(df,lag==0&window==7)
  i_con <- c(1:3)
  j_con <- c(14,21,28)
  for (i in 1:length(i_con)){
    for (j in 1:length(j_con)){
      df_s <-subset(df,lag==i_con[i]&window==j_con[j])
      #colnames(df_s) <- paste0(colnames(df_s),'_',i_con[i],'_'j_con[j])
      df_out <- cbind(df_b,df_s)
    }
  }
  return(df_out)
}

jums <- df_prep(rot)

rpca_dat <- subset(rot,window==21&lag==1)
rpca_out <- rpca_dat$outb
rpca_dat1 <- rpca_dat[,c(5,7,8,9,10,11,12,13)]
colnames(rpca_dat1)<-paste0(colnames(rpca_dat1),'_21_1')

jums <- cbind(rpca_dat1,rpca_out)


system.time({my.pca <- prcomp(rpca_dat1,retx = T,center = T,scale=T)
pca.ind <- get_pca_ind(my.pca)
df.sc <- data.frame(cbind(pca.ind$coord[,c(1,2)]))
md <-  pairwise.mahalanobis(df.sc[,1:2],grouping=rpca_dat$outb,cov=cov(df.sc[,1:2]))
mdd <- md$distance})
fmd <- mdd[upper.tri(mdd,diag=F)]
