wd <- getwd()
w_data <- paste0(wd,'/working_data/')

jums <- read.csv(paste0(w_data,'sw_corr_bor_14window.csv'))
jums <- subset(jums,method=='spearman'&wc=='Snow')
jums <- jums[,-c(3,4,6,7,8)]
head(jums)
jums$week <- paste0('W',jums$week)
#jums <- subset(jums,val<0.05)
#jums$val <- log(jums$val)

jums <- reshape(jums,idvar="lag",timevar = 'week',direction = 'wide')

colnames(jums) <- sub('val.',"",colnames(jums))

rownames(jums) <- jums[,1]
jums$lag <- NULL
jums <- as.matrix(jums)
jums[sapply(jums,is.infinite)] <- NA
jums[sapply(jums,is.na)] <- 0
heatmap(jums)

library("pheatmap")
pheatmap(t(jums))
