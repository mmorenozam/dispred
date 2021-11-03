library(tidyverse)
wd <- getwd()
out_data <- paste0(wd,'/stan_modeling/stan_outputs/')
tra_data <- paste0(wd,'/stan_modeling/stan_data/')



load(paste0(out_data,'temp_ari_lag_0_win_7_rot.Rsave'))

test_data <- read.csv(paste0(tra_data,'inf/inf_training_w7_lag_2.csv'))
y_pred <- as.data.frame(model,pars="y_rep")%>%
  gather(factor_key = T) %>%
  group_by(key)%>%
  summarize(lb=quantile(value,probs=0.05),
            median=quantile(value,probs=0.5),
            ub=quantile(value,probs=0.95)) %>%
  add_column(week=test_data$week,cases=test_data$cases,year=test_data$year+2000)

ggplot(data=subset(y_pred,year!=2016),aes(y=median,x=week))+
  geom_line(color="red")+
  geom_line(aes(x=week,y=ub),color="red",linetype="dashed")+
  geom_line(aes(x=week,y=lb),color="red",linetype="dashed")+
  geom_point(aes(x=week,y=cases),size=0.8)+
  xlab("epi-week")+ylab("number of cases")+
  facet_wrap(~year)

shinystan::launch_shinystan(model)


