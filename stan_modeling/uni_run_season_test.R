library(tidyverse)
library(rstan)

wd <- getwd()

w_data <- paste0(wd,'/stan_modeling/stan_data/rot/')

training <- read.csv(paste0(w_data,'rot_training_w7_lag_3.csv'))
testing <- read.csv(paste0(w_data,'rot_testing_w7_lag_3.csv'))

sea_func <- function(df){
  df$date <- as.Date(df$date)
  df$month <- as.numeric(format(df$date,"%m"))
  df <- df %>% mutate(season=ifelse(month >= 3& month <= 5,1,
                                ifelse(month >=6 & month <=8,2,
                                       ifelse(month >= 9 & month <= 11,3,4))))
  return(df)
}

training <- sea_func(training)
testing <- sea_func(testing)

                    
W <- length(unique(training$week))
S <- length(unique(training$season))
N <- nrow(training)
ww <- training$week
sea <- training$season
y <- training$cases
w1 <- training$TXK/10

N_pred <- nrow(testing)
S_pred <- testing$season
W_pred <- testing$week
y_test <- testing$cases
w1_pred <- testing$TXK/10

model <- stan(paste0(wd,'/stan_modeling/uni_slope_season.stan'),
              data = c("W","N","ww","y","w1","N_pred","W_pred","y_test","w1_pred","S_pred","S","sea"),
              cores = min(2,parallel::detectCores()),
              chains=2,warmup = 1000,iter=3000)

save(model,file=paste0(wd,"/stan_modeling/stan_outputs/temp_season_lag_3_win_7_rot.Rsave"))
