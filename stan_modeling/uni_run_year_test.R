library(rstan)

wd <- getwd()

w_data <- paste0(wd,'/stan_modeling/stan_data/rot/')

training <- read.csv(paste0(w_data,'rot_training_w7_lag_0.csv'))
testing <- read.csv(paste0(w_data,'rot_testing_w7_lag_0.csv'))

W <- length(unique(training$cal_week))
N_year <- length(unique(training$year))
N <- nrow(training)
ww <- training$week
y <- training$cases
w1 <- training$TXK/10
yy <- training$year

N_pred <- nrow(testing)
W_pred <- testing$week
y_test <- testing$cases
w1_pred <- testing$TXK/10

model <- stan(paste0(wd,'/stan_modeling/uni_slope_year.stan'),
              data = c("W","N","ww","y","w1","N_pred","W_pred","y_test","w1_pred","yy","N_year"),
              cores = min(2,parallel::detectCores()),
              chains=2,warmup = 1000,iter=3000)

save(model,file=paste0(wd,"/stan_modeling/stan_outputs/temp_year_lag_0_win_7_rot.Rsave"))
