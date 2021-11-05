library(rstan)

wd <- getwd()

w_data <- paste0(wd,'/stan_modeling/stan_data/rot/')

training <- read.csv(paste0(w_data,'rot_training_w7_lag_0.csv'))
testing <- read.csv(paste0(w_data,'rot_testing_w7_lag_0.csv'))

N <- nrow(training)
y <- training$cases


model <- stan(paste0(wd,'/stan_modeling/intercept_long.stan'),
              data = c("N","y"),
              cores = min(2,parallel::detectCores()),
              chains=2,warmup = 1000,iter=3000)

save(model,file=paste0(wd,"/stan_modeling/stan_outputs/inter_simp_lag_0_win_7_rot.Rsave"))
