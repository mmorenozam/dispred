data {
  int<lower=0> W; // # of weeks
  int<lower=0> N; // # of observations
  int<lower=1,upper=W> ww[N]; //week index
  int y[N]; //observations
  vector[N] w1; //weather variable 1
  int<lower=0> N_pred; // # of observations for prediction
  int W_pred[N_pred]; // index of weeks for prediction
  int y_test[N_pred]; // data for testing RMSE
  vector[N_pred] w1_pred; //weather data for prediction
  
}

parameters {
  real alpha_beta;
  real mu;
  real <lower=0> inv_phi;
  vector[W] beta;
  real <lower=0> sigma_beta;
  real <lower=0,upper=1> rho_raw;
  vector[W] week_raw;
  real <lower=0> sigma_week;
}

transformed parameters {
  real phi = 1/inv_phi;
  vector[W] week = sigma_week*week_raw;
  real rho = 2*rho_raw-1;
  week[1] /= sqrt(1-rho^2);
  for (w in 2:W){
    week[w] += rho*week[w-1];
  }
}

model {
  mu ~ normal(log(30),1);
  inv_phi ~ normal(0,1);
  alpha_beta ~ normal(-1,1);
  sigma_beta ~ normal(0,1);
  rho_raw ~ beta(10,5);
  week_raw ~ normal(0,1);
  sigma_week ~ normal(0,1);
  beta ~ normal(alpha_beta,sigma_beta);
  for (n in 1:N){
    y[n] ~ neg_binomial_2_log(mu+beta[ww[n]]*w1[n]+week[ww[n]],phi);
  }
}

generated quantities {
  int y_rep[N];
  int y_pred[N_pred];
  vector[N_pred] indiv_squared_errors;
  real <lower=0> sum_of_squares;
  real <lower=0> root_mean_squared_error;
  vector[N] log_lik;
  for (n in 1:N){
    y_rep[n] = neg_binomial_2_log_rng(mu+beta[ww[n]]*w1[n]+week[ww[n]],phi);
  }
  for (n in 1:N){
    log_lik[n] = neg_binomial_2_log_lpmf(y[n]|mu+beta[ww[n]]*w1[n]+week[ww[n]],phi);
  }
  for (i in 1:N_pred){
    y_pred[i] = neg_binomial_2_log_rng(mu+beta[W_pred[i]]*w1_pred[W_pred[i]]+week[W_pred[i]],phi);
    indiv_squared_errors[i] = (y_test[i]-y_pred[i])^2;
  }
  sum_of_squares = sum(indiv_squared_errors);
  root_mean_squared_error = sqrt(sum_of_squares/N_pred);
}

