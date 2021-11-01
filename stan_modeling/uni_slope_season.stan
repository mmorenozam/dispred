data {
  int<lower=0> W; // # of weeks
  int<lower=0> N; // # of observations
  int<lower=0> S; // # season
  int<lower=0,upper=S> sea[N];
  int<lower=1,upper=W> ww[N]; //week index
  int y[N]; //observations
  vector[N] w1; //weather variable 1
  int<lower=0> N_pred; // # of observations for prediction
  int S_pred[N_pred]; // index of season for predition
  int W_pred[N_pred]; // index of weeks for prediction
  int y_test[N_pred]; // data for testing RMSE
  vector[N_pred] w1_pred; //weather data for prediction
}

parameters {
  real alpha_beta;
  real alpha_gamma;
  real <lower=0> sigma_gamma;
  real mu;
  real <lower=0> inv_phi;
  vector[W] beta;
  real <lower=0> sigma_beta;
  vector[S] gamma;
}

transformed parameters {
  real phi = 1/inv_phi;
}

model {
  mu ~ normal(log(30),1);
  inv_phi ~ normal(0,1);
  alpha_beta ~ normal(-1,1);
  sigma_beta ~ normal(0,1);
  alpha_gamma ~ normal(-1,1);
  sigma_gamma ~ normal(0,1);
  gamma ~ normal(alpha_gamma,sigma_gamma);
  beta ~ normal(alpha_beta,sigma_beta);
  for (n in 1:N){
    y[n] ~ neg_binomial_2_log(mu+beta[ww[n]]*w1[n]+gamma[sea[n]],phi);
  }
}

generated quantities {
  int y_rep[N];
  int y_pred[N_pred];
  vector[N] indiv_squared_errors_train;
  real <lower=0> sum_of_squares_train;
  real <lower=0> root_mean_squared_error_train;
  vector[N_pred] indiv_squared_errors_pred;
  real <lower=0> sum_of_squares_pred;
  real <lower=0> root_mean_squared_error_pred;
  vector[N] log_lik;
  for (n in 1:N){
    y_rep[n] = neg_binomial_2_log_rng(mu+beta[ww[n]]*w1[n]+gamma[sea[n]],phi);
    log_lik[n] = neg_binomial_2_log_lpmf(y[n]|mu+beta[ww[n]]*w1[n]+gamma[sea[n]],phi);
    indiv_squared_errors_train[n] = (y[n]-y_rep[n])^2;
  }
  for (i in 1:N_pred){
    y_pred[i] = neg_binomial_2_log_rng(mu+beta[W_pred[i]]*w1_pred[W_pred[i]]+gamma[S_pred[i]],phi);
    indiv_squared_errors_pred[i] = (y_test[i]-y_pred[i])^2;
  }
  sum_of_squares_pred = sum(indiv_squared_errors_pred);
  sum_of_squares_train = sum(indiv_squared_errors_train);
  root_mean_squared_error_pred = sqrt(sum_of_squares_pred/N_pred);
  root_mean_squared_error_train = sqrt(sum_of_squares_train/N);
}

