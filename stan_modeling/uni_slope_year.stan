data {
  int<lower=0> W; // # of weeks
  int<lower=0> N; // # of observations
  int<lower=0> N_year; // # year
  int<lower=0,upper=N_year> yy[N];
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
  real alpha_gamma;
  real <lower=0> sigma_gamma;
  real mu;
  real <lower=0> inv_phi;
  vector[W] beta;
  real <lower=0> sigma_beta;
  vector[N_year] gamma;
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
    y[n] ~ neg_binomial_2_log(mu+beta[ww[n]]*w1[n]+gamma[yy[n]],phi);
  }
}

generated quantities {
  int y_rep[N];
  int y_pred[N_pred];
  real gamma_pred;
  vector[N_pred] indiv_squared_errors;
  real <lower=0> sum_of_squares;
  real <lower=0> root_mean_squared_error;
  vector[N] log_lik;
  gamma_pred = normal_lpdf(gamma|alpha_gamma,sigma_gamma);
  for (n in 1:N){
    y_rep[n] = neg_binomial_2_log_rng(mu+beta[ww[n]]*w1[n]+gamma[yy[n]],phi);
  }
  for (n in 1:N){
    log_lik[n] = neg_binomial_2_log_lpmf(y[n]|mu+beta[ww[n]]*w1[n]+gamma_pred,phi);
  }
  for (i in 1:N_pred){
    y_pred[i] = neg_binomial_2_log_rng(mu+beta[W_pred[i]]*w1_pred[W_pred[i]]+gamma_pred,phi);
    indiv_squared_errors[i] = (y_test[i]-y_pred[i])^2;
  }
  sum_of_squares = sum(indiv_squared_errors);
  root_mean_squared_error = sqrt(sum_of_squares/N_pred);
}

