data {
  int<lower=0> W; // # of weeks
  int<lower=0> N; // # of observations
  int<lower=1,upper=W> ww[N]; //week index
  int y[N]; //observations
  vector[N] w1; //weather variable 1
  vector[N] w2; //weather variable 2
  int<lower=0> N_pred; // # of observations for prediction
  int W_pred[N_pred]; // index of weeks for prediction
  int y_test[N_pred]; // data for testing RMSE
  vector[N_pred] w1_pred; //weather data for prediction
  vector[N_pred] w2_pred;
}

parameters {
  real alpha_beta;
  real <lower=0> sigma_beta;
  real alpha_theta;
  real <lower=0> sigma_theta;
  real alpha_zeta;
  real <lower=0> sigma_zeta;
  real mu;
  real <lower=0> inv_phi;
  vector[W] beta;
  vector[W] theta;
  vector[W] zeta;
  
}

transformed parameters {
  real phi = 1/inv_phi;
}

model {
  mu ~ normal(log(30),1);
  inv_phi ~ normal(0,1);
  alpha_beta ~ normal(-1,1);
  sigma_beta ~ normal(0,1);
  alpha_theta ~ normal(-1,1);
  sigma_theta ~ normal(0,1);
  alpha_zeta ~ normal(-1,1);
  sigma_zeta ~ normal(0,1);
  beta ~ normal(alpha_beta,sigma_beta);
  theta ~ normal(alpha_theta,sigma_theta);
  zeta ~ normal(alpha_theta,sigma_theta);
  for (n in 1:N){
    y[n] ~ neg_binomial_2_log(mu+beta[ww[n]]*w1[n]+theta[ww[n]]*w2[n]+zeta[ww[n]]*w1[n]*w2[n],phi);
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
    y_rep[n] = neg_binomial_2_log_rng(mu+beta[ww[n]]*w1[n]+theta[ww[n]]*w2[n]+zeta[ww[n]]*w1[n]*w2[n],phi);
  }
  for (n in 1:N){
    log_lik[n] = neg_binomial_2_log_lpmf(y[n]|mu+beta[ww[n]]*w1[n]+theta[ww[n]]*w2[n]+zeta[ww[n]]*w1[n]*w2[n],phi);
  }
  for (i in 1:N_pred){
    y_pred[i] = neg_binomial_2_log_rng(mu+beta[W_pred[i]]*w1_pred[W_pred[i]]+theta[W_pred[i]]*w2_pred[W_pred[i]]+zeta[W_pred[i]]*w1_pred[W_pred[i]]*w2_pred[W_pred[i]],phi);
    indiv_squared_errors[i] = (y_test[i]-y_pred[i])^2;
  }
  sum_of_squares = sum(indiv_squared_errors);
  root_mean_squared_error = sqrt(sum_of_squares/N_pred);
}

