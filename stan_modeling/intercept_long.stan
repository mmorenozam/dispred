data {
  int <lower=0> N; //# of observations
  int y[N]; //cases
}

parameters{
  real mean_mu;
  real <lower=0> sigma_mu;
  real <lower=0> inv_phi;
  vector[N] mu_random;
  real mu;
}

transformed parameters {
  vector[N] eta = mu+mu_random;
}

model {
  mu ~ normal(mean_mu, sigma_mu);
  mu_random ~ normal(-1,1);
  inv_phi ~ normal(0,1);
  mean_mu ~ normal(log(30),1);
  sigma_mu ~ normal(0,1);
  y ~ neg_binomial_2_log(eta,1/inv_phi);
}

generated quantities {
  int y_rep[N];
  vector[N] ise; //individual squared errors
  real <lower=0> sos; //sum of squares
  real <lower=0> rmse; //root mean squared error
  for (n in 1:N){
    y_rep[n] = neg_binomial_2_log_rng(eta[n],1/inv_phi);
    ise[n] = (y[n]-y_rep[n])^2;
  }
  sos = sum(ise);
  rmse = sqrt(sos/N);
}

