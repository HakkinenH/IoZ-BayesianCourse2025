data {
  int n; // number of observations
  int n_county; // number of counties
  vector[n] log_radon;
  vector[n] vfloor;
  array[n] int<lower = 0, upper = n_county> county;  
}

parameters {
  vector[n_county] alpha; // vector of county intercepts
  real beta; // slope parameter
  real<lower = 0> sigma_a; // variance of counties
  real<lower = 0> sigma_y; // model residual variance
  real mu_a; // mean of counties
}

model {
  // conditional mean
  vector[n] mu;

  // linear combination
  mu = alpha[county] + beta * vfloor;

  // priors
  beta ~ normal(0, 1);

  // hyper-priors
  mu_a ~ normal(0, 1);
  sigma_a ~ cauchy(0, 2.5);
  sigma_y ~ cauchy(0, 2.5);

  // level-2 likelihood
  alpha ~ normal(mu_a, sigma_a);

  // level-1 likelihood
  log_radon ~ normal(mu, sigma_y);
}

generated quantities {
  vector[n] log_lik; // calculate log-likelihood
  vector[n] y_rep; // replications from posterior predictive distribution

  for (i in 1:n) {
    // generate mpg predicted value
    real log_radon_hat = alpha[county[i]] + beta * vfloor[i];

    // calculate log-likelihood
    log_lik[i] = normal_lpdf(log_radon[i] | log_radon_hat, sigma_y);
    // normal_lpdf is the log of the normal probability density function

    // generate replication values
    y_rep[i] = normal_rng(log_radon_hat, sigma_y);
    // normal_rng generates random numbers from a normal distribution
  }
}
