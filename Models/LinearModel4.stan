data {
  int < lower = 1 > n; // Sample size
  vector[n] antm;
  vector[n] log_f;
  vector[n] weight;
  vector[n] stage;
}
parameters {
  real alpha;
  real beta_1;
  real beta_2;
  real beta_3;
  real<lower=0> sigma;
}
model {
  antm ~ normal(alpha + beta_1 * log_f + beta_2 * weight + beta_3 * stage, sigma);
  alpha ~ normal(0, 10);
  beta_1 ~ normal(0, 10);
  beta_2 ~ normal(0, 10);
  beta_3 ~ normal(0, 10);
  sigma ~ cauchy(0, 10);
}
generated quantities {
  //replications for the posterior predictive distribution
  array[n] real y_rep = normal_rng(alpha + beta_1 * log_f + beta_2 * weight + beta_3 * stage, sigma);

}
