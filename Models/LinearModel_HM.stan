data {
  int<lower=0> n;
  int<lower=0> N_stage;
  vector[n] antm;
  vector[n] log_f;
  vector[n] weight;
  array[n] int stage_ind;
}
parameters {
// we create the same number intercept estimates as number of stages
  vector[N_stage] alpha;
  real beta_1;
  real beta_2;
  real<lower=0> sigma;
  real<lower=0> sigma_alpha;
}
model {

// basic linear model, but with a hierarchical/random effect on the intercept
  for (i in 1:n){
    antm[i] ~ normal(alpha[stage_ind[i]] + beta_1 * log_f[i] + beta_2 * weight[i], sigma);
  }
  
// this section allows an intercept per stage category. key part of HC  
   for (j in 1:N_stage) {
     alpha[j] ~ normal(0, sigma_alpha);
   }

// set priors
  beta_1 ~ normal(0, 10);
  beta_2 ~ normal(0, 10);
// note we have two variance terms now! One for the global distribution
// and one for the variance of intercepts around stages
  sigma ~ cauchy(0, 10);
  sigma_alpha ~ cauchy(0, 10);
}

// generate additional predictions for model fit checks
generated quantities {
  vector[n] y_rep;
  for (i in 1:n) {
    y_rep[i] = normal_rng(alpha[stage_ind[i]] + beta_1 * log_f[i] + beta_2 * weight[i], sigma);
  }
}

