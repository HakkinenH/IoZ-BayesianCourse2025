data {
  int n; //number of observations in the data
  vector[n] weight; //vector of length n for lynx weight
  vector[n] kills_c; //vector of length n for number of kills per Ind
  vector[n] speed_c; //vector of length n for each lynx's speed
  vector[n] humanDens_c; //vector of length n for local human density per Ind
}

parameters {
  real alpha; //the intercept parameter
  real beta_k; //slope parameter for kills
  real beta_sp; //slope parameter for speed
  real beta_hu; //slope parameter for human density
  real<lower = 0> sigma; //variance parameter and restrict it to positive values
}

model {
  //linear predictor mu
  vector[n] mu;
  
  //write the linear equation
  mu = alpha + beta_k * kills_c + beta_sp * speed_c + beta_hu * humanDens_c;
  
  //prior expectations
  alpha ~ normal(20, 10);
  beta_k ~ normal(0, 5);
  beta_sp ~ normal(0, 5); 
  beta_hu ~ normal(0, 5); 
  sigma ~ uniform(0, 10);
  
  //likelihood function
  weight ~ normal(mu, sigma);
}

generated quantities {
  //replications for the posterior predictive distribution
  array[n] real y_rep = normal_rng(alpha + beta_k * kills_c + beta_sp * speed_c + beta_hu * humanDens_c, sigma);

}
