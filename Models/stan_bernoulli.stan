data {
  int n; //number of observations in the data
  array [n] int<lower = 0, upper = 1> admit; //integer of length n for admission decision
  vector[n] gre; //vector of length n for GRE scores
  vector[n] gpa; //vector of length n for GPA
  vector[n] ranking; //vector of length n for school ranking
}

parameters {
  real alpha; //intercept parameter
  vector[3] beta; //vector of coefficients, we want three beta parameters (gre/gpa/ranking) so specify 3. Could also define them all manually
}

model {
  //linear predictor
  vector[n] p;
  
  //linear equation
  p = alpha + beta[1] * gre + beta[2] * gpa + beta[3] * ranking;
  
  //prior expectations
  // NOTE: can comment out for demonstration purposes
  alpha ~ normal(-1, 1.5);
  beta ~ normal(0.5, 1.0);
  
  //likelihood and link function
  admit ~ bernoulli_logit(p);
}
