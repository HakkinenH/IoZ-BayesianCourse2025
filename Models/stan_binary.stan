data {
  int n; //number of observations in the data
  int<lower = 0, upper = 1> admit[n]; //integer of length n for admission decision
  vector[n] gre; //vector of length n for GRE scores
  vector[n] gpa; //vector of length n for GPA
  vector[n] ranking; //vector of length n for school ranking
}

parameters {
  real alpha; //intercept parameter
  vector[3] beta; //vector of coefficients
}

model {
  //linear predictor
  vector[n] p;
  
  //linear equation
  p = alpha + beta[1] * gre + beta[2] * gpa + beta[3] * ranking;
  
  //likelihood and link function
  admit ~ bernoulli_logit(p);
}