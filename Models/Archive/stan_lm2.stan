// Stan model for simple linear regression

data {
 int < lower = 1 > N; // Sample size
 vector[N] x; // Predictor
 vector[N] y; // Outcome
}

parameters {
 real alpha; // Intercept
 real beta; // Slope (regression coefficients)
 real < lower = 0 > sigma; // Error SD
}

model {
 alpha ~ normal(1, 10); //prior intercept
 beta ~ normal(0, 0.1); //prior slope
 sigma ~ normal(0, 10);

 // full model with linear component and error term
 y ~ normal(alpha + beta * x, sigma);
}
generated quantities {
 // save linear component so we can look at estimates in diagnostics
 vector[N] mu = alpha + beta * x;
}
