// Stan model for simple linear regression
////////////////////////////////////////////


// DATA BLOCK This is where we declare the inputs. 
// These are the variables in the model, and must match the variables names in our dataframe/list
// we also declare N as our sample size, this is always requred for stan models
data {
 int < lower = 1 > N; // Sample size
 vector[N] x; // Predictor
 vector[N] y; // Outcome
}

// PARAMETER BLOCK. This is where we declare our model parameters
// in a linear regression we have:
// alpha: our intercept
// beta: our slope for one variable. If we have multiple linear predictors we would need one variable per predictor
// sigma: our observed error around our linear fit
parameters {
 real alpha; // Intercept
 real beta; // Slope (regression coefficients)
 real < lower = 0 > sigma; // Error SD
}

// MODEL BLOCK. This is where we declare our model and our priors
// for example, beta has a normally distributed prior, with mean of 0 and variance of 0.1
model {
 alpha ~ normal(1, 10); //prior intercept
 beta ~ normal(0, 0.1); //prior slope
 sigma ~ normal(0, 10);

 // full model with linear component and error term
 y ~ normal(alpha + beta * x, sigma);
}

//GENERATED BLOCK. This is optional but useful for model fitting checks
// This section asks the stan model to record predictions as we go for all our estimates, but without the error term
// it allows us to do posterior checks and look at our model fit
generated quantities {
 // save linear component so we can look at estimates in diagnostics
 vector[N] mu = alpha + beta * x;
}
