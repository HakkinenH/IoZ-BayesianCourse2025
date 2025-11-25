

#https://ourcodingclub.github.io/tutorials/stan-intro/#priors
setwd("C:/Users/Henry/Documents/Research/Teaching/BayesianCourse2025")
setwd("C:/Users/henry.hakkinen/OneDrive - Zoological Society of London/Documents/Research/Teaching/BayesianCourse2025/")
library(rstan)
library(gdata)
library(bayesplot)
library(ggplot2)

data_kilpis <- read.delim("data/kilpisjarvi-summer-temp-2022.csv", sep = ";")
ggplot() +
  geom_point(aes(year-1952, temp.summer), data = data.frame(data_kilpis), size = 1) +
  labs(y = 'Summer temp. in Kilpisjärvi', x= "Years since 1952") +
  guides(linetype = "none")

#data_kilpis<-read.csv("data/seaice.csv")

#
write("// Stan model for simple linear regression

data {
 int < lower = 1 > N; // Sample size
 vector[N] x; // Predictor
 vector[N] y; // Outcome
}

parameters {
 real alpha; // Intercept
 real beta; // Slope (regression coefficients)
 real < lower = 0 > sigma; // Error SD
 real xpred; // new covariate value to make predictions
}

model {
 alpha ~ normal(1, 10); //prior intercept
 beta ~ normal(1, 0.1); //prior slope
 y ~ normal(alpha + x * beta , sigma);
}

generated quantities {
  // sample from the predictive distribution
  real ypred = normal_rng(alpha + beta * xpred, sigma);
  // compute log predictive densities to be used for LOO-CV
  vector[N] log_lik;
  for (i in 1 : N) {
    log_lik[i] = normal_lpdf(y[i] | mu[i], sigma);
  }
} // The posterior predictive distribution",

"Models/stan_lm2.stan")

stanc("Models/stan_lm2.stan")
stan_model1 <- "Models/stan_lm2.stan"

x <- I(data_kilpis$year - 1952)
y <- data_kilpis$temp.summer
N <- length(data_kilpis$year)
stan_data <- list(N = N, x = x, y = y)

fit <- stan(file = stan_model1, 
            data = stan_data, 
            warmup = 500, 
            iter = 1000, 
            chains = 4, 
            cores = 2, 
            thin = 1)



#convergence
traceplot(fit, inc_warmup =T)
traceplot(fit, inc_warmup =F)

fit
posterior <- extract(fit)
str(posterior)


plot(posterior$alpha, type = "l")
plot(posterior$beta, type = "l")



plot(posterior$sigma, type = "l")

par(mfrow = c(1,3))

plot(density(posterior$alpha), main = "Alpha")

plot(density(posterior$beta), main = "Beta")

plot(density(posterior$sigma), main = "Sigma")


#
stan_dens(fit)
stan_hist(fit)

#check fit
library(rstantools)
yrep_nb <- posterior_predict(fit, draws = 500)


sum(posterior$beta>0)/length(posterior$beta)
sum(posterior$beta>0.2)/length(posterior$beta)



plot(y ~ x, pch = 20)

for (i in 1:500) {
  abline(posterior$alpha[i], posterior$beta[i], col = "gray", lty = 1)
}

abline(mean(posterior$alpha), mean(posterior$beta), col = 6, lw = 2)


