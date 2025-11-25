
install.packages("rstan", type="source")
install.packages(c("loo", "ggplot2", "bayesplot"), type="source")

#install.packages("threejs", type="source")
#install.packages("shinystan", type="source")

library(tidyr) 
library(rstan) 
rstan_options(auto_write = TRUE)
options(mc.cores = 1)
library(loo)
library(ggplot2)
library(gridExtra)
library(bayesplot)
theme_set(bayesplot::theme_default(base_family = "sans"))
#library(shinystan)
library(rprojroot)
#root<-has_file(".BDA_R_demos_root")$make_fix_file()
SEED <- 48927 # set random seed for reproducability

"C:\Users\henry.hakkinen\Downloads"
data_kilpis <- read.delim("C:/Users/henry.hakkinen/Downloads/kilpisjarvi-summer-temp.csv", sep = ";")
data_lin <-list(N = nrow(data_kilpis),
                x = data_kilpis$year,
                xpred = 2016,
                y = data_kilpis[,5])
ggplot() +
  geom_point(aes(x, y), data = data.frame(data_lin), size = 1) +
  labs(y = 'Summer temp. @Kilpisjärvi', x= "Year") +
  guides(linetype = "none")

#code_lin <- root("demos_rstan", "lin.stan")
code_lin <-("// Gaussian linear model with adjustable priors
data {
  int<lower=0> N; // number of data points
  vector[N] x; // covariate / predictor
  vector[N] y; // target
  real xpred; // new covariate value to make predictions
  real pmualpha; // prior mean for alpha
  real psalpha; // prior std for alpha
  real pmubeta; // prior mean for beta
  real psbeta; // prior std for beta
  real pssigma; // prior std for half-normal prior for sigma
}
parameters {
  real alpha; // intercept
  real beta; // slope
  real<lower=0> sigma; // standard deviation is constrained to be positive
}
transformed parameters {
  // deterministic transformation of parameters and data
  vector[N] mu = alpha + beta * x; // linear model
}
model {
  alpha ~ normal(pmualpha, psalpha); // prior
  beta ~ normal(pmubeta, psbeta); // prior
  sigma ~ normal(0, pssigma); // as sigma is constrained to be positive,
  // this is same as half-normal prior
  y ~ normal(mu, sigma); // observation model / likelihood
  // the notation using ~ is syntactic sugar for
  //  target += normal_lpdf(alpha | pmualpha, psalpha);
  //  target += normal_lpdf(beta | pmubeta, psbeta);
  //  target += normal_lpdf(y | mu, sigma);
  // target is the log density to be sampled
}
generated quantities {
  // sample from the predictive distribution
  real ypred = normal_rng(alpha + beta * xpred, sigma);
  // compute log predictive densities to be used for LOO-CV
  vector[N] log_lik;
  for (i in 1 : N) {
    log_lik[i] = normal_lpdf(y[i] | mu[i], sigma);
  }
}")
writeLines(code_lin, "StanGLM.stan")

data_lin_priors <- c(list(
  pmualpha = mean(unlist(data_kilpis[,5])), # centered
  psalpha = 100, # weakly informative
  pmubeta = 0, # a priori incr. and decr. as likely
  psbeta = (.1--.1)/6, # avg temp prob does does not incr. more than a degree per 10 years
  pssigma = 1), # total variation in summer average temperatures is less +-3 degrees
  data_lin)

ta<-Sys.time()
fit_lin <- stan(file = "StanGLM.stan", 
                data = data_lin_priors, 
                seed = SEED,
                chains=10,
                iter=20000,
                warmup=500,
                cores=4,
                thin=1
)
tb<-Sys.time()
print(tb-ta)

traceplot(fit_lin, "beta")
mcmc_rank_overlay(fit_lin, "beta")

#1 core, 2 chains 
#1 core, 4 chains 57s
#2 core, 4 chains 47s
#4 core, 4 chains 44s

#10 chain, 20000 iter, 1 core 4.75 minutes (on laptop 10.6 min)
#10 chain, 20000 iter, 10 core 2 minutes
#10 chain, 20000 iter, 100 core 2 min



monitor(fit_lin)

check_hmc_diagnostics(fit_lin)

draws_lin <- rstan::extract(fit_lin, permuted = T)
mean(draws_lin$beta>0) # probability that beta > 0


mu <- apply(draws_lin$mu, 2, quantile, c(0.05, 0.5, 0.95)) %>%
  t() %>% data.frame(x = data_lin$x, .)  %>% gather(pct, y, -x)

pfit <- ggplot() +
  geom_point(aes(x, y), data = data.frame(data_lin), size = 1) +
  geom_line(aes(x, y, linetype = pct), data = mu, color = 'red') +
  scale_linetype_manual(values = c(2,1,2)) +
  labs(y = 'Summer temp. @Kilpisjärvi', x= "Year") +
  guides(linetype = "none")
pars <- intersect(names(draws_lin), c('beta','sigma','ypred'))
draws <- as.data.frame(fit_lin)
phist <- mcmc_hist(draws, pars = pars)
grid.arrange(pfit, phist, nrow = 2)


