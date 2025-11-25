library(tibble)

set.seed(34)            # set a seed for the random number generator
# define the data structure
n <- 50                 # sample size
x <- runif(n, 10, 30)   # sample values of the predictor variable

# define values for each model parameter
sigma <- 5              # standard deviation of the residuals
b0 <- 2                 # intercept
b1 <- 0.7               # slope

# simulate y-values from the model
mu <- b0 + b1 * x       # define the regression line (deterministic part)
y <- rnorm(n, mu, sd = sigma) # simulate y-values (stochastic part)

# save data in an object
dat <- tibble(x = x, y = y)


mod <-  lm(y ~ x, data = dat)
coef(mod)
summary(mod)$sigma


library(arm)
nsim <- 1000  # in real analyses, we set nsim to 5000 or more; rstanarm and brms have a default of 4000
bsim <- sim(mod, n.sim = nsim)

apply(X = coef(bsim), MARGIN = 2, FUN = quantile, probs = c(0.025, 0.975)) %>% 
  round(2)


library(brms)

# check which parameters need a prior and what default prior is used by brm
# get_prior(y ~ x, data = dat)

priors <- get_prior(y ~ x, data = dat)
priors[,c("prior","class","coef","lb","source")]  # only show columns with an entry


library(rstanarm)
mod.rstanarm <- stan_glm(y~x, data=dat, refresh=0)
mod.brm <- brm(y~x, data=dat)

# compile the model without MCMC sampling, save the compiled model for later use,
# get MCMC samples from the compiled model using update with recompile=F.

# mod.brm.compiled <- brm(y~x, data=dat, chains=0)
# save(mod.brm.compiled,file="RData/mod.brm.compiled.RData")
load("RData/mod.brm.compiled.RData")
mod.brm <- update(mod.brm.compiled, recompile = FALSE, seed=123, refresh=0)
# refesh=0 prevents printing the MCMC progress; this makes sense here in the
# book, but in your application you may prefer to see the progress



par(mar = c(4, 4, 0, 0))
plot(x, y, pch = 16, las = 1, 
     xlab = "Outcome (y)")
library(scales)   # for function alpha
# alpha() takes a named color, the second argument is transparency; it corresponds to
# rgb(173/255, 216/255, 230/255, 0.05)
for(i in 1:nsim) {
  abline(coef(bsim)[i,1], coef(bsim)[i,2], col = alpha("lightblue",0.05))
}

# Calculate 95% compatibility interval
newdat <- tibble(x = seq(min(dat$x), max(dat$x), length = 100))
newmodmat <- model.matrix( ~ x, data = newdat)
fitmat <- matrix(ncol = nsim, nrow = nrow(newdat))
for(i in 1:nsim) {fitmat[,i] <- newmodmat %*% coef(bsim)[i,]}
newdat$CI_lo <- apply(fitmat, 1, quantile, probs = 0.025)
newdat$CI_up <- apply(fitmat, 1, quantile, probs = 0.975)

# Make plot
regplot <- 
  ggplot(dat, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  geom_line(data = newdat, aes(x = x, y = CI_lo), lty = 3) +
  geom_line(data = newdat, aes(x = x, y = CI_up), lty = 3) +
  labs(x = "Predictor (x)", y = "Outcome (y)")
regplot



# increase number of simulation to produce smooth lines of the posterior
# predictive distribution
set.seed(34)
nsim <- 50000
bsim <- sim(mod, n.sim=nsim)
fitmat <- matrix(ncol=nsim, nrow=nrow(newdat))
for(i in 1:nsim) fitmat[,i] <- newmodmat%*%coef(bsim)[i,]

# prepare matrix for simulated new data
newy <- matrix(ncol=nsim, nrow=nrow(newdat)) 

# for each simulated fitted value, simulate one new y-value
for(i in 1:nsim) {
  newy[,i] <- rnorm(nrow(newdat), mean = fitmat[,i], sd = bsim@sigma[i])
}

# Calculate 2.5% and 97.5% quantiles
newdat$pred_lo <- apply(newy, 1, quantile, probs = 0.025)
newdat$pred_up <- apply(newy, 1, quantile, probs = 0.975)

# Add the posterior predictive distribution to the plot
regplot +
  geom_line(data = newdat, aes(x = x, y = pred_lo), lty = 2) +
  geom_line(data = newdat, aes(x = x, y = pred_up), lty = 2)


# when creating newdat$x, we did not makes sure that 25 is one of the 100 x-values. This could
# be achieved by specifically adding the 25:
# newdat <- tibble(x = sort(c(seq(min(dat$x), max(dat$x), length = 100),25)))
# we did not do that above to not overload the new material. So, now, we simply search for the
# x-value closest to 25 (which is so near 25 that the conclusion is valid)

sum(newy[abs(newdat$x-25) == min(abs(newdat$x-25)), ] > 20) / nsim


bsim.rstanarm <- as.matrix(mod.rstanarm)
# bsim.rstanarm: in analogy to the bsim we created above
head(bsim.rstanarm,2)
bsim.rstanarm.fix <- bsim.rstanarm[,colnames(bsim.rstanarm) %in% names(coef(mod.rstanarm))]
apply(bsim.rstanarm.fix, 2, mean)
apply(bsim.rstanarm.fix, 2, quantile, probs=c(0.025,0.975))  # 95% compatibility interval

fitmat.rstanarm <- posterior_epred(mod.rstanarm, newdata=newdat)

newdat.rstanarm <- tibble(x = seq(min(dat$x), max(dat$x), length = 100))  # same as above
newdat.rstanarm$fit <- apply(fitmat.rstanarm,2,mean)
newdat.rstanarm$lwr <- apply(fitmat.rstanarm,2,quantile,probs=0.025)
newdat.rstanarm$upr <- apply(fitmat.rstanarm,2,quantile,probs=0.975)




library(posterior)
bsim.brm.fix <- as_draws_matrix(mod.brm) %>%
  subset_draws(variable="b_",regex=T)

head(bsim.brm.fix)   # in analogy to bsim.rstanarm.fix created above
# we store the values for later use; the outer parentheses make that the result is also printed
( t.fit <- apply(bsim.brm.fix,2,mean) )
( t.ci <- apply(bsim.brm.fix,2,quantile,probs=c(0.025,0.975)) )
newdat.brm <- conditional_effects(mod.brm, effects="x", plot=F)$x
head(newdat.brm)
