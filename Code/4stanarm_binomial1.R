################################################################################
# SIMPLE BINARY (BERNOULLI) MODEL IN STANARM
################################################################################

#############
#DESCRIPTION
#This file uses a similar model to stan_bernoulli
#but uses the stanarm package
#this uses stan, but is designed to look more like regular LME packages
#it loads example data, runs a simple binomial model
#runs diagnostics and gives a simple output
#then we build some more complex models and do some model comparison
#############


#Clear your previous work
rm(list=ls())


#set your working directory
#I'm lazy so I autodetect where this file is and set it one level higher
#if this fails for any reason, set setwd manually to the folder path of the main repo
if(!require("rstudioapi")) install.packages("rstudioapi")
curpath<-dirname(rstudioapi::getSourceEditorContext()$path)
curpath
setwd(curpath)
#go up a level out of the code folder for neatness
setwd("../")


#load packages and check installs
source("Code/0Setup.R")

#the following uses stan arm!


#############
### LOAD AND EXPLORE DATA
#############

#load our example dataset
data(wells)
#A survey of 3200 residents in a small area of Bangladesh suffering from arsenic contamination of groundwater. 
#switch is our result variable, about whether residents have swapped their well source or not
#see ?wells for a full description of the data set

#simplify distance to make units more interpretable. dist100 is in hundreds of metres
wells$dist100 <- wells$dist / 100

#quick plot: have people who swapped wells live closer to nearest safe water?
ggplot(wells, aes(x = dist100, y = after_stat(density), fill = switch == 1)) +
  geom_histogram() +
  scale_fill_manual(values = c("gray30", "skyblue"))


#############
### LOAD AND RUN MODEL 1
#############

#we will start with a very simple model. does the probability of well switching go up or down with distance?
#stan arm is designed to look a bit like frequentist packages, so it is less flexible than regular stan
#but still very powerful for most standard models!

#first off we should set our priors for our predictors. stanarm comes with an array of prior shapes
#we will use a t distribution (like a normal distribution but a bit broader)
t_prior <- student_t(df = 7, location = 0, scale = 2.5)

#now we can run the model
#a minimal example is just:
fit1 <- stan_glm(switch ~ dist100, 
                 data = wells, 
                 family = binomial(link = "logit"))

#but this uses a lot of default options
#I prefer to set some options manually
fit1 <- stan_glm(switch ~ dist100, 
                 data = wells,
                 family = binomial(link = "logit"),
                 prior = t_prior,
                 prior_intercept = t_prior,
                 chains=4, iter=1500,
                 cores = 2, 
                 seed = 12345)
#if you don't specify your priors, the model will assign them for you
#you can check them like this:
prior_summary(fit1)

#you can extract the stan code and look at what's going on
#but requires rstan which can be fiddly to install, I've commented it out for now
#library(rstan)
#get_stanmodel(fit1$stanfit)


#check rhat and convergence as we would for any other model
mcmc_trace(fit1)
bayesplot::rhat(fit1)
#looks good

#check posterior as well for fit
rstanarm::pp_check(fit1, plotfun = "error_binned", nreps=1) 
#binomial diagnostics can look weird, but again all we want is even errors around 0


#can also compare where your posterior ended up compared to our prior
posterior_vs_prior(fit1)
#looks like our priors were too vague if anything... but ok for now

#we can launch a fancy interface to look at other diagnostics
launch_shinystan(fit1, ppd = FALSE)
#not specific to stanarm, will work with most stan models!

#we can look at our 5 and 95 estimate
round(posterior_interval(fit1, prob = 0.9), 2)
#dist100 is below 0 for both so we can be 90% there is a correlation!

#let's visualise this, gives a spread of spread and distribution
plot(fit1, prob = 0.5)
plot(fit1, "hist")

#what else can we find out with stanarm?
summary(residuals(fit1)) # not deviance residuals but we expect them to be around normal


#let's plot our findings!

# Predicted probability as a function of x
#this allows us to make a probability curve from our model estimates
pr_switch <- function(x, ests) plogis(ests[1] + ests[2] * x)
# A function to slightly jitter the binary data
jitt <- function(x, ...) {
  geom_point(aes(x, ), position = position_jitter(height = 0.05, width = 0.1),
             size = 2, shape = 21, stroke = 0.2)
}
ggplot(wells, aes(x = dist100, y = switch, color = switch)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  jitt(x=wells$dist100) +
  stat_function(fun = pr_switch, args = list(ests = coef(fit1)),
                size = 2, color = "gray35")




#############
### LOAD AND RUN MODEL 2
#############

#we have a simple model, but maybe we are interested in more predictors
#let's add another predictor (arsenic)

#in stanarm, this is easy, we can keep all our parameters from above and just update the model

#now add arsenic as a predictor
fit2 <- update(fit1, formula = switch ~ dist100 + arsenic)
(coef_fit2 <- round(coef(fit2), 3))

#check rhat and convergence as we would for any other model
mcmc_trace(fit2)
bayesplot::rhat(fit2)
#looks good

#make a plot of the new results

# Quantiles
q_ars <- quantile(wells$dist100, seq(0, 1, 0.25))
q_dist <- quantile(wells$arsenic, seq(0, 1, 0.25))
base <- ggplot(wells) + xlim(c(0, NA)) +
  scale_y_continuous(breaks = c(0, 0.5, 1))
vary_arsenic <- base + geom_point(aes(x=arsenic, y=switch, color=switch),
                          position = position_jitter(width = 0, height = 0.05), shape = 1)
vary_dist <- base + geom_point(aes(x=dist100, y=switch, color=switch),
                          position = position_jitter(width = 0, height = 0.05), shape = 1)

pr_switch2 <- function(x, y, ests) plogis(ests[1] + ests[2] * x + ests[3] * y)
for (i in 1:5) {
  vary_dist <-
    vary_dist + stat_function(fun = pr_switch2, color = "gray35",
                              args = list(ests = coef(fit2), y = q_dist[i]))
  vary_arsenic <-
    vary_arsenic + stat_function(fun = pr_switch2, color = "gray35",
                                 args = list(ests = coef(fit2), x = q_ars[i]))
}
bayesplot_grid(vary_dist, vary_arsenic,
               grid_args = list(ncol = 2))

#we have plotted the 0, 25, 50, 75 and 100th percentile estimates
#I normally would not recommend using the 0 and 100th percentile as they can be large outliers
#this is just illustrative


#############
### MODEL COMPARISON
#############
#we have two models, but which is better?
#we can use BIC, WAIC or any similar metric to compare models much like AIC
#loo is commonly used and performs well in most circumstances, so let's compare
(loo1 <- loo(fit1))
(loo2 <- loo(fit2))
#LOO stands for "leave-one-out cross-validation". 
#A standard LOO would mean fitting the entire model -1 data point, 
#then seeing if it can predict that missing datapoint
#it then would repeat this for every data point
#this would take absolutely ages, so we cheat
#the PSIS (pareto-smoothing importance sampling) method allows approximate LOO 
#in a fraction of the time. So it's an approximate LOO! (but accurate for 99.99% of cases)


loo_compare(loo1, loo2)
#fit2 looks better than fit 1! (best performing model goes on top automatically), -ve indicates worse performance

#the scale on this can be tough to interpret but generally if the 
#elpd_diff/se_dif
#-71.7/12.1 = -5.93
#is +/-2 or less, then model performs nearly the same
#this is an arbitrary quantity to designate "improvement" but still useful

#in this case we say fit 2 is better and fit1 is substantially worse

#loo has other functions as a diagnostic
#check sensitivity of posterior to outliers
par(mfrow = 1:2, mar = c(5,3.8,1,0) + 0.1, las = 3)
plot(loo1, label_points = TRUE)
plot(loo2, label_points = TRUE)
#the pareto k shape shows proportional weight of data, we want it to stay between 0.7 and -0.7
#a failure on this assumption means there are massive outliers your model cannot account for
#in which case LOO will be an unreliable metric
