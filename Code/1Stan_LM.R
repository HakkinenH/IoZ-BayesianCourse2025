

################################################################################
# SIMPLE LINEAR MODEL IN STAN (AND CMDSTANR)
################################################################################

#############
#DESCRIPTION
#In the following file we will:
#load our example data and format it so it can be fed into a stan model
#load a simple linear model
#run a stan model using cmdstanr
#look at basic diagnostics
#make some output plots
#these are the data and model presented in the workshop
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



#############
### LOAD AND EXPLORE DATA
#############

#load our data frame looking at the recorded sizes of wildlife around Noddy Sands
data_nod <- read.delim("data/NoddyMutants_TS.csv", sep = ";")

head(data_nod)
#we have one predictor, year
summary(data_nod$year)
#running from 1952 to 2022

#all other variables are our responses. A variety of giant woodland animals
#for now we only care about lengthCent_m (length of centipedes)

#prepare the data for stan
#a peculiarty of stan is that it needs data as a set of lists, NOT as a dataframe
#so we have to prepare the data
data_lin <-list(N = nrow(data_nod),
                x = data_nod$year,
                xpred = 2016,
                y = data_nod[,5])
#N(or n) is the number of rows in the data. All stan models require this to be declared
#all other data variable names are flexible, BUT they have to match the names in our model (see code_lin later)


#rescale data!
#in general I strongly recommend scaling your predictors
#this either means correcting by mean, OR setting the lowest value to 0
#this makes some things harder (years will now be "years since 1952")
#but makes understanding intercepts and slopes MUCH easier
data_lin$x<-data_lin$x-1952


#visualise data, what do we expect to see
ggplot() +
  geom_point(aes(x, y), data = data.frame(data_lin), size = 1) +
  labs(y = 'Av. centipede length (m)', x= "Year") +
  guides(linetype = "none")



#############
### LOAD AND RUN MODEL
#############

#you can declare models inside R, but I often save them in separate files
#they are not written in R code, but native stan.
code_lin <- "Models/stan_lm1.stan"
writeLines(readLines(code_lin))
#look over the model, do you understand what these sections mean?
#can you spot the linear equation?
#can you spot the priors? what are they?
#the names of parameters in here need to match our data set (data_lin)


#now we have the model, the data, let's build the model!
#the following line compiles the model so it is ready to run
#take a minute as it is converting our text model in an executable stan model
mod_lin <- cmdstan_model(stan_file = code_lin)
#NOTE: occasionally R will lose the location of the cmdstan, by default it will be be ~Home (for me this is "C:/Users/[username]/")
#if you get this error, find the .cmdstan folder and run this line
#set_cmdstan_path(path = "[path to].cmdstan/cmdstan-2.xx.x")


#now we run the model with our data
fit_lin <- mod_lin$sample(
  data = data_lin,
  iter_warmup = 500, #how many iterations do we want for warm-up?
  iter_sampling = 1500, #how many iterations after warm-up do we want to sample the posterior?
  chains = 4, #how many chains?
  parallel_chains = 4, #run in parallel or not? Can be turned to 1 if you don't have parallel enabled
  save_warmup = F, #do we want to save warmup estimates? Generally no, but can be useful for diagnostics
  seed = 123, #MC models will pick random starting points, this makes it replicable. optional for most purposes
  refresh = 500 # print update every 500 iterations
)



#############
### DIAGNOSTICS
#############

#we need to check if the model had any errors, and if it converged properly

#check some diagnostics
fit_lin$diagnostic_summary(diagnostics = c("divergences", "treedepth"))
#what does the above mean?
#each chain is checked for some common diagnostics
#divergence is number of divergences per chain 
  # this indicates that sample MC is not fully able to explore the posterior distribution, and has got "stuck".
  # will often appear as a "gap" in the fuzzy caterpillar diagnostic traceplot
#tree depth: is the number of time treedepth hit its max per chain
  # MC will change parameter estimates by steps trying to find an improvement in model fit
  # if it can't find one, it will U-turn and go the other way
  # how far it goes before giving up is the "tree depth", hitting the limit a lot indicates parameter space cannot be adequately explored
  # and you should look at the traceplot to make sure estimates have converged properly
#if they are all 0 then you're good to go

#if you get warnings in your model then check Rhat and convergence and parameter estimate plots
#if these look ok you may be able to carry on anyway!
#max tree depth warnings are usually an issue with efficiency, not model fit. They are warnings, not a "game-over"!


#check the trace plots, do our model converge after burn in?
#do all chains converge to the same values?
bayesplot::mcmc_trace(fit_lin$draws(), pars=c("alpha", "beta", "sigma"))
#how much of a fuzzy caterpillar is it?


#############
### MODEL OUTPUT
#############

#If the diagnostics pass, we can move on
#now we can look at parameter estimates, what are mean parameter estimates?
fit_lin$summary()
#we see our intercept (alpha), effect of year (beta), and error term (sigma)
#the various "mu" parameters are for checking model fit, then can be ignored for now
#lp_ is the logarithm of the (unnormalized) posterior density. Used for a variety of model fit metrics, not easy to interpet by itself
#When looking at output I always look at mean and median first of parameters of interest.
#then: what are the CIs? What can we conclude from this?


#the the above is difficult to interpret except in a crude way
#we can plot parameters and see the range of answers we got
mcmc_dens_overlay(fit_lin$draws(), pars=c("alpha", "beta", "sigma"))
#hopefully all chains are similar and there is only one peak

#alternatively we can directly compare parameters and see how they correlate between draws
draws_lin <- fit_lin$draws(format = "df")
draws_lin |>
  mcmc_scatter(pars=c("alpha","beta"))
#so generally the higher the intercept, the lower the slope estimate
#makes sense. The true answer is likely in the middle, but how can we work it out

#if we want a quick summary, we can use the above summary, or take mean, 
#OR we can make work out what the best model looks like
#all models are compared and optimized based on (penalized) maximum likelihood estimate (MLE)
fit_optim <- mod_lin$optimize(data = data_lin, seed = 123)
fit_optim$summary()
#Bayesian models don't give point estimates like frequentist models
#and mean estimates are useful, but useless without CI info around them
#MLE estimates are better, especially for simple visualisations
#but in general I recommend always reporting mean, 5th and 95th percentile estimates
#in the case of a normally distributed parameter estimate, we want to see if our 5-95th estimate crosses 0
#if no, then we can be 95% confident the parameter is not 0! Not the same as significance, but a useful equivalent benchmark


#what proportion of posterior models find that beta (slope) is above 0?
#this is proportional to the probability that beta>0!
mean(draws_lin[,"beta"]>0)
#~100% of models! So we can say with 100% certainty the slope is positive (rare!)


#we can plot every estimate on a plot
#note: might take a minute, we are plotting a line per iteration
all <- draws_lin |>
  as_draws_df() |>
  as_tibble() |>
  select(starts_with("mu")) |>
  t() %>%
  data.frame(x = data_lin$x, .) |>
  pivot_longer(!x, names_to="run", values_to="y")

#if we plot all of them it's a mess (we ran thousands of iterations), so let's take 500 random ones
#rerun it if you want to see a different set!
sam<-sample(unique(all$run), 500, replace=F)
allsample<-all[all$run %in% sam,]

#remember each line is a single estimate from the MC process
#some are not that good, but they AVERAGE out to a (nearly) perfect solution
ggplot(allsample, aes(x = x, y = y)) + 
  geom_point(aes(x, y), data = data.frame(data_lin), size = 1) +
  geom_line(aes(group = run), color="grey", linewidth=0.8, alpha=0.1)+
  labs(y = 'Av. centipede length (m)', x= "Year")


#interesting but quite messy, let's summarise it
#let's make a mean line and a 95th percentile CI line
mu <- draws_lin |>
  as_draws_df() |>
  as_tibble() |>
  select(starts_with("mu")) |>
  apply(2, quantile, c(0.05, 0.5, 0.95)) |>
  t() %>%
  data.frame(x = data_lin$x, .) |>
  pivot_longer(!x, names_to="pct", values_to="y")

ggplot() +
  geom_point(aes(x, y), data = data.frame(data_lin), size = 1) +
  geom_line(aes(x, y, linetype = pct), data = mu, color = 'red') +
  scale_linetype_manual(values = c(2,1,2)) +
  labs(y = 'Av. centipede length (m)', x= "Year") +
  guides(linetype = "none")
#this is our mean estimate and our 5-95th percentile estimate
#very clearly positive across our chosen probability band

#so we can conclude centipedes are getting longer over time!


