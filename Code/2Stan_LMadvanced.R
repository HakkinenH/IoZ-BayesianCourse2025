

################################################################################
# MORE COMPLEX LINEAR MODEL IN STAN (AND CMDSTANR)
################################################################################

#############
#DESCRIPTION
#In the following file we will:
#load our example data and format it so it can be fed into a stan model
#load a more complex linear model with more parameters
#run a stan model using cmdstanr
#look at more advanced diagnostics
#make some more complex output plots

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

#our dataset has weights of wild lynxes, as well as recorded information
#which may predict their weight and health
lynxData<-read.csv("Data/LynxWeight.csv")


#weight decreases if lynx lives in an area of high human population density
ggplot(data = lynxData, aes(x = humanDens, y = weight)) +
  geom_point()

#can add a rough line of best fit
ggplot(data = lynxData, aes(x = humanDens, y = weight)) +
  geom_point() +
  geom_smooth(method = "lm")



#############
### LOAD AND RUN MODEL
#############

#in StanLM1 we formatted the data for stan manually
#the tidybayes package can do this automatically
#while we're here, we can centre variables
model.data <- 
  lynxData %>%
  mutate(kills_c = kills - mean(kills),
         speed_c = speed - mean(speed),
         humanDens_c = humanDens - mean(humanDens)) %>%
  select(weight, kills_c, speed_c, humanDens_c) %>%
  compose_data(.)
#kill is the number of kills made in the past 3 months
#speed is the highest recorded running speed of an individual
#humanDens is the mean human population density across an individual's range

#load the model
code_lin <- "Models/stan_lm2.stan"
writeLines(readLines(code_lin))
#note the more complex linear model
#and note the data variables match our model.data dataframe


#run stan
mod_lin <- cmdstan_model(stan_file = code_lin)


# sample from our model
linear.fit.1 <- mod_lin$sample(
  data = model.data,
  iter_warmup = 1000,
  iter_sampling = 2000,
  chains = 4,
  parallel_chains = 4,
  save_warmup = F,
  seed = 123,
  refresh = 500 # print update every 500 iterations
)


#############
### DIAGNOSTICS
#############

#we have to make sure the model converged properly
#then we will check our model actually fits the data properly!

#check some diagnostics
linear.fit.1$diagnostic_summary(diagnostics = c("divergences", "treedepth"))

# summarize our model
print(linear.fit.1)
#get draws (samples from the posterior distribution)
fit.draws <- linear.fit.1$draws()

#because we have a lot of posterior predictor estimates, we have a lot of "y_rep" estimates
#so let's make a list of parrameters we are interested in
ppreds<-c("alpha", "beta_k", "beta_sp", "beta_hu", "sigma")
mcmc_trace(fit.draws, pars=ppreds)


#check rhat for convergence, rule of thumb it should be below 1.05
rhats <- bayesplot::rhat(linear.fit.1, pars=ppreds)
mcmc_rhat(rhats)

#check number of effective parameters
#this is to do with the number of "independent" draws and checking a lack of autocorrelation
#the higher the number of effective parameters the better! 
#in general, if any ratio is under 0.1 then there's a problem
eff.ratio <- neff_ratio(linear.fit.1, pars=ppreds)
eff.ratio
mcmc_neff(eff.ratio)

#the following is optional, but this shows the autocorrelation between draws
#there will always be some, since models inform future draws off past ones
#but the faster it drops to 0 the better.
mcmc_acf(fit.draws, pars=ppreds)


#ok the model passes all the basic diagnostics we have tried
#but how well does it fit our data? How can we check?

#one very good check of model fit is to use a "posterior prediction check"
#can our model predict our original data, or are there systematic errors?
#similar conceptually to residual checks
#this is where our "generated quantities" block goes in handy. We have yrep for this exact purpose
y.rep <- linear.fit.1 |>
  as_draws_df() |>
  as_tibble() |>
  select(starts_with("y_rep"))

#we can compare a random model with our original data
plot(lynxData$weight, as.numeric(y.rep[1,]), 
     xlab="Actual response", ylab="predicted response")
abline(0,1)
#looks pretty good! no prediction is perfect but closely matches 0,1 line

dim(y.rep)
#we generated 8000 sample predictions of yrep. Some are good, some are not
#but we want to make sure they are ON AVERAGE a good fit.

#Simple option: what were the prediction errors from a single random model run
hist(lynxData$weight-as.numeric(y.rep[1,]))
#this should look like a normal distribution
#obviously this is not that helpful since it's just one draw!

#instead let's show on average we can predict our data
#(and optionally the 90th CI around these estimates)
#if it's bad we probably need to change our model design
posdf<-data.frame(y=lynxData$weight,
                  ypos_mean=apply(y.rep, 2, mean),
                  ypos_05=apply(y.rep, 2, quantile, 0.05),
                  ypos_95=apply(y.rep, 2, quantile, 0.95))

ymin=min(posdf$ypos_05)
ymax=max(posdf$ypos_95)

#in the following black points are the mean predicted data, and the red lines show uncertainty around it
#we want the 0,1 line to go through the middle, and the uncertainty to not be too large
plot(posdf$y, posdf$ypos_mean, ylim=c(ymin, ymax), pch=16)
points(posdf$y, posdf$ypos_05, col="red", pch="+")
points(posdf$y, posdf$ypos_95, col="red", pch="+")
segments(posdf$y, posdf$ypos_05, posdf$y,  posdf$ypos_95, col="red", lty=2)
abline(0,1)

#we can see the model fits well with low and medium values of y
#but overestimates a little at the upper end
#generally fine, but worth bearing in mind!

#the above section was a bit laborious, happily there are easier ways to check this
#with pre-made function!
y.rep<-as.matrix(y.rep)

#show a random sample of our draws
ppc_dens_overlay(y = lynxData$weight, yrep = y.rep[sample(nrow(y.rep),100), ])
#ideal fit is the y line going through the middle of all our predictions (blue)
#this is basically the same as our scatter plot before, but presented in a neater way!



#############
### MODEL OUTPUT and PLOTS
#############

#our model has passed our diagnostics. So let's look at output
# summarize our model
print(linear.fit.1)

#what can we learn from this? 
#beta_k is positive, in the mean, q5 and 95. So we can be 90% sure number of kills has a positive effect on weight
#beta_sp is close to 0 in the mean mean, q5 is negative and and 95 is positive. No evidence of effect of speed on weight
#beta_hu is negative, in the mean, q5 and 95. So we can be 90% sure it has a negative effect


#let's imagine we only really care about effect of local human density on lynx weight
#how likely is it that beta_hu (human density) has an effect greater than 0?
fit.draws<-linear.fit.1$draws(format="df")
mean(fit.draws[,"beta_hu"]>0)
#not at all likely!
#what about below 0?
mean(fit.draws[,"beta_hu"]<0)
#very likely!
#what about a slope less than -2?
mean(fit.draws[,"beta_hu"]< -2)
#~90%!

#after all that, let's make a line of best fit for the effect of human density on lynx weight
lynxData <- 
  lynxData %>%
  mutate(kills_c = kills - mean(kills),
         speed_c = speed - mean(speed),
         humanDens_c = humanDens - mean(humanDens)) 

fit.draws <- linear.fit.1$draws(format="df")
alpha<-unlist(fit.draws[,"alpha"])
beta_hu<-unlist(fit.draws[,"beta_hu"])

ggplot(data = lynxData, aes(x = humanDens_c, y = weight)) +
  geom_point() +
  stat_function(fun = function(x) mean(alpha) + mean(beta_hu) * x)



