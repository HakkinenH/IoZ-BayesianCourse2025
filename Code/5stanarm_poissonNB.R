
################################################################################
# SIMPLE COUNT MODELS IN STANARM
################################################################################

#############
#DESCRIPTION
#This file analyses count data with a poisson and negative binomial model
#uses the stanarm package
#stanarm uses stan, but is designed to look more like regulare LME packages
#it loads example data, runs a simple poisson model
#runs diagnostics and gives a simple output
#then we build some a second negative binomial model
#finally, we compare model fits and pick our best one
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

toolik_richness <- read.csv("Data/toolik_richness.csv")
#this dataset shows species richness has changed over time near Toolik Lake Field Station.
#Cover is the relative cover (out of 1) for different plant species, 
#Mean.Temp is the mean annual temperature at Toolik Lake Station and SD.Temp is the standard deviation of the mean annual temperature. 
#Treatment is another categorical variable that refers to different chemical treatments, e.g. some plots received extra nitrogen, others extra phosphorus. 
# Block and Plot give more detailed information about where the measurements were taken.

# Inspect data
head(toolik_richness)

#we should treat plot as categorical
toolik_richness$Plot <- as.factor(as.character(toolik_richness$Plot))

#what does our response variable (richness) look like?
(hist <- ggplot(toolik_richness, aes(x = Richness)) +
    geom_histogram() +
    theme_classic())
#definitely not normal, count data is often biased towards low numbers

#how many years? What is the lowest
unique(toolik_richness$Year)




#############
### LOAD AND RUN MODEL 1
#############

#start with a very simple model, is richness affected by year
#I() in this case is to force year to be treated as an integer, not a category
# Note how now we are using stan_glm because there are no random effects
stan_glm1 <- stan_glm(Richness ~ I(Year-2007),
                      data = toolik_richness, family = poisson,
                      chains = 4, cores = 4)

#I am a little lazy here and do not specify priors or iterations
#the model chooses for me but you should always check this yourself!

#you can check them like this:
prior_summary(stan_glm1)
#get stan model, note requires rstan to be installed which can be fiddly
stancode <- rstan::get_stancode(stan_glm1$stanfit)
cat(stancode)

summary(stan_glm1)

#did model converge ok?
plot(stan_glm1, plotfun = "trace")

#posterior prediction checks
pp_check(stan_glm1, plotfun = "stat", stat = "mean") #should be normally distributed with true y in the centre
pp_check(stan_glm1, plotfun = "dens_overlay")
#looks ok though not that close a match in the centre and at higher values

#let's look at fancy output (optionally)
launch_shinystan(stan_glm1)

#make a plot showing probability bands over time
(model_fit <- toolik_richness %>%
    data_grid(Year = seq_range(Year, n = 101)) %>%
    add_predicted_draws(stan_glm1) %>%
    ggplot(aes(x = Year, y = Richness)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),
                    alpha = 1/2, colour = "black") +
    geom_point(data = toolik_richness, colour = "darkseagreen4", size = 3) +
    scale_fill_brewer(palette = "Greys"))
model_fit



#############
### MODEL 2: Negative binomial
#############

#let's try a different distribution and see if it fits better

#in stanarm this is very easy, we keep the same setting from model 1 and just update the distribution
#use different distribution
stan_glm2 <- update(stan_glm1, family = neg_binomial_2)

# Check convergence & priors
plot(stan_glm2, plotfun = "trace")
summary(stan_glm2)
prior_summary(stan_glm2)

# Posterior Predictive Checks
pp_check(stan_glm2, plotfun = "stat", stat = "mean")
pp_check(stan_glm2, plotfun = "dens_overlay")

#plot the new model
(model_fit <- toolik_richness %>%
    data_grid(Year = seq_range(Year, n = 101)) %>%
    add_predicted_draws(stan_glm2) %>%
    ggplot(aes(x = Year, y = Richness)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),
                    alpha = 1/2, colour = "black") +
    geom_point(data = toolik_richness, colour = "darkseagreen4", size = 3) +
    scale_fill_brewer(palette = "Greys"))
model_fit

#############
### MODEL COMPARISON
#############

#we have two models, but which is better?
#we can use BIC, WAIC or any similar metric to compare models much like AIC
#loo is commonly used and performs well in most circumstances, so let's compare
loo1<-loo(stan_glm1); loo2<-loo(stan_glm2)
loo_compare(loo1, loo2)
#NB model is a lot better
#LOO stands for "leave-one-out cross-validation". 
#A standard LOO would mean fitting the entire model -1 data point, 
#then seeing if it can predict that missing datapoint
#it then would repeat this for every data point
#this would take absolutely ages, so we cheat
#the PSIS (pareto-smoothing importance sampling) method allows approximate LOO 
#in a fraction of the time. So it's an approximate LOO! (but accurate for 99.99% of cases)


#WAIC is also commonly used, but has some of the same reservations as AIC
#generally I recommend LOO even if it's a bit slower
#I only use WAIC if I have very large models and I want to do things quickly
stan_glm1$waic <- waic(stan_glm1)
stan_glm2$waic <- waic(stan_glm2)
loo_compare(stan_glm1, stan_glm2, criterion = "waic")


#loo has other functions as a diagnostic
#check sensitivity of posterior to outliers
par(mfrow = 1:2, mar = c(5,3.8,1,0) + 0.1, las = 3)
plot(loo1, label_points = TRUE)
plot(loo2, label_points = TRUE)
#the pareto k shape shows proportional weight of data, we want it to stay between 0.7 and -0.7
#a failure on this assumption means there are massive outliers your model cannot account for

