
################################################################################
# EXAMPLE MODELS IN BRMS
################################################################################

#############
#DESCRIPTION
#This file demonstrates a different stan-based package: brms
#this is a mid-ground between stanarm and regular stan
#it's very flexible, powerful, and comes with lots of useful functions. I recommend it!
#in this file we will load some data, make a few models based on a poisson distribution and make some plots
#we demonstrate some simple model diagnostics and comparisons
#############

#the following is adapted from: https://ourcodingclub.github.io/tutorials/brms/
#credit: Louise Litrico

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

# Load the data
France <- read_csv("Data/red_knot.csv")

head(France)  # to get the first observations in each column
str(France)  # what type of variables do we have

#for the purposes of this example we are interested in:
#pop: population count per site
#year: the year of record
#location: location of population


#years as they are are difficult to interpret in terms of priors and intercepts
#so create a standarised variable
France <- France %>% mutate(yearStd = I(year - 1975))
#I forces integer

#let's make some basic plots
(hist_france <- ggplot(France, aes(x = pop)) +
    geom_histogram(colour = "#8B5A00", fill = "#CD8500") +
    theme_bw() +
    ylab("Count\n") +
    xlab("\nCalidris canutus abundance") +  # latin name for red knot
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain")))   
#our response variable, pop, doesn't look that normal. It's count data so maybe a poisson distribution?

#let's try a straight-forward scatterplot to compare year and population
(point_year <- ggplot(France, aes(x=as.numeric(year), y=pop)) +
    geom_point() +
    theme_bw() +
    xlab("Year\n") +
    ylab("\nCalidris canutus abundance") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain")))  

#what about location and population
(boxplot_location <- ggplot(France, aes(Location.of.population, pop)) +
    geom_boxplot() +  # could be a significant effect between locations so should look at that
    theme_bw() +
    xlab("Location\n") +
    ylab("\nCalidris canutus abundance") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain")))  


###################################################################
#MAKE MODELS IN BRMS
###################################################################


#brms will fill in priors for us
#but we might want to know what they are
#we state our model structure and it will report back the defaults for priors
default_prior(pop ~ I(yearStd)+Location.of.population,
              data = France, family = poisson())
#note the I before year!
#flat prior for most, and a student t distribution for intercept


#not bad, but for demonstration purposes I will show how to make priors in brms
#let's set some manually
prior1 <- c(prior(prior = 'normal(0,6)', class='b', coef='IyearStd'), 	
            # global slope belongs to a normal distribution centered around 0
            prior(prior = 'normal(0,20)', class='Intercept', coef=''))
#otherwise we can let BRMS decide, but always try to double check

#brms is great because you can feed it basic info and it will design a model for you
#OR you can specify your own
#I always recommend looking at the model structure before running it
#and customising it in a .stan file if needed (see the linear regression files on what these are)


# we can check what the model will look like in Stan, very useful for customisation
# verify that the priors indeed found their way into Stan's model code
stancode(pop ~ I(yearStd) + Location.of.population, data = France,
         family = poisson(), chains = 3, prior = prior1,
         iter = 3000, warmup = 1000)
#a bit more complex that some we have looked at, but mostly the same!


#with all that said, let's run a model
france1_mbrms <- brms::brm(pop ~ I(yearStd),
                           data = France, family = poisson(), chains = 3,
                           prior=prior1,
                           iter = 3000, warmup = 1000, refresh=500)
#can check the priors were used successfully
prior_summary(france1_mbrms)


#I may want to run several models and compare fit, let's do it again but fit year as a random effect
#and as a linear effect
france2_mbrms <- brms::brm(pop ~ I(yearStd) + (1|year),
                           data = France, family = poisson(), chains = 3,
                           iter = 3000, warmup = 1000)
stancode(france2_mbrms)
prior_summary(france2_mbrms)


#and for model 3, let's add location instead
france3_mbrms <- brms::brm(pop ~ I(yearStd) + Location.of.population,
                           data = France, family = poisson(), chains = 3,
                           iter = 3000, warmup = 1000)
stancode(france3_mbrms)

# saveRDS(france1_mbrms, "france1_mbrms.RDS")
# you can save the model as an RDS (Rdata) that way you don't need to run the model again if you come back to this code


#you can summarise information in different ways with bayesian models
#the standard is the fixed effect way
#but note the intercept sd estimate! That's how much the intercept varies between years
summary(france2_mbrms)
fixef(france2_mbrms) # to get more detailed values for estimates
coef(france2_mbrms) # if you have group-level effects (hierarchical data)



#############
### DIAGNOSTICS
#############


#let's double check all the usual diagnostics for each model
plot(france1_mbrms)
pp_check(france1_mbrms)  # posterior predictive checks
#as always do the convergence plots look "fuzzy" do the parameter estimates look unimodal?
#do the posterior checks look sensible?

#same for model 2
summary(france2_mbrms)
plot(france2_mbrms)
pp_check(france2_mbrms, ndraws=50)

#same for model 3
summary(france3_mbrms)
plot(france3_mbrms)
pp_check(france3_mbrms)




#############
### MODEL COMPARISON
#############

#we have two models, but which is better?
#we can use BIC, WAIC or any similar metric to compare models much like AIC
#loo is commonly used and performs well in most circumstances, so let's compare
loo(france1_mbrms,france2_mbrms, france3_mbrms, compare = TRUE)
#france3_mbrms is easily the best, but we get some warning about pareto_k 

#loo has other functions as a diagnostic
#check sensitivity of posterior to outliers
plot(loo(france1_mbrms), label_points = TRUE)
plot(loo(france2_mbrms), label_points = TRUE)
plot(loo(france3_mbrms), label_points = TRUE)
#the pareto k shape shows proportional weight of data, we want it to stay between 0 and 0.7
#a failure on this assumption means there are massive outliers your model cannot account for
#and that loo is likely unreliable

#often this implies outliers have a great deal of influence, or we have overfitted our model
#our pp_check looks good, so why has it failed?
#in this case I believe it is because of the poisson distribution, it is very inflexible and small changes in data can have drastic implications on fit
#most likely they don't matter in this case, but
#in real life I would try other distributions
france4_mbrms <- brms::brm(pop ~ I(yearStd) + Location.of.population,
                           data = France, family = negbinomial(), chains = 3,
                           iter = 3000, warmup = 1000)
loo4<-loo(france4_mbrms); plot(loo4, label_points = TRUE)
#much better!
#but for now we will pretend the loo warning don't happen and accept france3_mbrms as the best model



#make a plot with posterior fit between year and abundance
(model_fit <- France %>%
    add_predicted_draws(france3_mbrms) %>%  # adding the posterior distribution
    ggplot(aes(x = year, y = pop)) +  
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),  # regression line and CI
                    alpha = 0.5, colour = "black") +
    geom_point(data = France, colour = "darkseagreen4", size = 3) +   # raw data
    scale_fill_brewer(palette = "Greys") +
    ylab("Calidris canutus abundance\n") +  # latin name for red knot
    xlab("\nYear") +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position.inside = c(0.15, 0.85)))

#make a plot with posterior fit between year and abundance, separate by location
(location_fit <- France %>%
    group_by(Location.of.population) %>%
    add_predicted_draws(france3_mbrms) %>%
    ggplot(aes(x = year, y = pop, color = ordered(Location.of.population), fill = ordered(Location.of.population))) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50), alpha = 1/4) +
    geom_point(data = France) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    theme_bw() +
    ylab("Calidris canutus abundance\n") +
    xlab("\nYear") +
    theme_bw() +
    theme(legend.title = element_blank()))

