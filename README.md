
Code contains example models and is the most relevant folder for most people. Read the descriptions below and choose files you are interested in.
I recommend starting with 1 or 2 as they contain more information on basic aspects of stan and Bayesian modelling

Model folder contains .stan files which specify the model. They can be opened in any text editor
Data is just example data for use in code files


#0Setup.R
Directory checks, installs and dependencies

#1Stan_LM.R
In the following file we will:
-load our example data and format it so it can be fed into a stan model
-load a simple linear model
-run a stan model using cmdstanr
-look at basic diagnostics
-make some output plots
-these are the data and model presented in the workshop

#2Stan_LMadvanced.R
-load our example data and format it so it can be fed into a stan model
-load a more complex linear model with more parameters
-run a stan model using cmdstanr
-look at more advanced diagnostics
-make some more complex output plots


#3Stan_bernoulli.R
-load our example data and format it so it can be fed into a stan model
-we have a binary response, and continuous predictors
-for this we will use a bernoulli distribution
-run a stan model using cmdstanr
-look at our diagnostics
-make some example outputs


#4stanarm_binomial1.R
-This file uses a similar model to stan_bernoulli
-but uses the stanarm package
-this uses stan, but is designed to look more like regular LME packages
-it loads example data, runs a simple binomial model
-runs diagnostics and gives a simple output
-then we build some more complex models and do some model comparison


#5stanarm_poissonNB.R
-This file analyses count data with a poisson and negative binomial model
-uses the stanarm package
-stanarm uses stan, but is designed to look more like regulare LME packages
-it loads example data, runs a simple poisson model
-runs diagnostics and gives a simple output
-then we build some a second negative binomial model
-finally, we compare model fits and pick our best one


#6BRMS_example.R
-This file demonstrates a different stan-based package: brms
-this is a mid-ground between stanarm and regular stan
-it's very flexible, powerful, and comes with lots of useful functions. I recommend it!
-in this file we will load some data, make a few models based on a poisson distribution and make some plots
-we demonstrate some simple model diagnostics and comparisons


#7Stan_HierMod.R
-load our example data and format it so it can be fed into a stan model
-make a linear model with several parameters in cmdstanr/stan
-then we will make a hierarchical model, where we add several levels to the model
-this is conceptually similar to a random effect, but more flexible
-hierarchical models are often referred to as random effect models, pooled models, multi-level models etc.

