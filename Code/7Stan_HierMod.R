################################################################################
# SIMPLE HIERARCHICAL MODEL IN STANARM
################################################################################

#############
#DESCRIPTION
#In the following file we will:
#load our example data and format it so it can be fed into a stan model
#make a linear model with several parameters in cmdstanr/stan
#then we will make a hierarchical model, where we add several levels to the model
#this is conceptually similar to a random effect, but more flexible
#hierarchical models are often referred to as random effect models, pooled models, multi-level models etc.
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

sbdf<-read.csv("Data/SeabirdDisease.csv")
head(sbdf)
#The data contain information on 97 seabirds who ate toxic herring following a heavy metal dump. 
#The response variable here is antm - the log of the detected metal concentration from blood samples. 
#We want to see whether this variable can be predicted from the others. The other key variables we will use are:

#log_f, the log volume of contaminated food found in stomach
#weight, the the weight of the bird
#stage, the observed health state when the bird was brought in for treatment (grade goes 6-9). A higher value is more serious

#save the response variable for shiny diagnostics
yreal<-sbdf$antm


#get it into appropriate format for stan using compose_data()
#also centralise all predictor variables
model.data <- 
  sbdf %>%
  mutate(weight = weight - mean(weight),
         log_f = log_f - mean(log_f),
         stage = stage - mean(stage)) %>%
  select(antm, weight, log_f, stage) %>%
  compose_data(.)



###################################################################
#MODEL 1: NORMAL LINEAR MODEL
###################################################################

#load our basic linear model
#note the priors and the variable names
code_lin <- "Models/LinearModel4.stan"
writeLines(readLines(code_lin))


#run stan
mod_lin <- cmdstan_model(stan_file = code_lin)
# sample from our model
mrun1 <- mod_lin$sample(
  data = model.data,
  iter_warmup = 500,
  iter_sampling = 1500,
  chains = 4,
  parallel_chains = 4,
  save_warmup = F,
  seed = 123,
  refresh = 500 # print update every 500 iterations
)

#do our usual diagnostic checks
mrun1$diagnostic_summary(diagnostics = c("divergences", "treedepth"))

#create a list of pars we want to inspect
parlist<-c("alpha", "beta_1", "beta_2", "beta_3", "sigma")

#check rhat again, rule of thumb it should be below 1.05
rhats <- bayesplot::rhat(mrun1, pars=parlist)
mcmc_rhat(rhats)

#do all chains converge to the same values?
bayesplot::mcmc_trace(mrun1$draws(), pars=parlist)
#reminder beta_1 is the slope estimate for log_f
#reminder beta_2 is the slope estimate for weight
#reminder beta_3 is the slope estimate for stage

#how are our post posterior checks
y.rep <- mrun1 |>
  as_draws_df() |>
  as_tibble() |>
  select(starts_with("y_rep"))
#with pre-made function
y.rep<-as.matrix(y.rep)

#show a random sample of our draws
ppc_dens_overlay(y = sbdf$antm, yrep = y.rep[sample(nrow(y.rep),100), ])
#our true data should go through the middle (ideally)

#we can visualise another way if we like!
antm_sim_summary = apply(y.rep, 2, 'quantile', probs = c(0.025, 0.5, 0.975))
plot(sbdf$antm, antm_sim_summary[2,], 
     xlab = 'True lpsa values', 
     ylab = 'Posterior predicted lpsa values',
     ylim = range(antm_sim_summary))
for(i in 1:ncol(antm_sim_summary)) {
  lines(c(sbdf$antm[i],sbdf$antm[i]),
        c(antm_sim_summary[1,i],antm_sim_summary[3,i]))
}
abline(a = 0, b = 1, col = 'red')


#all looks good in terms of model fit
#if you really want to look at everything then launch shiny
launch_shinystan(mrun1)

#now we can look at parameter estimates, what are mean parameter estimates?
mrun1$summary()
#we have our main parameter estimates (alpha, beta1, 2, 3), plus a load of generated posterior quantities (y_rep)

#we can plot parameters and see the range of answers we got
mcmc_dens_overlay(mrun1$draws(), pars=parlist)

fit_optim <- mod_lin$optimize(data = model.data, seed = 123)
fit_optim$summary()



###################################################################
#MODEL 2: SIMPLE HIERARCHICAL MODEL
###################################################################

#alright but what if we don't want stage as an additional effect, but instead as a hierarchical effect
#this is conceptually equivalent to a random effect. We accept there is a global mean effect, 
#but within there there is variation between groups of stage
#so we allow the intercept (alpha) to vary by stage value

#we add a new level to the model. The global, "on average" intercept and then multiple sub-intercepts within that
#this is a "hierarchical" effect, very similar to a random effect in a frequentist framework
#the global intercept has a prior, but with an additional level of detail that states how "sub-intercepts" vary from the global
#this second level of parameters and priors are known as "hyperparameters"
#this is a simple example, but this is a very flexible approach, you can add more sub-levels, nested effects, random slopes etc!


#prepare the data for a hierarchical model
#note we are going to change stage to an index for its use as a HE
modelHC.data <- 
  sbdf %>%
  mutate(log_f = log_f - mean(log_f),
         weight = weight - mean(weight),
         stage_ind = stage-5) %>%
  select(antm, log_f, weight, stage_ind) %>%
  compose_data(.)
#because we want to have a random/hierarchical effect we need one more thing
#for stan we need to state how many levels of stage there are
modelHC.data$N_stage<-4



#load HM code and view it
code_lin <- "Models/LinearModel_HM.stan"
writeLines(readLines(code_lin))
#note our intercept parameter (alpha) is more complicated now
#it varies by sigma_alpha across categories of stage
#equivalent to a random intercept model

#run stan
mod_hc <- cmdstan_model(stan_file = code_lin)
# sample from our model
mrunHC <- mod_hc$sample(
  data = modelHC.data,
  iter_warmup = 500,
  iter_sampling = 1500,
  chains = 4,
  parallel_chains = 4,
  save_warmup = F,
  seed = 123,
  refresh = 500 # print update every 500 iterations
)

#do our usual diagnostic checks
mrunHC$diagnostic_summary(diagnostics = c("divergences", "treedepth"))

#create a list of pars we want to inspect
parlist1<-c("alpha", "beta_1", "beta_2", "sigma")
parlist2<-c("alpha[1]","alpha[2]","alpha[3]","alpha[4]", "beta_1", "beta_2", "sigma")
#note that there is both a GLOBAL intercept estimate (alpha) and 
# one for each group of stage (alpha_1, _2 etc.)
#SO stage is not a fixed effect! There is a global alpha estimate and then variations within this per group
#NOT four separate estimates
#this is therefore a hierarchical estimate, and functionally the same as a random effect
#more accurately: random effects are a specific type of hierarchical effect
#hierarchical models (aka multilevel models, nested models etc), are incredibly flexible


#check rhat again, rule of thumb it should be below 1.05
rhats <- bayesplot::rhat(mrunHC, pars=parlist1)
mcmc_rhat(rhats)

#do all chains converge to the same values?
bayesplot::mcmc_trace(mrunHC$draws(), pars=parlist2)
#reminder beta_1 is the slope estimate for log_f
#reminder beta_2 is the slope estimate for weight
#individual estimates of alpha per level of stage


#take the posterior predictions for y_rep
y.repHC <- mrunHC |>
  as_draws_df() |>
  as_tibble() |>
  select(starts_with("y_rep"))
#with pre-made function
y.repHC<-as.matrix(y.rep)

#how well could we predict our real world data?
#show a random sample of our draws
ppc_dens_overlay(y = sbdf$antm, yrep = y.repHC[sample(nrow(y.repHC),100), ])
#our true data should go through the middle (ideally)


#all looks good in terms of model fit
#if you really want to look at everything then launch shiny
launch_shinystan(mrunHC)

#now we can look at parameter estimates, what are mean parameter estimates?
mrunHC$summary()


#we can plot parameters and see the range of answers we got
mcmc_dens_overlay(mrunHC$draws(), pars=parlist2)

fit_optim <- mod_hc$optimize(data = modelHC.data, seed = 123)
fit_optim$summary()


###################################################################
# MODEL COMPARISONS
###################################################################


#so which of these models are better?

#there are many statistics we can use to compare models
#BIC, WAIC etc.
#but leave-one-out cross-validation (LOO) is the most popular (and probably accurate)
#it fits your data point by point to your model, and measures log-likelihood error rates
#this used to be very slow but happily is now very much optimised!
#loo now approximates through PSIS, but is still very accurate
loo1 <- loo(mrun1$draws("y_rep"))
looHC <- loo(mrunHC$draws("y_rep"))
loo_compare(loo1, looHC)
loo_model_weights(list("LinearModel" = loo1, "HierModel" = looHC))

#linear model is slightly better! But very similar in terms of performance
#the scale on this can be tough to interpret but generally if the 
#elpd_diff/se_dif
#-0.7/1.7 = -0.41
#is +/-2 or less, then model performs nearly the same
#this is an arbitrary quantity to designate "improvment" but still useful


#loo has other functions as a diagnostic
#check sensitivity of posterior to outliers
plot(loo1)
plot(looHC)
#the pareto k shape shows proportional weight of data, we want it to stay between 0 and -0.7
#a failure on this assumption means there are massive outliers your model cannot account for

