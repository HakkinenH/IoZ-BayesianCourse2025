

################################################################################
# SIMPLE BINARY (BERNOULLI) MODEL IN STAN (AND CMDSTANR)
################################################################################

#############
#DESCRIPTION
#In the following file we will:
#load our example data and format it so it can be fed into a stan model
#we have a binary response, and continuous predictors
#for this we will use a bernoulli distribution
#run a stan model using cmdstanr
#look at our diagnostics
#make some example outputs
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

#binomial model


#############
### LOAD AND EXPLORE DATA
#############

#the following is admission data from UCLA graduate programs
gre.data<-read.csv("Data/gre.csv")
#admit is our response variable, whether a candidate got in or not
#gre is "Graduate Record Examinations" score, the higher the better
# gpa is "Grade Point Average", the higher the better
#rank is the overall national rank of their undergraduate program

#we want to know if score actually predicts submission success

#make a couple of rough plots (with jitter so we can see what's going on)
ggplot(data = gre.data, aes(x = gpa, y = admit)) +
  geom_point(position = position_jitter(width = 0, height = 0.02), shape = 1)

ggplot(data = gre.data, aes(x = gre, y = admit)) +
  geom_point(position = position_jitter(width = 0, height = 0.02), shape = 1)

ggplot(data = gre.data, aes(x = rank, y = admit)) +
  geom_point(position = position_jitter(width = 0.02, height = 0.02), shape = 1)

#binary plots are pretty difficult to interpret by sight, try another way
aggregate(gre.data$gpa, by=list(factor(gre.data$admit)), FUN=mean) 
aggregate(gre.data$gre, by=list(factor(gre.data$admit)), FUN=mean) 
table(gre.data$rank, gre.data$admit)



#convert data to a set of lists so it can be fed into stan using the compose_data() function
model.data <- 
  gre.data %>%
  rename(ranking = rank) %>%
  compose_data()



#############
### LOAD AND RUN MODEL
#############

code_lin <- "Models/stan_bernoulli.stan"
writeLines(readLines(code_lin))
#note our priors are normal, which is convenient as our model relies on an underlying linear equation
#however, this will be converted to a binary scale using logit converter, so we need to be careful
#I chose some vaguely useful priors, but would normally look at this more carefully

#we can plot our priors in "real" life using the following
#the main important thing is the beta term, the wider the SD the less information
#and the more positive, or negative, the more informative
a <- rnorm(50, -1, 1.5)
b <- rnorm(50, 0.5, 1.0) #sample size, mean, sd

#can compare to less informative priors. You will see much sharper curves all over the place
#a <- rnorm(50, -1, 100)
#b <- rnorm(50, 0.5, 10)

logistic <- function(z) 1/(1 + exp(-z))
plot(NULL, xlim=c(0, 10), ylim=c(0,1), xlab = "x", ylab="p(y | x)",
     main = "Almost flat priors")
for (i in 1:50) {
  curve(logistic(a[i] + b[i]*x), add=T, col="grey", lwd=2)
}
#as you'd imagine, this is easier when all variables are on the same scale. But for now we make do...


# #run stan and prepare model
logit.model <- cmdstan_model(stan_file = code_lin)

# sample from our model
logit.fit <- logit.model$sample(
  data = model.data,
  iter_warmup = 500,
  iter_sampling = 1500,
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
logit.fit$diagnostic_summary(diagnostics = c("divergences", "treedepth"))

#get draws (samples from the posterior distribution)
fit.draws <- logit.fit$draws()

#because we have a lot of posterior predictor estimates, we have a lot of "y_rep" estimates
#so let's make a list of parrameters we are interested in
ppreds<-c("alpha", "beta[1]", "beta[2]", "beta[3]")
mcmc_trace(fit.draws, pars=ppreds)


#check rhat for convergence, rule of thumb it should be below 1.05
rhats <- bayesplot::rhat(logit.fit, pars=ppreds)
mcmc_rhat(rhats)




#############
### MODEL OUTPUT and PLOTS
#############

# print results
print(logit.fit)

#this is on a logit scale, so some work is required to intuit results
#for example our "intercept" is the admission probability for a student
# with 0 on the GRE, a GPA of 0, and an undergraduate school ranking of 0
plogis(-2.59) * 100 #7%

#if a student increases their GRE (beta[1]) by 1 point 
#then their admission probability increases by
plogis(0) * 100 #50%



logit.draws<-logit.fit$draws(format="df")
logit.coefs <- 
  select(logit.draws, c("alpha", "beta[1]", "beta[2]", "beta[3]")) %>%
  summarise(alpha = mean(alpha),
            beta_1 = mean(`beta[1]`),
            beta_2 = mean(`beta[2]`),
            beta_3 = mean(`beta[3]`))

#plot it! (note we jitter the data as it is binary and we want to see it)
ggplot(data = gre.data, aes(x = gpa, y = admit)) +
  geom_point(position = position_jitter(width = 0, height = 0.05), shape = 1) +
  stat_function(fun = function(x) plogis(logit.coefs$alpha + logit.coefs$beta_2 * x))

#probability of admittance slowly increases with GPA


#let's plot it again but with uncertainty estimate
#pick 100 random draws and plot them to show variance in prediction
plot <- 
  ggplot(data = gre.data, aes(x = gpa, y = admit)) +
  geom_point(position = position_jitter(width = 0, height = 0.05), shape = 1)

sims <- logit.fit$draws(format="df")
sims <-
  sims %>%
  mutate(n = row_number()) %>%
  sample_n(size = 100) # select a random sample of 100 values from the posterior

#the following is essentially an external "Generated Quantities" block, producing an external prediction using each run's parameter estimates
lines <- 
  purrr::map(1:100, function(i) stat_function(
    fun = function(x) plogis(as.numeric(sims[i, "alpha"]) + as.numeric(sims[i, "beta[2]"]) * x), 
    linewidth = 0.1, color = "gray"))


#plot our data (jittered), add our mean prediction line, and some random draws to show variance
plot <- plot + lines
plot <- 
  plot + stat_function(fun = function(x) plogis(logit.coefs$alpha + logit.coefs$beta_2 * x))
plot


#Alternatively, we could summarise this to 5 and 95 estimates, or another CI boundary



