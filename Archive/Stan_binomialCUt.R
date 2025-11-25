
#binomial model


#############
### LOAD AND EXPLORE DATA
#############

gre.data <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

glimpse(gre.data)


code_lin <- "Models/stan_binary2.stan"
writeLines(readLines(code_lin))

admit.data <- as.data.frame(UCBAdmissions)


model.data <- 
  admit.data %>%
  select(Admit, Gender, Freq) %>%
  rename(admit = Admit,
         gender = Gender,
         applications = Freq) %>%
  compose_data()

# estimate
bin.model <- cmdstan_model(stan_file = code_lin)
bin.fit <- bin.model$sample(data = model.data, refresh = 0)

print(bin.fit)

#check some diagnostics
bin.fit$diagnostic_summary(diagnostics = c("divergences", "treedepth"))

#check the trace plots, do our model converge after burn in?
#do all chains converge to the same values?
bayesplot::mcmc_trace(bin.fit$draws(), pars=c("alpha", "beta[1]"))


#now we can look at parameter estimates, what are mean parameter estimates?
bin.fit$summary()

plogis(0.383)

#the the above is difficult to interpret except in a crude way
#we can plot parameters and see the range of answers we got
mcmc_dens_overlay(bin.fit$draws(), pars=c("alpha", "beta[1]"))


#alternatively we can directly compare parameters and see how they correlate between draws
draws_bin <- bin.fit$draws(format = "df")
draws_bin |>
  mcmc_scatter(pars=c("alpha","beta[1]"))
#so generally the higher the intercept, the lower the slope estimate
#makes sense. The true answer is likely in the middle, but how can we work it out

#if we want a quick summary, we can use the above summary, or take mean, 
#OR we can make work out what the best model looks like
#all models are compared and optimized based on (penalized) maximum likelihood estimate (MLE)
fit_optim <- bin.model$optimize(data = model.data, seed = 123)
fit_optim$summary()


bin.draws<-bin.fit$draws(format="df")
bin.coefs <- 
  select(bin.draws, c("alpha", "beta[1]")) %>%
  summarise(alpha = mean(alpha),
            beta_1 = mean(`beta[1]`))

#plot it! (note we jitter the data as it is binary and we want to see it)
ggplot(data = gre.data, aes(x = gpa, y = admit)) +
  geom_point(position = position_jitter(width = 0, height = 0.05), shape = 1) +
  stat_function(fun = function(x) plogis(logit.coefs$alpha + logit.coefs$beta_2 * x))

#probability of admittance slowly increases with GPA


#let's plot it again but with uncertainty estimate
#pick 100 random draws and plot them to show variance in prediction
plot <- 
  ggplot(data = admit.data, aes(x = Gender, y = Admit)) +
  geom_point(position = position_jitter(width = 0, height = 0.05), shape = 1)

plot <- 
  ggplot(data = admit.data, aes(x = Gender, y = Admit)) +
  geom_point(position = position_jitter(width = 0, height = 0.05), shape = 1)


sims <- bin.fit$draws(format="df")
sims <-
  sims %>%
  mutate(n = row_number()) %>%
  sample_n(size = 100) # select a random sample of 100 values from the posterior

#male is "1" in this model
#female is "2"

m_points<-purrr::map(1:100, function(i) plogis(as.numeric(sims[i, "alpha"]) + as.numeric(sims[i, "beta[1]"]) * 1))
f_points<-purrr::map(1:100, function(i) plogis(as.numeric(sims[i, "alpha"]) + as.numeric(sims[i, "beta[1]"]) * 2))



mdf<-data.frame("Gender"="Male", Admit=unlist(m_points), ind=1:length(m_points))
fdf<-data.frame("Gender"="Female", Admit=unlist(f_points), ind=1:length(m_points))
pdf<-rbind(mdf, fdf)

boxplot(Admit~Gender, data=pdf)
  
  ggplot(pdf, aes(Gender,Admit)) +
  geom_point(aes(color=ind, size=3)) +
  geom_line(aes(group = ind))

mlines <- 
  purrr::map(1:100, function(i) stat_function(
    fun = function(x) plogis(as.numeric(sims[i, "alpha"]) + as.numeric(sims[i, "beta[1]"]) * 1), 
    linewidth = 0.1, color = "gray"))
flines <- 
  purrr::map(1:100, function(i) stat_function(
    fun = function(x) plogis(as.numeric(sims[i, "alpha"]) + as.numeric(sims[i, "beta[1]"]) * 2), 
    linewidth = 0.1, color = "gray"))


plot <- plot + lines

m_est<-stat_function(fun = function(x) plogis(logit.coefs$alpha + logit.coefs$beta_1 * 1))
f_est<-stat_function(fun = function(x) plogis(logit.coefs$alpha + logit.coefs$beta_1 * 2))


plot
