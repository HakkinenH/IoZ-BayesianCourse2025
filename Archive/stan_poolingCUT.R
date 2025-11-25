
data(radon)

library(lme4)

library(tidyverse)
library(broom.mixed)
library(ggpubr)

# fit the model
partial.pooling.fit <- lmer(formula = log_radon ~ floor + (1 | county),
                            data = radon)

# print results
tidy(partial.pooling.fit, conf.int = TRUE)

#there is no real separation between fixed and random effects in a Bayesian framework
#but if we have nested or pooled categories, we can model that!
#in this case we will use something called partial pooling


code_lin <- "Models/Stan_pooling.stan"
writeLines(readLines(code_lin))

# prepare data for stan
model.data <-
  radon %>%
  rename(vfloor = floor) %>%
  dplyr::select(log_radon, vfloor, county) %>%
  compose_data()

# fit stan model

library(rstan)
stan(code_lin,
     data=model.data,
     refresh=200)

partial.pooling.model <- cmdstan_model(stan_file = code_lin)
stan.partial.pooling.fit <- partial.pooling.model$sample(data = model.data,
                                                         refresh = 0)

rstan.stan.partial.pooling.fit.02<-stan.partial.pooling.fit$draws(format="df")
tidy(rstan.stan.partial.pooling.fit.02, conf.int = TRUE)

rstan.stan.partial.pooling.fit.02 %>%
  spread_draws(alpha[county], b_floor, b_uranium, sigma_a, sigma_y)# extract samples in tidy format
  

stan.partialpool.02.values <-
  stan.partial.pooling.fit %>% # use this model fit
  recover_types(radon) %>% # this matches indexes to original factor levels
  spread_draws(alpha[county], b_floor, b_uranium, sigma_a, sigma_y) %>% # extract samples in tidy format
  median_hdci() # calculate the median HDCI

# plot the model results for the median estimates
ggplot(data = radon, aes(x = floor, y = log_radon, group = county)) +
  geom_abline(data = stan.pooling.values,
              aes(intercept = alpha, slope = beta), color = "black") +
  # no pooling estimates in red
  geom_abline(data = stan.nopool.values,
              aes(intercept = alpha, slope = beta), color = "red") +
  # partial pooling estimates in blue
  geom_abline(data = stan.partialpool.values,
              aes(intercept = alpha, slope = beta), color = "blue") +
  geom_abline(data = stan.partialpool.02.values,
              aes(intercept = alpha, slope = b_floor), color = "purple", lty = 2) +
  geom_point(alpha = 0.3,
             size = 3,
             position = position_jitter(width = 0.1, height = 0.2)) +
  facet_wrap(~ county) +
  scale_color_manual(values = c("No Pooling", "Pooling")) +
  ggpubr::theme_pubr()

# plot the model results with posterior samples
stan.partialpool.02.samples <-
  stan.partial.pooling.model.02 %>% # use this model fit
  recover_types(radon) %>%
  spread_draws(alpha[county], b_floor, b_uranium, sigma_a, sigma_y, n = 20) # extract 20 samples in tidy format

ggplot() +
  geom_abline(data = stan.pooling.samples,
              aes(intercept = alpha, slope = beta),
              color = "black",
              alpha = 0.3) +
  geom_abline(data = stan.nopooling.samples,
              aes(intercept = alpha, slope = beta),
              color = "red",
              alpha = 0.3) +
  geom_abline(data = stan.partialpool.samples,
              aes(intercept = alpha, slope = beta),
              color = "blue",
              alpha = 0.3) +
  geom_abline(data = stan.partialpool.02.samples,
              aes(intercept = alpha, slope = b_floor),
              color = "purple",
              lty = 2,
              alpha = 0.3) +
  geom_point(data = radon,
             aes(x = floor, y = log_radon, group = county),
             alpha = 0.3,
             size = 3,
             position = position_jitter(width = 0.1, height = 0.2)) +
  facet_wrap(~ county) +
  ggpubr::theme_pubr()

