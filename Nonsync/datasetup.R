
#data generation


#1Stan_LM.R
#kilpisjarvi data from https://avehtari.github.io/BDA_course_Aalto/
#https://avehtari.github.io/BDA_R_demos/demos_rstan/cmdstanr_demo.html#


#3Stan_LM.R

#adapted from
cars.data <- mtcars

cars.data <- 
  cars.data %>%
  rename(weight = mpg,
          kills = cyl,
         rangeS = disp,
         speed = hp,
         height = drat,
         humanDens = wt,
         hunt_type = vs,
         parent_type = am,
         cubs = gear) %>%
  mutate(hunt_type = factor(hunt_type, levels = c(0, 1), 
                              labels = c("ambush", "pursuit")),
         trans_type = factor(parent_type, levels = c(0, 1),
                             labels = c("nurture", "abandon")))

cars.data$kills<-10-cars.data$kills
write.csv(cars.data, "Data/LynxWeight.csv", row.names=F)


#stan bernoulli
#https://nicholasrjenkins.science/tutorials/bayesian-inference-with-stan/logit_probit_models/

gre.data <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(gre.data)

write.csv(gre.data, "Data/gre.csv", row.names=F)



#stanarm_binomial1.R


#stan_hiermod.R
prostate = read.csv('https://raw.githubusercontent.com/andrewcparnell/bhm_course/master/data/prostate.csv')
head(prostate)

#The data contain information on 97 men with prostate cancer. 
#The response variable here is lpsa - the log of the prostate specific antigen. 
#We want to see whether this variable can be predicted from the others. The other key variables we will use are:
  
#lcavol, the log cancer volume
#lweight, the log of the weight of the patient
#gleason, the Gleason grade of the cancer (from 6 to 9). A higher value is more serious

#lpsa
prostate <- 
  prostate %>%
  select(lpsa, lcavol, lweight, gleason, age)  %>%
  rename(antm = lpsa,
        log_f = lcavol, 
         weight = lweight,
         age = age,
         stage = gleason)

write.csv(prostate, "Data/SeabirdDisease.csv", row.names=F)


# stanarm_poisson
#https://ourcodingclub.github.io/tutorials/stan-2/
  
  
  


