
#############
# DETECT AND SET WORKING DIRECTORY
#############

a<-Sys.time()

if(!require("rstudioapi")) install.packages("rstudioapi")

#set your working directory
#I'm lazy so I autodetect where this file is and set it one level higher
#if this fails for any reason, set setwd manually to the folder path of the main repo
# curpath<-dirname(rstudioapi::getSourceEditorContext()$path)
# curpath
# setwd(curpath)
# #go up a level out of the code folder for neatness
# setwd("../")


#############
# INSTALLS AND DEPENDENCIES
#############

if(!require("remotes")) install.packages("remotes")
if(!require("cmdstanr")){
  remotes::install_github("stan-dev/cmdstanr", upgrade="always")
  library(cmdstanr)
  install_cmdstan()
  }else{library(cmdstanr) }
  
if(!require("posterior")) install.packages("posterior")
if(!require("loo")) install.packages("loo")
if(!require("tidyr")) install.packages("tidyr")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("dplyr")) install.packages("dplyr")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("gridExtra")) install.packages("gridExtra")
if(!require("bayesplot")) install.packages("bayesplot")
if(!require("tidybayes")) install.packages("tidybayes")
#if(!require("rprojroot")) install.packages("rprojroot")
if(!require("rstanarm")) install.packages("rstanarm")
if(!require("HSAUR3")) install.packages("HSAUR3")
if(!require("brms")) install.packages("brms")
if(!require("modelr")) install.packages("modelr")


#check load
library(posterior)
library(loo)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(bayesplot)
library(tidybayes)
#library(rprojroot)
library(tidyverse)
library(rstanarm)
library(HSAUR3)
library(brms)
library(modelr)

#############
# OPTION SETTING
#############

#options(mc.cores = 1)
options(mc.cores = parallel::detectCores())
options(posterior.num_args=list(sigfig=2)) # by default summaries with 2 significant digits
options(pillar.neg=FALSE)
theme_set(bayesplot::theme_default(base_family = "sans"))

SEED <- 48927 # set random seed for reproducability


print(Sys.time()-a)
