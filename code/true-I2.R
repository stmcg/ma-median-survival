rm(list = ls())

library(doParallel)
library(doRNG)
library(foreach)
library(metafor)
library(survival)

source('settings-meta.R')
source('helper.R')

if (n_cores > 1){
  registerDoParallel(cores=n_cores)
}

sim_scenarios <- sim_scenarios[sim_scenarios$tausq != 0, ]
true_I2 <- rep(NA, nrow(sim_scenarios))

for (m in 1:nrow(sim_scenarios)){
  set.seed(seeds[m])
  
  tausq <- sim_scenarios[m, 'tausq']
  estimand <- sim_scenarios[m, 'estimand']
  
  if (estimand == 'm'){
    two_groups <- ratio <- FALSE
  } else if (estimand == 'dom'){
    two_groups <- TRUE; ratio <- FALSE
  } else if (estimand == 'rom'){
    two_groups <- ratio <- TRUE
  }
  
  random_effects <- rnorm(n = n_reps_I2, mean = 0, sd = sqrt(tausq))
  n_subjects <- round(runif(n_reps_I2, min = min_n, max = max_n))
  censoring_distribution <- sample(c('uniform', 'exponential'), 
                                   size = n_reps_I2, replace = TRUE, prob = c(1/2, 1/2))
  res_temp <- foreach(t = 1:n_reps_I2, .combine = c) %dorng%{
    dat <- get_study_data(n_subjects = n_subjects[t], 
                          max_time_points = max_time_points, 
                          random_effect = random_effects[t], 
                          two_groups = two_groups, 
                          ratio = ratio, 
                          distribution = 'exponential', 
                          censoring_distribution = censoring_distribution[t])
    return(get_est_and_se(dat = dat, two_groups = two_groups, ratio = ratio)$est)
  }
  
  true_I2[m] <- tausq / var(res_temp)
}
res_temp <- NULL

save.image('../results/true-I2.RData')