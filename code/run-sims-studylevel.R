rm(list = ls())

library(doParallel)
library(doRNG)
library(foreach)
library(survival)

source('settings-studylevel.R')
source('helper.R')


if (n_cores > 1){
  registerDoParallel(cores=n_cores)
}

res_bc_log <- res_bc_loglog <- res_boot <- matrix(NA, nrow = n_reps, ncol = n_scenarios)

for (m in 1:n_scenarios){
  set.seed(seeds[m])
  n_subjects <- sim_scenarios[m, 'n_subjects']
  censoring_distribution <- sim_scenarios[m, 'censoring_distribution']
  distribution <- sim_scenarios[m, 'distribution']
  
  res_temp <- foreach(t = 1:n_reps, .combine = rbind) %dorng%{
    dat <- get_study_data(n_subjects = n_subjects, 
                          max_time_points = max_time_points, 
                          random_effect = 0, 
                          two_groups = FALSE, 
                          ratio = FALSE, 
                          distribution = distribution, 
                          censoring_distribution = censoring_distribution)
    fit_bc_log <- get_est_and_se(dat = dat, two_groups = FALSE, ratio = FALSE, method = 'bc-log')
    fit_bc_loglog <- get_est_and_se(dat = dat, two_groups = FALSE, ratio = FALSE, method = 'bc-loglog')
    fit_boot <- get_est_and_se(dat = dat, two_groups = FALSE, ratio = FALSE, method = 'bootstrap')

    return(data.frame(se_bc_log = fit_bc_log$se, se_bc_loglog = fit_bc_loglog$se, se_boot = fit_boot$se))
    }
  
  res_bc_log[, m] <- res_temp$se_bc_log
  res_bc_loglog[, m] <- res_temp$se_bc_loglog
  res_boot[, m] <- res_temp$se_boot
}

# Obtain true SE
sei_true <- rep(NA, times = n_scenarios)
for (m in 1:n_scenarios){
  set.seed(seeds[m])
  n_subjects <- sim_scenarios[m, 'n_subjects']
  censoring_distribution <- sim_scenarios[m, 'censoring_distribution']
  distribution <- sim_scenarios[m, 'distribution']
  
  sei_true_temp <- foreach(t = 1:n_mc, .combine = c) %dorng%{
    dat <- get_study_data(n_subjects = n_subjects, 
                          max_time_points = max_time_points, 
                          random_effect = 0, 
                          two_groups = FALSE, 
                          ratio = FALSE, 
                          distribution = distribution, 
                          censoring_distribution = censoring_distribution)
    return(get_est_and_se(dat = dat, two_groups = FALSE, ratio = FALSE)$est)
  }
  sei_true[m] <- sd(sei_true_temp, na.rm = TRUE) 
}

save.image('../results/sim-res-studylevel.RData')
