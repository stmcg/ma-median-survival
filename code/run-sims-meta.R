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
res_all_ci <- res_all_true <- vector(mode = 'list', length = nrow(sim_scenarios))

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
  
  res_temp <- foreach(t = 1:n_reps) %dorng%{
    res_ci <- res_true <-  
      data.frame(pooled_mean_est = NA,
                 pooled_mean_ci_lb = NA,
                 pooled_mean_ci_ub = NA,
                 tausq_est = NA, 
                 tausq_ci_lb = NA, 
                 tausq_ci_ub = NA, 
                 I2 = NA)
    
    if (tausq > 0){
      random_effects <- rnorm(n = n_studies, mean = 0, sd = sqrt(tausq))
    } else {
      random_effects <- rep(0, times = n_studies)
    }
    n_subjects <- round(runif(n_studies, min = min_n, max = max_n))
    ci_method <- sample(c('bc-log', 'bc-loglog', 'bootstrap'), 
                        size = n_studies, replace = TRUE, prob = c(1/3, 1/3, 1/3))
    censoring_distribution <- sample(c('uniform', 'exponential'), 
                              size = n_studies, replace = TRUE, prob = c(1/2, 1/2))
    yi <-  rep(NA, n_studies)
    sei_ci <- rep(NA, n_studies)
    sei_true <- rep(NA, n_studies)
    
    for (study in 1:n_studies){
      # Generate data
      dat <- get_study_data(n_subjects = n_subjects[study], 
                            max_time_points = max_time_points, 
                            random_effect = random_effects[study], 
                            two_groups = two_groups, 
                            ratio = ratio, 
                            distribution = 'exponential', 
                            censoring_distribution = censoring_distribution[study])
      
      # Obtain point estimates and CIs
      fit <- get_est_and_se(dat = dat, two_groups = two_groups, ratio = ratio, 
                            method = ci_method[study])
      yi[study] <- fit$est
      sei_ci[study] <- fit$se
      
      # Obtain true SE
      sei_true_temp <- rep(NA, times = n_mc)
      for (i in 1:n_mc){
        dat <- get_study_data(n_subjects = n_subjects[study], 
                              max_time_points = max_time_points, 
                              random_effect = random_effects[study], 
                              two_groups = two_groups, 
                              ratio = ratio, 
                              distribution = 'exponential', 
                              censoring_distribution = censoring_distribution[study])
        sei_true_temp[i] <- get_est_and_se(dat = dat, two_groups = two_groups, 
                                           ratio = ratio, only_est = TRUE)$est
      }
      sei_true[study] <- sd(sei_true_temp, na.rm = TRUE)
    }
    
    if (tausq > 0){
      method = 'REML'
    } else {
      method = 'FE'
    }
    
    ci_ind <- which(!is.na(yi) & !is.na(sei_ci))
    true_ind <- which(!is.na(yi) & !is.na(sei_true))
    
    if (length(ci_ind) != length(yi)){
      print('Removed studies for the wald method')
    }
    if (length(true_ind) != length(yi)){
      print('Removed studies for the benchmark method')
    }
    
    fit_ci <- rma.uni_robust(yi = yi[ci_ind], sei = sei_ci[ci_ind], method = method)
    fit_true <- rma.uni_robust(yi = yi[true_ind], sei = sei_true[true_ind], method = method)
    
    mycol <- c('pooled_mean_est', 'pooled_mean_ci_lb', 'pooled_mean_ci_ub')
    res_ci[1, mycol] <- extract_pooled_mean(fit_ci, estimand = estimand)
    res_true[1, mycol] <- extract_pooled_mean(fit_true, estimand = estimand)
    
    if (tausq > 0){
      mycol <- c('tausq_est', 'tausq_ci_lb', 'tausq_ci_ub')
      res_ci[1, mycol] <- extract_tausq(fit_ci)
      res_true[1, mycol] <- extract_tausq(fit_true)
      res_ci[1, 'I2'] <- fit_ci$I2
      res_true[1, 'I2'] <- fit_true$I2
    }
    
    return(list(res_ci = res_ci, res_true = res_true))
  }
  
  res_ci <- res_true <- 
    data.frame(pooled_mean_est = rep(NA, times = n_reps),
               pooled_mean_ci_lb = rep(NA, times = n_reps),
               pooled_mean_ci_ub = rep(NA, times = n_reps),
               tausq_est = rep(NA, times = n_reps), 
               tausq_ci_lb = rep(NA, times = n_reps), 
               tausq_ci_ub = rep(NA, times = n_reps), 
               I2 = rep(NA, times = n_reps))
  for (rep in 1:n_reps) {
    res_ci[rep, ] <- res_temp[[rep]]$res_ci
    res_true[rep, ] <- res_temp[[rep]]$res_true
  }
  
  res_all_ci[[m]] <- res_ci
  res_all_true[[m]] <- res_true
}

save.image('../results/sim-res-meta.RData')
