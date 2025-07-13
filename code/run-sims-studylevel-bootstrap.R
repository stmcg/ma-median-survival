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

n_scenarios <- 4
res_1000 <- res_2000 <- res_5000 <- res_10000 <- 
  matrix(NA, nrow = n_reps, ncol = n_scenarios)

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
    
    fit <- survfit(Surv(time, status) ~ 1, data = dat, se.fit = FALSE, conf.type = "none")
    temp <- summary(fit)$table
    n <- nrow(dat); n_boot <- 10000
    boot_temp <- rep(NA, times = n_boot)
    for (i in 1:n_boot){
      fit_boot <- survfit(Surv(time, status) ~ 1, 
                          data = dat[sample(1:n, size = n, replace = TRUE), ], 
                          se.fit = FALSE, conf.type = "none")
      boot_temp[i] <- summary(fit_boot)$table['median']
    }
    
    if (mean(is.na(boot_temp)) > 0.10){
      n_na <- 
        print(paste0(100 * mean(is.na(boot_temp)), '% NA values in bootstrapping'))
    }
    
    lb <- quantile(boot_temp[1:1000], probs = 0.025, na.rm = TRUE)
    ub <- quantile(boot_temp[1:1000], probs = 0.975, na.rm = TRUE)
    res_1000 <- se_helper(est = temp['median'], lb = lb, ub = ub, verbose = verbose)
    
    lb <- quantile(boot_temp[1:2000], probs = 0.025, na.rm = TRUE)
    ub <- quantile(boot_temp[1:2000], probs = 0.975, na.rm = TRUE)
    res_2000 <- se_helper(est = temp['median'], lb = lb, ub = ub, verbose = verbose)
    
    lb <- quantile(boot_temp[1:5000], probs = 0.025, na.rm = TRUE)
    ub <- quantile(boot_temp[1:5000], probs = 0.975, na.rm = TRUE)
    res_5000 <- se_helper(est = temp['median'], lb = lb, ub = ub, verbose = verbose)
    
    lb <- quantile(boot_temp, probs = 0.025, na.rm = TRUE)
    ub <- quantile(boot_temp, probs = 0.975, na.rm = TRUE)
    res_10000 <- se_helper(est = temp['median'], lb = lb, ub = ub, verbose = verbose)
    
    return(data.frame(res_1000 = res_1000, res_2000 = res_2000, 
                      res_5000 = res_5000, res_10000 = res_10000))
  }
  res_1000[, m] <- res_temp$res_1000
  res_2000[, m] <- res_temp$res_2000
  res_5000[, m] <- res_temp$res_5000
  res_10000[, m] <- res_temp$res_10000
}

save.image('../results/sim-res-studylevel-bootstrap.RData')