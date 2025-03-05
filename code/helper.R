simulate_data <- function(n, distribution, lambda = 0.025){
  if (distribution == 'exponential'){
    return(rexp(n, lambda))
  } else if (distribution == 'weibull') {
    return(rweibull(n, 2, 35))
  } else if (distribution == 'mixture'){
    pi <- c(2/3, 1/3); shape <- c(2, 1.5); scale <- c(20, 50)
    comp <- sample(1:2, prob = pi, size = n, replace = TRUE)
    return(rweibull(n = n, shape = shape[comp], scale = scale[comp]))
  } else if (distribution == 'weibull skew 1'){
    return(rweibull(n, 2/3, 35))
  } else if (distribution == 'weibull skew 2'){
    return(rweibull(n, 1/3, 35))
  }
}

get_study_data <- function(n_subjects, max_time_points, random_effect, two_groups = FALSE, ratio = FALSE, 
                           distribution, censoring_distribution){
  
  if (two_groups){
    n_subjects_e <- n_subjects_c <- n_subjects
    if (ratio){
      lambda <- 0.025 / exp(random_effect)
    } else {
      lambda <- log(2) / (random_effect + log(2) / 0.025)
    }
    event_times_e <- simulate_data(n = n_subjects_e, distribution = distribution, lambda = lambda)
    event_times_c <- simulate_data(n = n_subjects_c, distribution = distribution)
    
    if (censoring_distribution == 'uniform'){
      censor_times_e <- runif(n_subjects_e, min = 0, max = 100)
      censor_times_c <- runif(n_subjects_c, min = 0, max = 100)
    } else if (censoring_distribution == 'exponential'){
      censor_times_e <- pmin(rexp(n_subjects_e, 1/60), max_time_points)
      censor_times_c <- pmin(rexp(n_subjects_c, 1/60), max_time_points)
    }
    
    time_e <- pmin(event_times_e, censor_times_e)
    time_c <- pmin(event_times_c, censor_times_c)
    
    status_e <- ifelse(event_times_e > censor_times_e, 1, 2)
    status_c <- ifelse(event_times_c > censor_times_c, 1, 2)
    
    time <- c(time_e, time_c)
    status <- c(status_e, status_c)
    group <- c(rep(1, n_subjects_e), rep(2, n_subjects_c))
  } else {
    lambda <- log(2) / (random_effect + log(2) / 0.025)
    event_times <- simulate_data(n = n_subjects, distribution = distribution, lambda = lambda)
    if (censoring_distribution == 'uniform'){
      censor_times <- runif(n_subjects, min = 0, max = 100)
    } else if (censoring_distribution == 'exponential'){
      censor_times <- pmin(rexp(n_subjects, 1/60), max_time_points)
    }
    time <- pmin(event_times, censor_times)
    status <- ifelse(event_times > censor_times, 1, 2)
    group <- rep(1, n_subjects)
  }
  return(data.frame(time = time, status = status, group = group))
}

get_est_and_se <- function(dat, two_groups = FALSE, ratio = FALSE, verbose = FALSE, 
                           method = 'bc-log', only_est = FALSE){
  if (two_groups){
    if (only_est){
      fit <- survfit(Surv(time, status) ~ group, data = dat, se.fit = FALSE, conf.type = "none")
      meds <- summary(fit)$table[, 'median']
      if (ratio){
        return(list(est = log(meds[1]) - log(meds[2]), se = NA))
      } else {
        return(list(est = meds[1] - meds[2], se = NA))
      }
    }
    if (method == 'bc-log'){
      fit <- survfit(Surv(time, status) ~ group, data = dat, conf.type = 'log')
      temp <- summary(fit)$table
      lb <- temp[, '0.95LCL']; ub <- temp[, '0.95UCL']
    } else if (method == 'bc-loglog'){
      fit <- survfit(Surv(time, status) ~ group, data = dat, conf.type = 'log-log')
      temp <- summary(fit)$table
      lb <- temp[, '0.95LCL']; ub <- temp[, '0.95UCL']
    } else if (method == 'bootstrap'){
      fit <- survfit(Surv(time, status) ~ group, data = dat, se.fit = FALSE, conf.type = "none")
      temp <- summary(fit)$table
      n <- nrow(dat); n_boot <- 1000
      boot_temp1 <- boot_temp2 <- rep(NA, times = n_boot)
      for (i in 1:n_boot){
        fit_boot <- survfit(Surv(time, status) ~ group, 
                            data = dat[sample(1:n, size = n, replace = TRUE), ], 
                            se.fit = FALSE, conf.type = "none")
        med_boot <- summary(fit_boot)$table[, 'median']
        boot_temp1[i] <- med_boot[1]
        boot_temp2[i] <- med_boot[2]
      }
      lb <- c(quantile(boot_temp1, probs = 0.025, na.rm = TRUE), 
              quantile(boot_temp2, probs = 0.025, na.rm = TRUE))
      ub <- c(quantile(boot_temp1, probs = 0.975, na.rm = TRUE), 
              quantile(boot_temp2, probs = 0.975, na.rm = TRUE))
      if (mean(is.na(boot_temp1)) > 0.10 | 
          mean(is.na(boot_temp2)) > 0.10){
        n_na <- print(paste0(100 * max(mean(is.na(boot_temp1)), mean(is.na(boot_temp2))), 
                             '% NA values in bootstrapping'))
      }
    }
    ses <- c(se_helper(est = temp[1, 'median'], lb = lb[1], ub = ub[1], verbose = verbose), 
             se_helper(est = temp[2, 'median'], lb = lb[2], ub = ub[2], verbose = verbose))
    meds <- temp[, 'median']
    if (ratio){
      res <- list(est = log(meds[1]) - log(meds[2]), 
                  se = sqrt(ses[1]^2 / meds[1]^2 + ses[2]^2 / meds[2]^2))
    } else {
      res <- list(est = temp[1, 'median'] - temp[2, 'median'], 
                  se = sqrt(ses[1]^2 + ses[2]^2))
    }
  } else {
    if (only_est){
      fit <- survfit(Surv(time, status) ~ 1, data = dat, se.fit = FALSE, conf.type = "none")
      return(list(est = summary(fit)$table['median'], se = NA))
    }
    if (method == 'bc-log'){
      fit <- survfit(Surv(time, status) ~ 1, data = dat, conf.type = 'log')
      temp <- summary(fit)$table
      lb <- temp['0.95LCL']; ub <- temp['0.95UCL']
    } else if (method == 'bc-loglog'){
      fit <- survfit(Surv(time, status) ~ 1, data = dat, conf.type = 'log-log')
      temp <- summary(fit)$table
      lb <- temp['0.95LCL']; ub <- temp['0.95UCL']
    } else if (method == 'bootstrap'){
      fit <- survfit(Surv(time, status) ~ 1, data = dat, se.fit = FALSE, conf.type = "none")
      temp <- summary(fit)$table
      n <- nrow(dat); n_boot <- 1000
      boot_temp <- rep(NA, times = n_boot)
      for (i in 1:n_boot){
        fit_boot <- survfit(Surv(time, status) ~ 1, 
                            data = dat[sample(1:n, size = n, replace = TRUE), ], 
                            se.fit = FALSE, conf.type = "none")
        boot_temp[i] <- summary(fit_boot)$table['median']
      }
      lb <- quantile(boot_temp, probs = 0.025, na.rm = TRUE)
      ub <- quantile(boot_temp, probs = 0.975, na.rm = TRUE)
      if (mean(is.na(boot_temp)) > 0.10){
        n_na <- 
          print(paste0(100 * mean(is.na(boot_temp)), '% NA values in bootstrapping'))
      }
    }
    se <- se_helper(est = temp['median'], lb = lb, ub = ub, verbose = verbose)
    res <- list(est = temp['median'],
                se = se)
  }
  return(res)
}

se_helper <- function(est, lb, ub, verbose = FALSE){
  if (!is.na(ub)){
    return((ub - lb) / (2 * qnorm(0.975)))
  } else {
    if (verbose){
      print('Upper bound not available')
    }
    return((est - lb) / (qnorm(0.975)))
  }
}

extract_pooled_mean <- function(fit, estimand){
  if (!is.null(fit)){
    if (estimand == 'rom'){
      res <- exp(c(fit$beta, fit$ci.lb, fit$ci.ub))
    } else {
      res <- c(fit$beta, fit$ci.lb, fit$ci.ub)
    }
    
  } else {
    res <- c(NA, NA, NA)
  }
  return(res)
}

extract_tausq <- function(fit){
  if (!is.null(fit)){
    res <- confint(fit)[[1]]['tau^2', ]
  } else {
    res <- c(NA, NA, NA)
  }
  return(res)
}

rma.uni_robust <- function(yi, sei, method){
  if (any(is.na(sei))){
    res <- NULL
  } else {
    if (method == 'FE'){
      res <- try(metafor::rma.uni(yi = yi, sei = sei, method = method, test = "t"), silent = TRUE)
    } else {
      res <- try(metafor::rma.uni(yi = yi, sei = sei, method = method, test = "adhoc"), silent = TRUE)
      if ('try-error' %in% class(res)){
        res <- metafor::rma.uni(yi = yi, sei = sei, method = 'DL', test = "adhoc")
      }
    }
  }
  return(res)
}