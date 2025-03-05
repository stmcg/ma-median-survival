source('helper.R')
perc_censored <- matrix(NA, nrow = 5, ncol = 2)
rownames(perc_censored) <- c('exponential', 'weibull', 'mixture', 'weibull skew 1', 'weibull skew 2')
colnames(perc_censored) <- c('uniform', 'exponential')

set.seed(1234)
n <- 1e7
for (distribution in rownames(perc_censored)){
  for (censoring_distribution in colnames(perc_censored)){
    if (censoring_distribution == 'uniform'){
      censor_data <- runif(n, min = 0, max = 100)
    } else if (censoring_distribution == 'exponential'){
      censor_data <- pmin(rexp(n, 1/60), 100)
    }
    event_data <- simulate_data(n = n, distribution = distribution)
    perc_censored[rownames(perc_censored) == distribution, 
                  colnames(perc_censored) == censoring_distribution] <- 
      mean(censor_data < event_data)
  }
}

round(100 * perc_censored)
