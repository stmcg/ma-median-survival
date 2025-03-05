max_time_points <- 100

n_reps <- 1000
n_mc <- 1e5

all_n_subjects <- c(50, 100, 250, 1000)
all_distribution <- c('exponential', 'weibull', 'mixture', 'weibull skew 1', 'weibull skew 2')
all_censoring_distribution <- c('uniform', 'exponential')

sim_scenarios <- as.data.frame(matrix(NA, ncol = 2))
colnames(sim_scenarios) <- c('n_subjects', 'distribution')
i <- 1
for (distribution in all_distribution){
  for (censoring_distribution in all_censoring_distribution){
    for (n_subjects in all_n_subjects){
      sim_scenarios[i, 'distribution'] <- distribution
      sim_scenarios[i, 'censoring_distribution'] <- censoring_distribution
      sim_scenarios[i, 'n_subjects'] <- n_subjects
      i <- i + 1
    }
  }
}

n_scenarios = nrow(sim_scenarios)
n_cores <- 20

set.seed(1234)
seeds <- sample.int(2^30, size = n_scenarios)
