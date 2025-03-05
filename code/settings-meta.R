min_n <- 50
max_n <- 1000
max_time_points <- 100

n_studies <- 20
all_estimands <- c('m', 'dom', 'rom')
all_tausq <- c(0, 4, 12)
n_reps <- 1000
n_mc <- 1000
n_reps_I2 <- 1e5

sim_scenarios <- as.data.frame(matrix(NA, ncol = 2))
colnames(sim_scenarios) <- c('estimand', 'tausq')
i <- 1
for (estimand in all_estimands){
  for (tausq in all_tausq){
    sim_scenarios[i, 'estimand'] <- estimand
    if (estimand == 'rom' & tausq == 4){
      tausq <- 1/100
    } else if (estimand == 'rom' & tausq == 12){
      tausq <- 3/100
    }
    sim_scenarios[i, 'tausq'] <- tausq
    i <- i + 1
  }
}

n_scenarios = nrow(sim_scenarios)
n_cores <- 20
set.seed(1234)
seeds <- sample.int(2^30, size = n_scenarios)
