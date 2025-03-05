rm(list = ls())

library('xtable')
load('../results/sim-res-meta.RData')

summarize_results <- function(estimand, tausq){
  my_ind <- which(sim_scenarios$estimand == estimand & sim_scenarios$tausq == tausq)
  res_ci <- res_all_ci[[my_ind]]
  res_true <- res_all_true[[my_ind]]
  
  if (estimand == 'm'){
    truth_mean <- log(2) / 0.025
    constant <- 1
  } else if (estimand == 'dom'){
    truth_mean <- 0
    constant <- 1
  } else if (estimand == 'rom'){
    truth_mean <- 1
    constant <- 100
  }
  
  res_ci_mean <- round(c(
    constant * mean(res_ci$pooled_mean_est - truth_mean), 
    constant * sd(res_ci$pooled_mean_est), 
    mean(res_ci$pooled_mean_ci_lb < truth_mean & res_ci$pooled_mean_ci_ub > truth_mean)
    ), 2)
  res_true_mean <- round(c(
    constant * mean(res_true$pooled_mean_est - truth_mean),
    constant * sd(res_true$pooled_mean_est),
    mean(res_true$pooled_mean_ci_lb <= truth_mean & res_true$pooled_mean_ci_ub >= truth_mean)
  ), 2)
  if (tausq > 0){
    res_ci_tausq <- round(c(
      constant * mean(res_ci$tausq_est - tausq),
      constant * sd(res_ci$tausq_est), 
      mean(res_ci$tausq_ci_lb < tausq & res_ci$tausq_ci_ub > tausq)
    ), 2)
    res_true_tausq <- round(c(
      constant * mean(res_true$tausq_est - tausq),
      constant * sd(res_true$tausq_est),
      mean(res_true$tausq_ci_lb <= tausq & res_true$tausq_ci_ub >= tausq)
    ), 2)
    out <- list(res_ci_mean = res_ci_mean, res_true_mean = res_true_mean, 
                res_ci_tausq = res_ci_tausq, res_true_tausq = res_true_tausq)
  } else {
    out <- list(res_ci_mean = res_ci_mean, res_true_mean = res_true_mean)
  }
  return(out)
}

################################################################################
## Estimating the median
################################################################################

m_12_res <- summarize_results(estimand = 'm', tausq = 12)
m_4_res <- summarize_results(estimand = 'm', tausq = 4)
m_0_res <- summarize_results(estimand = 'm', tausq = 0)

xtable(rbind(c(m_12_res$res_ci_mean, m_12_res$res_true_mean), 
      c(m_4_res$res_ci_mean, m_4_res$res_true_mean), 
      c(m_0_res$res_ci_mean, m_0_res$res_true_mean), 
      c(m_12_res$res_ci_tausq, m_12_res$res_true_tausq), 
      c(m_4_res$res_ci_tausq, m_4_res$res_true_tausq)))

################################################################################
## Estimating the difference of medians
################################################################################

dom_12_res <- summarize_results(estimand = 'dom', tausq = 12)
dom_4_res <- summarize_results(estimand = 'dom', tausq = 4)
dom_0_res <- summarize_results(estimand = 'dom', tausq = 0)

xtable(rbind(c(dom_12_res$res_ci_mean, dom_12_res$res_true_mean), 
             c(dom_4_res$res_ci_mean, dom_4_res$res_true_mean), 
             c(dom_0_res$res_ci_mean, dom_0_res$res_true_mean), 
             c(dom_12_res$res_ci_tausq, dom_12_res$res_true_tausq), 
             c(dom_4_res$res_ci_tausq, dom_4_res$res_true_tausq)))

################################################################################
## Estimating the ratio of medians
################################################################################

rom_03_res <- summarize_results(estimand = 'rom', tausq = 0.03)
rom_01_res <- summarize_results(estimand = 'rom', tausq = 0.01)
rom_0_res <- summarize_results(estimand = 'rom', tausq = 0)

xtable(rbind(c(rom_03_res$res_ci_mean, rom_03_res$res_true_mean), 
             c(rom_01_res$res_ci_mean, rom_01_res$res_true_mean), 
             c(rom_0_res$res_ci_mean, rom_0_res$res_true_mean), 
             c(rom_03_res$res_ci_tausq, rom_03_res$res_true_tausq), 
             c(rom_01_res$res_ci_tausq, rom_01_res$res_true_tausq)))

################################################################################
## Box plots
################################################################################

## Median

my_ind_high <- which(sim_scenarios$estimand == 'm' & sim_scenarios$tausq == 12)
my_ind_moderate <- which(sim_scenarios$estimand == 'm' & sim_scenarios$tausq == 4)
my_ind_none <- which(sim_scenarios$estimand == 'm' & sim_scenarios$tausq == 0)

pdf('../results/sim-median.pdf', width = 10, height = 5)
par(mfrow = c(1, 2))

boxplot(res_all_ci[[my_ind_high]]$pooled_mean_est, 
        res_all_true[[my_ind_high]]$pooled_mean_est, 
        res_all_ci[[my_ind_moderate]]$pooled_mean_est, 
        res_all_true[[my_ind_moderate]]$pooled_mean_est, 
        res_all_ci[[my_ind_none]]$pooled_mean_est, 
        res_all_true[[my_ind_none]]$pooled_mean_est, 
        xlab = 'Heterogeneity Level',
        xaxt = "n", 
        col = rep(c('lightblue', 'lightcoral'), times = 3), 
        at = c(1, 2, 4, 5, 7, 8), 
        outline = FALSE, 
        cex.lab = 1.25, cex.main = 1.25, 
        ylab = 'Estimate', 
        main = 'Pooled Median')
axis(side = 1, at = c(1.5, 4.5, 7.5), 
     labels = c("High", "Moderate", "None"), tick = FALSE)
points(c(1, 2, 4, 5, 7, 8), rep(log(2) / 0.025, 6), col = 'red', pch = 19, cex = 1.5)

boxplot(res_all_ci[[my_ind_high]]$tausq_est, 
        res_all_true[[my_ind_high]]$tausq_est, 
        res_all_ci[[my_ind_moderate]]$tausq_est, 
        res_all_true[[my_ind_moderate]]$tausq_est, 
        xlab = 'Heterogeneity Level',
        xaxt = "n", 
        col = rep(c('lightblue', 'lightcoral'), times = 3), 
        at = c(1, 2, 4, 5), 
        outline = FALSE, 
        cex.lab = 1.25, cex.main = 1.25, 
        ylab = 'Estimate', 
        main = 'Between-Study Variance')
axis(side = 1, at = c(1.5, 4.5), 
     labels = c("High", "Moderate"), tick = FALSE)
points(c(1, 2, 4, 5), c(12, 12, 4, 4), col = 'red', pch = 19, cex = 1.5)
dev.off()



## Difference of medians
my_ind_high <- which(sim_scenarios$estimand == 'dom' & sim_scenarios$tausq == 12)
my_ind_moderate <- which(sim_scenarios$estimand == 'dom' & sim_scenarios$tausq == 4)
my_ind_none <- which(sim_scenarios$estimand == 'dom' & sim_scenarios$tausq == 0)

pdf('../results/sim-dif-median.pdf', width = 10, height = 5)
par(mfrow = c(1, 2))

boxplot(res_all_ci[[my_ind_high]]$pooled_mean_est, 
        res_all_true[[my_ind_high]]$pooled_mean_est, 
        res_all_ci[[my_ind_moderate]]$pooled_mean_est, 
        res_all_true[[my_ind_moderate]]$pooled_mean_est, 
        res_all_ci[[my_ind_none]]$pooled_mean_est, 
        res_all_true[[my_ind_none]]$pooled_mean_est, 
        xlab = 'Heterogeneity Level',
        xaxt = "n", 
        col = rep(c('lightblue', 'lightcoral'), times = 3), 
        at = c(1, 2, 4, 5, 7, 8), 
        outline = FALSE, 
        cex.lab = 1.25, cex.main = 1.25, 
        ylab = 'Estimate', 
        main = 'Pooled Difference of Medians')
axis(side = 1, at = c(1.5, 4.5, 7.5), 
     labels = c("High", "Moderate", "None"), tick = FALSE)
points(c(1, 2, 4, 5, 7, 8), rep(0, 6), col = 'red', pch = 19, cex = 1.5)

boxplot(res_all_ci[[my_ind_high]]$tausq_est, 
        res_all_true[[my_ind_high]]$tausq_est, 
        res_all_ci[[my_ind_moderate]]$tausq_est, 
        res_all_true[[my_ind_moderate]]$tausq_est, 
        xlab = 'Heterogeneity Level',
        xaxt = "n", 
        col = rep(c('lightblue', 'lightcoral'), times = 3), 
        at = c(1, 2, 4, 5), 
        outline = FALSE, 
        cex.lab = 1.25, cex.main = 1.25, 
        ylab = 'Estimate', 
        main = 'Between-Study Variance')
axis(side = 1, at = c(1.5, 4.5), 
     labels = c("High", "Moderate"), tick = FALSE)
points(c(1, 2, 4, 5), c(12, 12, 4, 4), col = 'red', pch = 19, cex = 1.5)
dev.off()


## Ratio of medians
my_ind_high <- which(sim_scenarios$estimand == 'rom' & sim_scenarios$tausq == 3/100)
my_ind_moderate <- which(sim_scenarios$estimand == 'rom' & sim_scenarios$tausq == 1/100)
my_ind_none <- which(sim_scenarios$estimand == 'rom' & sim_scenarios$tausq == 0)

pdf('../results/sim-ratio-median.pdf', width = 10, height = 5)
par(mfrow = c(1, 2))

boxplot(res_all_ci[[my_ind_high]]$pooled_mean_est, 
        res_all_true[[my_ind_high]]$pooled_mean_est, 
        res_all_ci[[my_ind_moderate]]$pooled_mean_est, 
        res_all_true[[my_ind_moderate]]$pooled_mean_est, 
        res_all_ci[[my_ind_none]]$pooled_mean_est, 
        res_all_true[[my_ind_none]]$pooled_mean_est, 
        xlab = 'Heterogeneity Level',
        xaxt = "n", 
        col = rep(c('lightblue', 'lightcoral'), times = 3), 
        at = c(1, 2, 4, 5, 7, 8), 
        outline = FALSE, 
        cex.lab = 1.25, cex.main = 1.25, 
        ylab = 'Estimate', 
        main = 'Pooled Ratio of Medians')
axis(side = 1, at = c(1.5, 4.5, 7.5), 
     labels = c("High", "Moderate", "None"), tick = FALSE)
points(c(1, 2, 4, 5, 7, 8), rep(1, 6), col = 'red', pch = 19, cex = 1.5)

boxplot(res_all_ci[[my_ind_high]]$tausq_est, 
        res_all_true[[my_ind_high]]$tausq_est, 
        res_all_ci[[my_ind_moderate]]$tausq_est, 
        res_all_true[[my_ind_moderate]]$tausq_est, 
        xlab = 'Heterogeneity Level',
        xaxt = "n", 
        col = rep(c('lightblue', 'lightcoral'), times = 3), 
        at = c(1, 2, 4, 5), 
        outline = FALSE, 
        cex.lab = 1.25, cex.main = 1.25, 
        ylab = 'Estimate', 
        main = 'Between-Study Variance')
axis(side = 1, at = c(1.5, 4.5), 
     labels = c("High", "Moderate"), tick = FALSE)
points(c(1, 2, 4, 5), c(0.03, 0.03, 0.01, 0.01), col = 'red', pch = 19, cex = 1.5)
dev.off()



################################################################################
## Additional simulations with the "wrongtransform" approaches
################################################################################

rm(list = ls())

library('xtable')
load('../results/sim-res-meta-wrongtransform.RData')

summarize_results_wrongtransform <- function(estimand, tausq){
  my_ind <- which(sim_scenarios$estimand == estimand & sim_scenarios$tausq == tausq)
  res_ci <- res_all_ci_wrongtransform[[my_ind]]
  res_true <- res_all_true_wrongtransform[[my_ind]]
  
  if (estimand == 'dom'){
    # Note: The estimand is really the ratio of medians since we are using the "wrongtransform" type approach
    if (tausq == 12){
      truth_mean <- 0.9999335
      truth_tausq <- 0.01626122
    } else if (tausq == 4){
      truth_mean <- 0.9999615
      truth_tausq <- 0.005274105
    } else {
      truth_mean <- 1
    }
    constant <- 100
  } else if (estimand == 'rom'){
    # Note: The estimand is really the difference of medians since we are using the "wrongtransform" type approach
    if (tausq == 0.03){
      truth_mean <- 0.4193087
      truth_tausq <- 24.13631
    } else if (tausq == 0.01){
      truth_mean <- 0.1370524
      truth_tausq <- 7.798296
    } else {
      truth_mean <- 0
    }
    constant <- 1
  }
  
  res_ci_mean <- round(c(
    constant * mean(res_ci$pooled_mean_est - truth_mean), 
    constant * sd(res_ci$pooled_mean_est), 
    mean(res_ci$pooled_mean_ci_lb < truth_mean & res_ci$pooled_mean_ci_ub > truth_mean)
  ), 2)
  res_true_mean <- round(c(
    constant * mean(res_true$pooled_mean_est - truth_mean),
    constant * sd(res_true$pooled_mean_est),
    mean(res_true$pooled_mean_ci_lb <= truth_mean & res_true$pooled_mean_ci_ub >= truth_mean)
  ), 2)
  if (tausq > 0){
    res_ci_tausq <- round(c(
      constant * mean(res_ci$tausq_est - truth_tausq),
      constant * sd(res_ci$tausq_est), 
      mean(res_ci$tausq_ci_lb < truth_tausq & res_ci$tausq_ci_ub > truth_tausq)
    ), 2)
    res_true_tausq <- round(c(
      constant * mean(res_true$tausq_est - truth_tausq),
      constant * sd(res_true$tausq_est),
      mean(res_true$tausq_ci_lb <= truth_tausq & res_true$tausq_ci_ub >= truth_tausq)
    ), 2)
    out <- list(res_ci_mean = res_ci_mean, res_true_mean = res_true_mean, 
                res_ci_tausq = res_ci_tausq, res_true_tausq = res_true_tausq)
  } else {
    out <- list(res_ci_mean = res_ci_mean, res_true_mean = res_true_mean)
  }
  return(out)
}

################################################################################
## Estimating the difference of medians (from ratio of median simulations)
################################################################################

dom_03_res <- summarize_results_wrongtransform(estimand = 'rom', tausq = 0.03)
dom_01_res <- summarize_results_wrongtransform(estimand = 'rom', tausq = 0.01)
dom_0_res <- summarize_results_wrongtransform(estimand = 'rom', tausq = 0)

xtable(rbind(c(dom_03_res$res_ci_mean, dom_03_res$res_true_mean), 
             c(dom_01_res$res_ci_mean, dom_01_res$res_true_mean), 
             c(dom_0_res$res_ci_mean, dom_0_res$res_true_mean), 
             c(dom_03_res$res_ci_tausq, dom_03_res$res_true_tausq), 
             c(dom_01_res$res_ci_tausq, dom_01_res$res_true_tausq)))

################################################################################
## Estimating the ratio of medians (from difference of median simulations)
################################################################################

rom_12_res <- summarize_results_wrongtransform(estimand = 'dom', tausq = 12)
rom_4_res <- summarize_results_wrongtransform(estimand = 'dom', tausq = 4)
rom_0_res <- summarize_results_wrongtransform(estimand = 'dom', tausq = 0)

xtable(rbind(c(rom_12_res$res_ci_mean, rom_12_res$res_true_mean), 
             c(rom_4_res$res_ci_mean, rom_4_res$res_true_mean), 
             c(rom_0_res$res_ci_mean, rom_0_res$res_true_mean), 
             c(rom_12_res$res_ci_tausq, rom_12_res$res_true_tausq), 
             c(rom_4_res$res_ci_tausq, rom_4_res$res_true_tausq)))

################################################################################
## Box plots
################################################################################
true_mean_dom <- c(0.4193087, 0.1370524, 0)
true_var_dom <- c(24.13631, 7.798296)
true_mean_rom <- c(0.9999335, 0.9999615, 1)
true_var_rom <- c(0.01626122, 0.005274105)

## Difference of medians
my_ind_high <- which(sim_scenarios$estimand == 'rom' & sim_scenarios$tausq == 0.03)
my_ind_moderate <- which(sim_scenarios$estimand == 'rom' & sim_scenarios$tausq == 0.01)
my_ind_none <- which(sim_scenarios$estimand == 'rom' & sim_scenarios$tausq == 0)

pdf('../results/sim-dif-median-wrongtransform.pdf', width = 10, height = 5)
par(mfrow = c(1, 2))

boxplot(res_all_ci_wrongtransform[[my_ind_high]]$pooled_mean_est, 
        res_all_true_wrongtransform[[my_ind_high]]$pooled_mean_est, 
        res_all_ci_wrongtransform[[my_ind_moderate]]$pooled_mean_est, 
        res_all_true_wrongtransform[[my_ind_moderate]]$pooled_mean_est, 
        res_all_ci_wrongtransform[[my_ind_none]]$pooled_mean_est, 
        res_all_true_wrongtransform[[my_ind_none]]$pooled_mean_est, 
        xlab = 'Heterogeneity Level',
        xaxt = "n", 
        col = rep(c('lightblue', 'lightcoral'), times = 3), 
        at = c(1, 2, 4, 5, 7, 8), 
        outline = FALSE, 
        cex.lab = 1.25, cex.main = 1.25, 
        ylab = 'Estimate', 
        main = 'Pooled Difference of Medians')
axis(side = 1, at = c(1.5, 4.5, 7.5), 
     labels = c("High", "Moderate", "None"), tick = FALSE)
points(c(1, 2, 4, 5, 7, 8), rep(true_mean_dom, each = 2), col = 'red', pch = 19, cex = 1.5)

boxplot(res_all_ci_wrongtransform[[my_ind_high]]$tausq_est, 
        res_all_true_wrongtransform[[my_ind_high]]$tausq_est, 
        res_all_ci_wrongtransform[[my_ind_moderate]]$tausq_est, 
        res_all_true_wrongtransform[[my_ind_moderate]]$tausq_est, 
        xlab = 'Heterogeneity Level',
        xaxt = "n", 
        col = rep(c('lightblue', 'lightcoral'), times = 3), 
        at = c(1, 2, 4, 5), 
        outline = FALSE, 
        cex.lab = 1.25, cex.main = 1.25, 
        ylab = 'Estimate', 
        main = 'Between-Study Variance')
axis(side = 1, at = c(1.5, 4.5), 
     labels = c("High", "Moderate"), tick = FALSE)
points(c(1, 2, 4, 5), rep(true_var_dom, each = 2), col = 'red', pch = 19, cex = 1.5)
dev.off()


## Ratio of medians
my_ind_high <- which(sim_scenarios$estimand == 'dom' & sim_scenarios$tausq == 12)
my_ind_moderate <- which(sim_scenarios$estimand == 'dom' & sim_scenarios$tausq == 4)
my_ind_none <- which(sim_scenarios$estimand == 'dom' & sim_scenarios$tausq == 0)

pdf('../results/sim-ratio-median-wrongtransform.pdf', width = 10, height = 5)
par(mfrow = c(1, 2))

boxplot(res_all_ci_wrongtransform[[my_ind_high]]$pooled_mean_est, 
        res_all_true_wrongtransform[[my_ind_high]]$pooled_mean_est, 
        res_all_ci_wrongtransform[[my_ind_moderate]]$pooled_mean_est, 
        res_all_true_wrongtransform[[my_ind_moderate]]$pooled_mean_est, 
        res_all_ci_wrongtransform[[my_ind_none]]$pooled_mean_est, 
        res_all_true_wrongtransform[[my_ind_none]]$pooled_mean_est, 
        xlab = 'Heterogeneity Level',
        xaxt = "n", 
        col = rep(c('lightblue', 'lightcoral'), times = 3), 
        at = c(1, 2, 4, 5, 7, 8), 
        outline = FALSE, 
        cex.lab = 1.25, cex.main = 1.25, 
        ylab = 'Estimate', 
        main = 'Pooled Ratio of Medians')
axis(side = 1, at = c(1.5, 4.5, 7.5), 
     labels = c("High", "Moderate", "None"), tick = FALSE)
points(c(1, 2, 4, 5, 7, 8), rep(true_mean_rom, each = 2), col = 'red', pch = 19, cex = 1.5)

boxplot(res_all_ci_wrongtransform[[my_ind_high]]$tausq_est, 
        res_all_true_wrongtransform[[my_ind_high]]$tausq_est, 
        res_all_ci_wrongtransform[[my_ind_moderate]]$tausq_est, 
        res_all_true_wrongtransform[[my_ind_moderate]]$tausq_est, 
        xlab = 'Heterogeneity Level',
        xaxt = "n", 
        col = rep(c('lightblue', 'lightcoral'), times = 3), 
        at = c(1, 2, 4, 5), 
        outline = FALSE, 
        cex.lab = 1.25, cex.main = 1.25, 
        ylab = 'Estimate', 
        main = 'Between-Study Variance')
axis(side = 1, at = c(1.5, 4.5), 
     labels = c("High", "Moderate"), tick = FALSE)
points(c(1, 2, 4, 5), rep(true_var_rom, each = 2), col = 'red', pch = 19, cex = 1.5)
dev.off()
