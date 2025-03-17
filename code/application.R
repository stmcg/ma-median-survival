rm(list = ls())

library('metafor')
library('xtable')

dat <- read.csv('data-illustration.csv')

# Table of extracted summary data
myround <- function(x, digits = 2){
  return(format(round(x, digits), nsmall = digits))
}
g1_summary <- paste0(c(myround(dat$med.g1)), ' (', myround(dat$med.ci.lb.g1), ', ', myround(dat$med.ci.ub.g1), ')')
g2_summary <- paste0(c(myround(dat$med.g2)), ' (', myround(dat$med.ci.lb.g2), ', ', myround(dat$med.ci.ub.g2), ')')
print(xtable(cbind(dat$study, dat$n.g1, g1_summary, dat$n.g2, g2_summary)), include.rownames = F)

# Computing outcome measures and their standard errors for each study
alpha <- 0.05
dat$med.se.g1 <- (dat$med.ci.ub.g1 - dat$med.ci.lb.g1) / (2 * qnorm(1 - alpha / 2))
dat$med.se.g2 <- (dat$med.ci.ub.g2 - dat$med.ci.lb.g2) / (2 * qnorm(1 - alpha / 2))

dat$diff_med <- dat$med.g1 - dat$med.g2
dat$diff_med_se <- sqrt(dat$med.se.g1^2 + dat$med.se.g2^2)

dat$logratio <- log(dat$med.g1) - log(dat$med.g2)
dat$logratio_se <- sqrt(dat$med.se.g1^2 / dat$med.g1^2 + dat$med.se.g2^2 / dat$med.g2^2)

# Running meta-analyses
res_g1 <- rma(yi = dat$med.g1, sei = dat$med.se.g1, test = 'adhoc')
res_g2 <- rma(yi = dat$med.g2, sei = dat$med.se.g2, test = 'adhoc')
res_dif <- rma(yi = dat$diff_med, sei = dat$diff_med_se, test = 'adhoc')
res_logratio <- rma(yi = dat$logratio, sei = dat$logratio_se, test = 'adhoc')

# Summarizing meta-analysis results
predict_res_g1 <- predict(res_g1)
predict_res_g2 <- predict(res_g2)
predict_res_dif <- predict(res_dif)
predict_ratio <- predict(res_logratio, transf = exp)

print_results <- function(res, res_predict){
  print(paste0(round(res_predict$pred, 2), ' (', round(res_predict$ci.lb, 2), ', ', round(res_predict$ci.ub, 2), ')'))
  print(paste0('(', round(res_predict$pi.lb, 2), ', ', round(res_predict$pi.ub, 2), ')'))
  print(paste(round(res$I2, 2)))
}
print_results(res_g1, predict_res_g1)
print_results(res_g2, predict_res_g2)
print_results(res_dif, predict_res_dif)
print_results(res_logratio, predict_ratio)


# Forest plots
pdf('median-experimental.pdf', width = 7, height = 9)
forest(res_g1, slab = dat$study, addpred = T, shade = T, refline = F,
       header = c('Study', 'Median OS [95% CI]'),
       xlab = 'Median OS in Experimental Arm',
       mlab = 'Overall')
dev.off()

pdf('median-control.pdf', width = 7, height = 9)
forest(res_g2, slab = dat$study, addpred = T, shade = T, refline = F,
       header = c('Study', 'Median OS [95% CI]'),
       xlab = 'Median OS in Comparator Arm',
       mlab = 'Overall')
dev.off()

pdf('difference-median.pdf', width = 7, height = 9)
forest(res_dif, slab = dat$study, addpred = T, shade = T,
       header = c('Study', 'Difference of Median OS [95% CI]'),
       xlab = 'Difference of Median OS',
       mlab = 'Overall')
dev.off()

pdf('ratio-median.pdf', width = 7, height = 9)
forest(res_logratio, slab = dat$study, addpred = T, shade = T,
       header = c('Study', 'Ratio of Median OS [95% CI]'),
       xlab = 'Ratio of Median OS',
       mlab = 'Overall',
       atransf = exp,
       at = log(c(0.25, 0.5, 1, 2, 4)))
dev.off()


pdf('dif-and-ratio-median.pdf', width = 14, height = 14)
par(mfrow = c(1, 2))
forest(res_dif, slab = dat$study, addpred = T, shade = T,
       header = c('Study', 'Difference of Median OS [95% CI]'),
       xlab = 'Difference of Median OS',
       mlab = 'Overall', cex = 1.3, xlim = c(-35, 50))
forest(res_logratio, slab = dat$study, addpred = T, shade = T,
       header = c('Study', 'Ratio of Median OS [95% CI]'),
       xlab = 'Ratio of Median OS',
       mlab = 'Overall',
       atransf = exp,
       at = log(c(0.25, 0.5, 1, 2, 4)), cex = 1.3, xlim = c(-2.8, 3))
dev.off()

pdf('dif-and-ratio-median-original.pdf', width = 14, height = 12)
par(mfrow = c(1, 2))
forest(res_dif, slab = dat$study, addpred = T, shade = T,
       header = c('Study', 'Difference of Median OS [95% CI]'),
       xlab = 'Difference of Median OS',
       mlab = 'Overall', cex = 1.25, xlim = c(-40, 50))
forest(res_logratio, slab = dat$study, addpred = T, shade = T,
       header = c('Study', 'Ratio of Median OS [95% CI]'),
       xlab = 'Ratio of Median OS',
       mlab = 'Overall',
       atransf = exp,
       at = log(c(0.25, 0.5, 1, 2, 4)), cex = 1.25, xlim = c(-3, 3))
dev.off()
# Note: metamedian# Note: The main discrepancy with the analysis reported in the original
# paper is the exclusion of a study that did not report HR. A small discrepancy has to do with
# the method for estimating between-study heterogeneity and the use of inflated CIs
