rm(list = ls())
library('xtable')

load('../results/sim-res-studylevel.RData')
cols <- c('#FFDAB9', '#8FBC8F', '#A892CC')
# cols <- c('#ADD8E6', '#8FBC8F')


mytitle <- paste0('../results/StudyLevel.pdf')
pdf(mytitle, width = 8, height = 8)
par(mfrow = c(2, 2))

for (distribution in c('exponential', 'weibull')){
  for (censoring_distribution in all_censoring_distribution){
    my_ind <- which(sim_scenarios[, 'distribution'] == distribution & 
                      sim_scenarios[, 'censoring_distribution'] == censoring_distribution)
    
    if (distribution == 'exponential'){
      main_pt1 <- 'Exponential Event Times'
    } else if (distribution == 'weibull'){
      main_pt1 <- 'Weibull Event Times'
    }
    if (censoring_distribution == 'uniform'){
      main_pt2 <- 'Uniform Censoring'
    } else if (censoring_distribution == 'exponential'){
      main_pt2 <- 'Exponential Censoring'
    }
    mydat <- cbind(res_bc_log[, my_ind[1]], res_bc_loglog[, my_ind[1]], res_boot[, my_ind[1]],
                   res_bc_log[, my_ind[2]], res_bc_loglog[, my_ind[2]], res_boot[, my_ind[2]],
                   res_bc_log[, my_ind[3]], res_bc_loglog[, my_ind[3]], res_boot[, my_ind[3]],
                   res_bc_log[, my_ind[4]], res_bc_loglog[, my_ind[4]], res_boot[, my_ind[4]])
    boxplot(mydat,
            main = paste0(main_pt1, '\n ', main_pt2),
            col = rep(cols, times = 4),  # Soft colors
            xaxt = 'n',
            ylab = "Estimated SE", 
            outline = FALSE, 
            cex.main = 1.25, 
            cex.lab = 1.15, 
            cex.axis = 1.1)                     # Avoids outliers cluttering
    points(1:12, 
           rep(sei_true[my_ind], each = 3), 
           col = 'red', pch = 19, cex = 1.25)
    axis(1, at = c(2, 5, 8, 11), labels = paste0("n=", all_n_subjects), cex.axis = 1.1)
    
  }
}
dev.off()


mytitle <- paste0('../results/StudyLevel-mixture.pdf')
pdf(mytitle, width = 10, height = 5)
par(mfrow = c(1, 2))

for (censoring_distribution in all_censoring_distribution){
  my_ind <- which(sim_scenarios[, 'distribution'] == 'mixture' & 
                    sim_scenarios[, 'censoring_distribution'] == censoring_distribution)
  
  main_pt1 <- 'Weibull Mixture Event Times'
  if (censoring_distribution == 'uniform'){
    main_pt2 <- 'Uniform Censoring'
  } else if (censoring_distribution == 'exponential'){
    main_pt2 <- 'Exponential Censoring'
  }
  mydat <- cbind(res_bc_log[, my_ind[1]], res_bc_loglog[, my_ind[1]], res_boot[, my_ind[1]],
                 res_bc_log[, my_ind[2]], res_bc_loglog[, my_ind[2]], res_boot[, my_ind[2]],
                 res_bc_log[, my_ind[3]], res_bc_loglog[, my_ind[3]], res_boot[, my_ind[3]],
                 res_bc_log[, my_ind[4]], res_bc_loglog[, my_ind[4]], res_boot[, my_ind[4]])
  boxplot(mydat,
          main = paste0(main_pt1, '\n ', main_pt2),
          col = rep(cols, times = 4),  # Soft colors
          xaxt = 'n',
          ylab = "Estimated SE", 
          outline = FALSE, 
          cex.main = 1.25, 
          cex.lab = 1.15, 
          cex.axis = 1.1)                     # Avoids outliers cluttering
  points(1:12, 
         rep(sei_true[my_ind], each = 3), 
         col = 'red', pch = 19, cex = 1.25)
  axis(1, at = c(2, 5, 8, 11), labels = paste0("n=", all_n_subjects), cex.axis = 1.1)
}
dev.off()


mytitle <- paste0('../results/StudyLevel-skew.pdf')
pdf(mytitle, width = 8, height = 8)
par(mfrow = c(2, 2))

for (skew_dist in c('weibull skew 1', 'weibull skew 2')){
  for (censoring_distribution in all_censoring_distribution){
    my_ind <- which(sim_scenarios[, 'distribution'] == skew_dist & 
                      sim_scenarios[, 'censoring_distribution'] == censoring_distribution)
    
    if (skew_dist == 'weibull skew 1'){
      main_pt1 <- 'Weibull(2/3, 35) Event Times'
    } else if (skew_dist == 'weibull skew 2'){
      main_pt1 <- 'Weibull(1/3, 35) Event Times'
    }
    if (censoring_distribution == 'uniform'){
      main_pt2 <- 'Uniform Censoring'
    } else if (censoring_distribution == 'exponential'){
      main_pt2 <- 'Exponential Censoring'
    }
    
    
    mydat <- cbind(res_bc_log[, my_ind[1]], res_bc_loglog[, my_ind[1]], res_boot[, my_ind[1]],
                   res_bc_log[, my_ind[2]], res_bc_loglog[, my_ind[2]], res_boot[, my_ind[2]],
                   res_bc_log[, my_ind[3]], res_bc_loglog[, my_ind[3]], res_boot[, my_ind[3]],
                   res_bc_log[, my_ind[4]], res_bc_loglog[, my_ind[4]], res_boot[, my_ind[4]])
    boxplot(mydat,
            main = paste0(main_pt1, '\n ', main_pt2),,
            col = rep(cols, times = 4),  # Soft colors
            xaxt = 'n',
            ylab = "Estimated SE", 
            outline = FALSE, 
            cex.main = 1.25, 
            cex.lab = 1.15, 
            cex.axis = 1.1)                     # Avoids outliers cluttering
    points(1:12, 
           rep(sei_true[my_ind], each = 3), 
           col = 'red', pch = 19, cex = 1.25)
    axis(1, at = c(2, 5, 8, 11), labels = paste0("n=", all_n_subjects), cex.axis = 1.1)
  }
}
dev.off()

relative_bias_exponential <- matrix(NA, nrow = 6, ncol = 4)
relative_bias_weibull <- matrix(NA, nrow = 6, ncol = 4)
relative_bias_mixture <- matrix(NA, nrow = 6, ncol = 4)
relative_bias_weibull_skew_1 <- matrix(NA, nrow = 6, ncol = 4)
relative_bias_weibull_skew_2 <- matrix(NA, nrow = 6, ncol = 4)


for (distribution in all_distribution){
  i <- 1
  for (censoring_distribution in all_censoring_distribution){
    j <- 0
    for (n_subjects in all_n_subjects){
      j <- j + 1
      my_ind <- which(sim_scenarios[, 'distribution'] == distribution & 
                        sim_scenarios[, 'censoring_distribution'] == censoring_distribution & 
                        sim_scenarios[, 'n_subjects'] == n_subjects)
      if (distribution == 'exponential'){
        relative_bias_exponential[i, j] <- (mean(res_bc_log[, my_ind], na.rm = TRUE) - sei_true[my_ind]) / sei_true[my_ind]
        relative_bias_exponential[i+1, j] <- (mean(res_bc_loglog[, my_ind], na.rm = TRUE) - sei_true[my_ind]) / sei_true[my_ind]
        relative_bias_exponential[i+2, j] <- (mean(res_boot[, my_ind], na.rm = TRUE) - sei_true[my_ind]) / sei_true[my_ind]
      } else if (distribution == 'weibull'){
        relative_bias_weibull[i, j] <- (mean(res_bc_log[, my_ind], na.rm = TRUE) - sei_true[my_ind]) / sei_true[my_ind]
        relative_bias_weibull[i+1, j] <- (mean(res_bc_loglog[, my_ind], na.rm = TRUE) - sei_true[my_ind]) / sei_true[my_ind]
        relative_bias_weibull[i+2, j] <- (mean(res_boot[, my_ind], na.rm = TRUE) - sei_true[my_ind]) / sei_true[my_ind]
      } else if (distribution == 'mixture'){
        relative_bias_mixture[i, j] <- (mean(res_bc_log[, my_ind], na.rm = TRUE) - sei_true[my_ind]) / sei_true[my_ind]
        relative_bias_mixture[i+1, j] <- (mean(res_bc_loglog[, my_ind], na.rm = TRUE) - sei_true[my_ind]) / sei_true[my_ind]
        relative_bias_mixture[i+2, j] <- (mean(res_boot[, my_ind], na.rm = TRUE) - sei_true[my_ind]) / sei_true[my_ind]
      } else if (distribution == 'weibull skew 1'){
        relative_bias_weibull_skew_1[i, j] <- (mean(res_bc_log[, my_ind], na.rm = TRUE) - sei_true[my_ind]) / sei_true[my_ind]
        relative_bias_weibull_skew_1[i+1, j] <- (mean(res_bc_loglog[, my_ind], na.rm = TRUE) - sei_true[my_ind]) / sei_true[my_ind]
        relative_bias_weibull_skew_1[i+2, j] <- (mean(res_boot[, my_ind], na.rm = TRUE) - sei_true[my_ind]) / sei_true[my_ind]
      } else if (distribution == 'weibull skew 2'){
        relative_bias_weibull_skew_2[i, j] <- (mean(res_bc_log[, my_ind], na.rm = TRUE) - sei_true[my_ind]) / sei_true[my_ind]
        relative_bias_weibull_skew_2[i+1, j] <- (mean(res_bc_loglog[, my_ind], na.rm = TRUE) - sei_true[my_ind]) / sei_true[my_ind]
        relative_bias_weibull_skew_2[i+2, j] <- (mean(res_boot[, my_ind], na.rm = TRUE) - sei_true[my_ind]) / sei_true[my_ind]
      }
    }
    i <- i + 3
  }
}

xtable(round(100 * relative_bias_exponential), digits = 0)
xtable(round(100 * relative_bias_weibull), digits = 0)
xtable(round(100 * relative_bias_mixture), digits = 0)
xtable(round(100 * relative_bias_weibull_skew_1), digits = 0)
xtable(round(100 * relative_bias_weibull_skew_2), digits = 0)
