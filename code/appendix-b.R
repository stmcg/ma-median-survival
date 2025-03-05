rm(list = ls())

library('xtable')

alpha1 <- c(0.025, 0.03, 0.035, 0.04, 0.045, 0.049)
alpha2 <- 0.05 - alpha1

k1 <- qnorm(1 - alpha1)
k2 <- qnorm(1 - alpha2)

print(xtable(
  cbind(alpha1, alpha2, k1, k2, 1 / (k1 + k2)), digits = c(0, 3, 3, 2, 2, 2)
), include.rownames = F)

alpha <- 0.05
alpha1_all <- seq(from = 0, to = alpha, length.out = 1000)
alpha2_all <- alpha - alpha1_all
k1 <- qnorm(1 - alpha1_all)
k2 <- qnorm(1 - alpha2_all)

pdf('../results/intuition.pdf', width = 6, height = 5)
par(mar = c(5.5, 5, 4.1, 2.1), tcl = -0.3, mgp = c(3, 0.7, 0))
plot(alpha1_all, 1 / (k1 + k2), type = 'l', lwd = 3,
     xlab = expression(alpha[1]),
     ylab = "",
     ylim = c(0, 0.30),
     las = 1, cex.lab = 1.2, cex.axis = 1.1)
abline(h = 1 / (2 * qnorm(1 - alpha / 2)), col = 'red', lwd = 3, lty = 2)
dev.off()
