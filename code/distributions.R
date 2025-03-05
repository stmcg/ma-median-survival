x <- seq(from = 0, to = 100, length.out = 1e3)
f_exp <- dexp(x, rate = 0.025)
f_weibull <- dweibull(x, 2, 35)

pi <- c(2/3, 1/3); shape <- c(2, 1.5); scale <- c(20, 50)
f_mixture <- pi[1] * dweibull(x, shape[1], scale[1]) + pi[2] * dweibull(x, shape[2], scale[2])

pdf('../results/Distributions.pdf', width = 6, height = 5)
par(mar = c(5.5, 5, 4.1, 2.1), tcl = -0.3, mgp = c(3, 0.7, 0))
plot(x, f_exp, type = 'l', col = '#1b9e77', lwd = 2, 
     ylab = '', xlab = 'Event time', 
     ylim = c(0, 0.035), 
     las = 1, cex.lab = 1.2, cex.axis = 1.1)
mtext("Density", side = 2, line = 3.8, cex = 1.2)  # Adjust line spacing for the label
lines(x, f_weibull, col = '#d95f02', lwd = 2, lty = 2)
lines(x, f_mixture, col = '#7570b3', lwd = 2, lty = 6)
legend(
  "topright", legend = c("Exponential", "Weibull", "Weibull Mixture"), 
  col = c('#1b9e77', '#d95f02', '#7570b3'), lwd = 2, lty = c(1, 2, 6), 
  bty = "n", inset = 0.02, cex = 1.1
)
dev.off()


pdf('../results/Distributions-Skew.pdf', width = 6, height = 5)
par(mar = c(5.5, 5, 4.1, 2.1), tcl = -0.3, mgp = c(3, 0.7, 0))
plot(x, dweibull(x, 1/3, 35), type = 'l', col = '#e6c27a', lwd = 3,      
     ylab = '', xlab = 'Event time', 
     las = 1, cex.lab = 1.2, cex.axis = 1.1)
lines(x, dweibull(x, 2/3, 35), col = '#729ece', lwd = 3, lty = 2)
mtext("Density", side = 2, line = 3.5, cex = 1.2) 
legend(
  "topright", legend = c("Weibull(2/3, 35)", "Weibull(1/3, 35)"), 
  col = c('#729ece', '#e6c27a'), lwd = 3, lty = c(2, 1), 
  bty = "n", inset = 0.02, cex = 1.1
)
dev.off()


bowley <- function(q1, q2, q3){
  return(((q3 - q2) - (q2 - q1)) / (q3 - q1))
}
quants <- quantile(rweibull(1e6, 1/3, 35), probs = c(0.25, 0.5, 0.75))
bowley(quants[1], quants[2], quants[3])

library('EnvStats')
skewness(rweibull(1e7, 1/3, 35))
skewness(rweibull(1e6, 2/3, 35))
