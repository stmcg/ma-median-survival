rm(list = ls())
set.seed(1234)
n <- 1e7

# Ratio is true
tausq <- 0.03
random_effect <- rnorm(n, mean = 0, sd = sqrt(tausq))
lambda <- 0.025 / exp(random_effect)
dom_high <- log(2) / lambda - log(2) / 0.025

tausq <- 0.01
random_effect <- rnorm(n, mean = 0, sd = sqrt(tausq))
lambda <- 0.025 / exp(random_effect)
dom_low <- log(2) / lambda - log(2) / 0.025

density_dom_high <- density(dom_high)
density_dom_low <- density(dom_low)

# Difference is true
tausq <- 12
random_effect <- rnorm(n, mean = 0, sd = sqrt(tausq))
lambda <- log(2) / (random_effect + log(2) / 0.025)
rom_high <- log(0.025 / lambda)

tausq <- 4
random_effect <- rnorm(n, mean = 0, sd = sqrt(tausq))
lambda <- log(2) / (random_effect + log(2) / 0.025)
rom_low <- log(0.025 / lambda)

density_rom_high <- density(rom_high)
density_rom_low <- density(rom_low)

cols <- c('#117733', '#882255')

pdf('../results/Distributions-TrueEffects-DOM.pdf', width = 4.8, height = 4.8)
par(mar = c(5.5, 4.7, 4.1, 2.1), tcl = -0.3, mgp = c(3, 0.7, 0))
plot(density_dom_high$x, density_dom_high$y, type = 'l', ylim = c(0, 0.15), 
     col = cols[1], lwd = 2, xlim = c(-12, 12), main = 'Distribution of True Differences of Medians', 
     ylab = '', xlab = 'True Difference of Medians', 
     las = 1, cex.lab = 1.2, cex.axis = 1.1)
mtext("Density", side = 2, line = 3.6, cex = 1.2)
lines(density_dom_low$x, density_dom_low$y, col = cols[2], lwd = 2, lty = 2)
dev.off()

pdf('../results/Distributions-TrueEffects-ROM.pdf', width = 4.7, height = 4.7)
par(mar = c(5.5, 4.1, 4.1, 2.1), tcl = -0.3, mgp = c(3, 0.7, 0))
plot(density_rom_high$x, density_rom_high$y, type = 'l', ylim = c(0, 6), 
     col = cols[1], lwd = 2, xlim = c(-0.4, 0.4), main = 'Distribution of True Log Ratios of Medians', 
     ylab = '', xlab = 'True Log Ratio of Medians', 
     las = 1, cex.lab = 1.2, cex.axis = 1.1)
mtext("Density", side = 2, line = 2.6, cex = 1.2)
lines(density_rom_low$x, density_rom_low$y, col = cols[2], lwd = 2, lty = 2)
dev.off()