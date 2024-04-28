# Demo of hp(): Hodrick-Prescott filter

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Expanded plots window
par(mar=c(1,1,1,1))

# Plugging libraries and functions
source('hp.R')

# Data: GDP at constant prices, France, 1950.Q1 - 2023.Q4
load('gdp_france.Rdata')

# Trade-off between fitness and smoothness
lambda <- 1600

# Pre-CV19 sample
T0 <- 280L
z0 <- as.matrix(z[1:T0])

# Calling function
res0 <- hp(z0, 
           lambda)

res <- hp(z, 
          lambda)

aux <- rbind((100*(res0$cycle/res0$trend)), matrix(NA, 16, 1))
cycle0_ts <- ts(aux, start = z_list$z_start, frequency = z_list$z_freq)
cycle_ts <- ts(100*res$cycle/res$trend, start = z_list$z_start, frequency = z_list$z_freq)

# Graph: pre-CV19 sample vs full sample
windows()
par(mfrow = c(2,1))
plot.ts(cycle0_ts,
        main = 'Pre-CV19 sample',
        xlab = '',
        ylab = '',
        col = 'red')
plot.ts(cycle_ts,
        main = 'Full sample',
        xlab = '',
        ylab = '',
        col = 'blue')

