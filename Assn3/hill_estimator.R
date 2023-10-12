# Example to see how the functions work

library("fExtremes")
library("timeSeries")
danish <- as.timeSeries(data(danishClaims)) # Load Data
o1 <- hillPlot(danish)
detach("package:fExtremes")
library("evir")
o2 <- hill(danish)
detach("package:evir")



#Problem 1

set.seed(123)

# Model A : generate standard Cauchy data
cauchy_data = rcauchy(1000)

# Plot data
plot(cauchy_data, xlab = "t", ylab = "X_t")

# Hill plot
library(evir)
hill(cauchy_data)
