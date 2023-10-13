# Example to see how the functions work

library("fExtremes")
library("timeSeries")
danish <- as.timeSeries(data(danishClaims)) # Load Data
o1 <- hillPlot(danish)
detach("package:fExtremes")
library("evir")
o2 <- hill(danish)
detach("package:evir")

######################
##### Problem 1 ######
######################

set.seed(123)

############
#### Model A : generate standard Cauchy data
cauchy_data = rcauchy(1000)

# Scatter Plot 
plot(cauchy_data, xlab = "t", ylab = "X_t", main="Standard Cauchy Data Plot")

# Box Plot
boxplot(cauchy_data, main = "Box Plot of Cauchy Data", ylab = "X_t")

# Hill plot
library(evir)
hill(cauchy_data)

# Add line to hill plot
abline(h = 1, col = "pink")

############
#### Model B


# Set the number of data points to simulate
n <- 1000

# Define the quantile function (inverse of the cumulative distribution function)
quantile_function <- function(p) {
  x <- exp(1 / p + 1)  # Inverse of F(x) = 1 / (x log(x))
  return(x)
}

# Generate random uniform values between 0 and 1
u <- runif(n)

# Apply the quantile function to obtain the simulated data
simulated_data <- quantile_function(u)

# The 'simulated_data' now contains your 1000 IID data points

# You can plot the histogram to visualize the distribution
plot(simulated_data, xlab = "t", ylab = "X_t", main="Model 2")

############
#### Model C :
############
library(evir)
set.seed(123)

## Model C - AR(1) with phi {0.95,0.5,0.15}
## noises from symmetric distribution with exact Pareto tail(10,0.5^0.1)
#  The below vectors will store the time series data
AR_data_0.95 = c(rep(0,1001))
AR_data_0.5 = c(rep(0,1001))
AR_data_0.15 = c(rep(0,1001))
 
#min and max to ensure that x>=1
pareto = (2*(1-runif(1000, min = 0.5, max = 1)))^(-1/10)
# Make the Pareto noise symmetric by randomly multiplying each value by 1 or -1 with equal probability.
pareto = sample(c(1,-1),1000,replace = TRUE)*pareto
# same noise applied to all three to isolate impact of phi
for(i in 2:1001){
  AR_data_0.95[i] = 0.95*AR_data_0.95[i-1]+pareto[i-1]
  AR_data_0.5[i] = 0.5*AR_data_0.5[i-1]+pareto[i-1]
  AR_data_0.15[i] = 0.15*AR_data_0.15[i-1]+pareto[i-1]
}

AR_data_0.95 = AR_data_0.95[2:1001]
AR_data_0.5 = AR_data_0.5[2:1001]
AR_data_0.15 = AR_data_0.15[2:1001]

### Scatter plot for each model
plot(AR_data_0.95, xlab = "t", ylab = "X_t")
plot(AR_data_0.5, xlab = "t", ylab = "X_t")
plot(AR_data_0.15, xlab = "t", ylab = "X_t")

#AR_data_0.5 is fairly similar to the normal

### Hill plot for each model
hill(AR_data_0.95)
hill(AR_data_0.5)
hill(AR_data_0.15)


detach("package:evir")











