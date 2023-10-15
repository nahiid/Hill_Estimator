######################
##### Problem 2 ######
######################


####### MODEL A
###############
library(ismev)
library(timeSeries)

setwd("./data/")
load("a3data.RData")
a3data

lossSPX <- -1*a3data$R.SPX
lossSPX_ts <- as.timeSeries(lossSPX, charvec = a3data$Date)

#Get the Daily Maximum of each month
SPX_max_loss <- rollMonthlySeries(lossSPX_ts, period="1m", by="1m", FUN=max)

#Fit Models

#########Calculate RV Coefficient
#Create time series of RV for SPX
RV_SPX_ts <- as.timeSeries(a3data$.SPX, charvec = a3data$Date)
# Omit NULL values
RV_SPX_ts <- na.omit(RV_SPX_ts)
SPX_RV_func <- function(x) {
  result <- sqrt(sum((x)^2) / length(x))
  return(result)
}
SPX_RVcoef <- rollMonthlySeries(RV_SPX_ts, period="1m", by="1m", FUN=SPX_RV_func)


#########Calculate IV Coefficient
#Create time series of implied volatility for SPX
IV_SPX_ts <- as.timeSeries(as.double(a3data$VIX), charvec = a3data$Date)
# Omit NULL values
IV_SPX_ts <- na.omit(IV_SPX_ts)
SPX_IV_func <- function(x) {
  result <- sqrt(sum((x)^2) / length(x))
  return(result)
}
SPX_IVcoef <- rollMonthlySeries(IV_SPX_ts, period="1m", by="1m", FUN=SPX_IV_func)

###Model 0
SPX_fitgev0 = gev.fit(SPX_max_loss)
mu_0 <- SPX_fitgev$mle[1]
sigma_0 <- SPX_fitgev$mle[2]
xi_0 <- SPX_fitgev$mle[3]


###Model 1
SPX_fitgev1 = gev.fit(SPX_max_loss, ydat = SPX_RVcoef, mul = c(1))
mu_1 <- SPX_fitgev1$mle[1]
sigma_1 <- SPX_fitgev1$mle[2]
xi_1 <- SPX_fitgev1$mle[3]
#location=mu, scale=sigma, shape=xi
gev.diag(SPX_fitgev1)

###Model 2
SPX_fitgev2 = gev.fit(SPX_max_loss, ydat = cbind(SPX_RVcoef, SPX_IVcoef), mul = c(1,2))
mu_1 <- SPX_fitgev2$mle[1]
sigma_1 <- SPX_fitgev2$mle[2]
xi_1 <- SPX_fitgev2$mle[3]
gev.diag(SPX_fitgev2)








####### MODEL FTSE
###############

lossFTSE <- -1*a3data$R.FTSE
lossFTSE_ts <- as.timeSeries(lossFTSE, charvec = a3data$Date)

#Get the Daily Maximum of each month
FTSE_max_loss <- rollMonthlySeries(lossFTSE_ts, period="1m", by="1m", FUN=max)


FTSE_fitgev = gev.fit(FTSE_max_loss)


#Fit Models

#########Calculate RV Coefficient
#Create time series of RV for FTSE
RV_FTSE_ts <- as.timeSeries(a3data$.FTSE, charvec = a3data$Date)
# Omit NULL values
RV_FTSE_ts <- na.omit(RV_FTSE_ts)
FTSE_RV_func <- function(x) {
  result <- sqrt(sum((x)^2) / length(x))
  return(result)
}
FTSE_RVcoef <- rollMonthlySeries(RV_FTSE_ts, period="1m", by="1m", FUN=FTSE_RV_func)


#########Calculate IV Coefficient
#Create time series of implied volatility for FTSE
IV_FTSE_ts <- as.timeSeries(as.double(a3data$IV.FTSE), charvec = a3data$Date)
# Omit NULL values
IV_FTSE_ts <- na.omit(IV_FTSE_ts)
FTSE_IV_func <- function(x) {
  result <- sqrt(sum((x)^2) / length(x))
  return(result)
}
FTSE_IVcoef <- rollMonthlySeries(IV_FTSE_ts, period="1m", by="1m", FUN=FTSE_IV_func)



