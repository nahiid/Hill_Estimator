######################
##### Problem 2 ######
######################
library(timeSeries)

setwd("./data/")
load("a3data.RData")
a3data

lossSPX <- -1*a3data$R.SPX
  
lossSPX_ts <- as.timeSeries(lossSPX, charvec = a3data$Date)

#Get the Daily Maximum of each month
SPX_max_loss <- rollMonthlySeries(lossSPX_ts, period="1m", by="1m", FUN=max)

SPX_max_loss


lossFTSE <- -1*a3data$R.FTSE

lossFTSE_ts <- as.timeSeries(lossFTSE, charvec = a3data$Date)

#Get the Daily Maximum of each month
FTSE_max_loss <- rollMonthlySeries(lossFTSE_ts, period="1m", by="1m", FUN=max)






