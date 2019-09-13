#install.packages("ggplot2") #-- do this only once 
library(forecast)
library(ggplot2)

TumblrPeople<-read.csv(file.choose(), header=TRUE, sep=",")

TumblrPeople_uts <- ts(TumblrPeople$People,start= c(2010, 03), frequency=12) # ts function defines the dataset as timeseries starting April 2010 and having seasonality of frequency 12 (monthly) 
plot(TumblrPeople_uts/1000000)

fit <- decompose(TumblrPeople_uts, type="multiplicative") #decompose using "classical" method, multiplicative form
plot(fit)

fit <- decompose(TumblrPeople_uts, type="additive") #decompose using "classical" method, additive form
plot(fit)

fit <- stl(TumblrPeople_uts, t.window=12, s.window="periodic") #decompose using STL (Season and trend using Loess)
plot(fit)

plot(TumblrPeople_uts)
(TumblrPeople_uts/lag(TumblrPeople_uts, k=-12))-1

growth_rates_cagr_37 <- (TumblrPeople_uts[38]/TumblrPeople_uts[1])^(1/38)-1
growth_rates_cagr_12 <- (TumblrPeople_uts[38]/TumblrPeople_uts[38-12])^(1/12)-1

# Create exponential smoothing models: additive vs multiplicative noise (first A vs M), additive vs multiplicative trend (second A vs M) and no seasonality vs automatic detection (third N vs Z) trend and no seasonlity (AAN), multiplicative (MMN)
TumblrPeople_AAN <- ets(TumblrPeople_uts, model="AAN", damped=FALSE)
TumblrPeople_AAZ <- ets(TumblrPeople_uts, model="AAZ", damped=FALSE)
TumblrPeople_MMN <- ets(TumblrPeople_uts, model="MMN", damped=FALSE)
TumblrPeople_MMZ <- ets(TumblrPeople_uts, model="MMZ", damped=FALSE)
TumblrPeople_AAN_D <- ets(TumblrPeople_uts, model="AAN", damped=TRUE)
TumblrPeople_AAZ_D <- ets(TumblrPeople_uts, model="AAZ", damped=TRUE)
TumblrPeople_MMN_D <- ets(TumblrPeople_uts, model="MMN", damped=TRUE)
TumblrPeople_MMZ_D <- ets(TumblrPeople_uts, model="MMZ", damped=TRUE)
TumblrPeople_ZZZ <- ets(TumblrPeople_uts, model="MMZ", damped=FALSE)
TumblrPeople_ZZZ_D <- ets(TumblrPeople_uts, model="MMZ", damped=TRUE)

# Create their prediction "cones" for 115 months (30 years) into the future with quintile confidence intervals
TumblrPeople_AAN_pred <- forecast(TumblrPeople_AAN, h=115, level=c(0.8, 0.95))
TumblrPeople_AAZ_pred <- forecast(TumblrPeople_AAZ, h=115, level=c(0.8, 0.95))
TumblrPeople_MMN_pred <- forecast(TumblrPeople_MMN, h=115, level=c(0.8, 0.95))
TumblrPeople_MMZ_pred <- forecast(TumblrPeople_MMZ, h=115, level=c(0.8, 0.95))
TumblrPeople_AAN_D_pred <- forecast(TumblrPeople_AAN_D, h=115, level=c(0.8, 0.95))
TumblrPeople_AAZ_D_pred <- forecast(TumblrPeople_AAZ_D, h=115, level=c(0.8, 0.95))
TumblrPeople_MMN_D_pred <- forecast(TumblrPeople_MMN_D, h=115, level=c(0.8, 0.95))
TumblrPeople_MMZ_D_pred <- forecast(TumblrPeople_MMZ_D, h=115, level=c(0.8, 0.95))
TumblrPeople_ZZZ_pred <- forecast(TumblrPeople_MMN_D, h=115, level=c(0.8, 0.95))
TumblrPeople_ZZZ_D_pred <- forecast(TumblrPeople_MMZ_D, h=115, level=c(0.8, 0.95))

# Compare the prediction "cones" visually
par(mfrow=c(2,4)) # This command sets the plot window to show 1 row of 4 plots
plot(TumblrPeople_AAN_pred, xlab="Year", ylab="Predicted Growth", ylim=c(1,700000000))
plot(TumblrPeople_AAZ_pred, xlab="Year", ylab="Predicted Growth", ylim=c(1,700000000))
plot(TumblrPeople_AAN_D_pred, xlab="Year", ylab="Predicted Growth",ylim=c(1,700000000))
plot(TumblrPeople_MMN_D_pred, xlab="Year", ylab="Predicted Growth",ylim=c(1,700000000))
plot(TumblrPeople_AAZ_D_pred, xlab="Year", ylab="Predicted Growth",ylim=c(1,700000000))
plot(TumblrPeople_MMZ_D_pred, xlab="Year", ylab="Predicted Growth",ylim=c(1,700000000))
plot(TumblrPeople_ZZZ_pred, xlab="Year", ylab="Predicted Growth",ylim=c(1,700000000))
plot(TumblrPeople_ZZZ_D_pred, xlab="Year", ylab="Predicted Growth",ylim=c(1,700000000))

#Create a trigonometric box-cox autoregressive trend seasonality (TBATS) model
TumblrPeople_tbats <- tbats(TumblrPeople_uts)
TumblrPeople_tbats_pred <-forecast(TumblrPeople_tbats, h=115, level=c(0.8, 0.95))
par(mfrow=c(1,1))
plot(TumblrPeople_tbats_pred, xlab="Year", ylab="Predicted Electric Rate")

par(mfrow=c(1,3)) # Lets look at the three models with seasonality on one graph on the same scale
plot(TumblrPeople_AAZ_pred, xlab="Year", ylab="Predicted Electric Rate", ylim=c(0,0.4))
plot(TumblrPeople_MMZ_pred, xlab="Year", ylab="Predicted Electric Rate", ylim=c(0,0.4))
plot(TumblrPeople_tbats_pred, xlab="Year", ylab="Predicted Electric Rate", ylim=c(0,0.4))

par(mfrow=c(1,1)) # Lets look at them one-by-one
plot(TumblrPeople_AAZ_pred, xlab="Year", ylab="Predicted Electric Rate")
plot(TumblrPeople_MMZ_pred, xlab="Year", ylab="Predicted Electric Rate")
plot(TumblrPeople_tbats_pred, xlab="Year", ylab="Predicted Electric Rate")

# Lets look at what our models actually are
TumblrPeople_AAZ
TumblrPeople_MMZ
TumblrPeople_tbats
