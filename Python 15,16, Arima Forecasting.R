install.packages("TSA")
library(tseries)
library(TSA)
library(forecast)

returns=read.csv("Returns.csv")
head(returns)
original=returns
attach(original)
attach(returns)
nrow(returns)
#break the data
(breakpoint=floor(nrow(returns)*.95))
#timeseries
(Returns=ts(returns[c(1:breakpoint),],frequency = 1))

#2. Identify Trends & Seasonality
#KPSS
#Ho: Data is stationary
#Ha: Data is not stationary
kpss.test(Returns)
#Data is stationary since p-value is greater than 0.05

#3. identify p,q,d
acf.returns=acf(returns[c(1:breakpoint),],main="ACF Plot")
#q can be 3,5 and 8
pacf.returns=pacf(returns[c(1:breakpoint),],main="PACF Plot")
#p can be 3
#d=0, because it is already stationary
#models can be (3,0,3), (3,0,5), (3,0,8)
#residual
(fit1=Arima(Returns,order=c(3,0,3)))
(fit2=Arima(Returns,order=c(3,0,5)))
(fit3=Arima(Returns,order=c(3,0,8)))

acf(residuals(fit2))
pacf(residuals(fit2))

Box.test(residuals(fit2),lag = 1,type="Ljung")

#whatever the data is fitted properly, residuals should be independtly distributed, mean should be sigma square and mean should be 0

pred=forecast(fit2,h=5) #h=5 no of days to be ptredicted
plot(pred,xlab="Lags",ylab = "Returns")
pred$mean
lines(original,col="red")

#create a new time series with the forecasted values
new_series=pred$fitted
#plot the original returns data
plot(Returns,main="Original versus fitted series",col="blue")
#plot the forecasted returns data
lines(new_series,col="red")

fit2

fit_arma=auto.arima(Returns,trace=TRUE,test="kpss",ic="aic") #dont use it because its heavily tilted towards MA

(fit4=Arima(Returns,order=c(4,0,1)))
acf(residuals(fit4))
pacf(residuals(fit4))

Box.test(residuals(fit4),lag = 1,type="Ljung")
pred1=forecast(fit4,h=5) #h=5 no of days to be ptredicted
plot(pred1,xlab="Lags",ylab = "Returns")
pred1$mean
lines(original,col="red")

#create a new time series with the forecasted values
new_series=pred$fitted
#plot the original returns data
plot(Returns,main="Original versus fitted series",col="blue")
#plot the forecasted returns data
lines(new_series,col="red")

fit4

