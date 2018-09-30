install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
library("quantmod")
library("tseries")
library("timeSeries")
library("timeDate")

getSymbols("TCS.NS",from="2016-01-01")
stock1=na.locf(TCS.NS$TCS.NS.Adjusted) 

getSymbols("INFY.NS",from="2016-01-01") 
stock2=na.locf(INFY.NS$INFY.NS.Adjusted) 

ret_stock1=Delt(stock1) #calulate pt/pt-1-1
ret_stock2=Delt(stock2)

data=merge(ret_stock1,ret_stock2)
data=na.omit(data)

# spread trading
ret_stock11=round(ret_stock1+1,4) #adding 1 to every return to get total return on Rs 1 and rounding it 4 decimals

ret_stock11[1]=1 #inserted 1 in first row to get value of Rs 1 ie Normalised the prices

norm_stock1=cumprod(ret_stock11) #cumulative product multiplies 

plot(norm_stock1)


ret_stock22=round(ret_stock2+1,4)

ret_stock22[1]=1

norm_stock2=cumprod(ret_stock22)

plot(norm_stock2)

plot_new <- plot(norm_stock1, type = "l", ylim = c(0,6))
lines(norm_stock2, col = "red")

#check the spread
difference=norm_stock1-norm_stock2

plot(difference)

adf.test(difference)

mean_di=rollapply(difference,5,mean)
sd_di=rollapply(difference,5,sd)



ul=mean_di+1*sd_di #upper limit
ll=mean_di-1*sd_di #lower limit

plot(cbind(difference,ul,ll))

sig=ifelse(difference>ul,-1,ifelse(difference<ll,1,0)) 
#signal and if else condition
#-1: sell, 1:buy
sig=lag(sig,1) #lag=1 for daily
plot(sig)

sprd_rtn=ret_stock1-ret_stock2
trd_ret=sprd_rtn*sig

plot(trd_ret)

summary(trd_ret)

charts.PerformanceSummary(trd_ret) 

Return.cumulative(trd_ret)

Return.annualized(trd_ret)

chart.CumReturns(trd_ret) 

SharpeRatio(as.ts(trd_ret),Rf=0,p = 0.95, FUN = "StdDev")
