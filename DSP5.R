library(hflights)
library(data.table)
dt<-data.table(hflights)
dt[,date:=ISOdate(Year, Month, DayofMonth)]
daily<-dt[,list(N=.N,Delays=sum(ArrDelay,na.rm=TRUE),Cancelled=sum(Cancelled),Distance=mean(Distance)),by=date]
str(daily)

nts<-ts(daily$N,frequency = 7)
plot(nts)

# Arima
library(forecast)
auto.arima(nts)

auto.arima(nts, approximation=FALSE)

fit <- HoltWinters(nts)
plot(fit)

forecast(fit)

f = forecast(HoltWinters(nts),5)
f

plot(f)

cts <- ts(daily$Cancelled)
fit <- auto.arima(cts)
auto.arima(cts)

# Outliers
library(tsoutliers)
outliers <- tso(cts, tsmethod='arima', args.tsmethod=list(order=c(1,1,2)))
outliers

plot(outliers)

plot(tso(ts(daily$Cancelled)))