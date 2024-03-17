getwd()
setwd("C:/Users/koh99/바탕 화면/시계열 for github")
dev.off()

df<-read.csv('apart.csv')
head(df)
colnames(df)<-c('year','month','value')

df.ts<-ts(df$value,frequency = 12,start=c(2006,01))
plot(df.ts)

#### Box-jeckins ####
# detrending
x<-time(df.ts)
DT2<-lm(df.ts~x+I(x^2))$residuals
lines(lowess(DT2),col="blue")
DT.ts<-ts(DT2,start=start(df.ts),frequency = 12)
plot(DT.ts)
# differencing degree
library(tseries)
par(mfrow=c(3,1))
plot(DT.ts)
acf(DT.ts)
pacf(DT.ts)
adf.test(DT.ts)
# dirrerncing
kpss.test(diff(DT.ts))
par(mfrow=c(3,1))
plot(diff(DT.ts))
acf(diff(DT.ts))
pacf(diff(DT.ts))

#### model Inference ####
# parameter
ma1<-Arima(DT.ts,order=c(0,1,1),include.mean = TRUE,include.drift=TRUE)
ar1<-Arima(DT.ts,order=c(1,1,0),include.mean = TRUE,include.drift=TRUE)
arma11<-Arima(DT.ts,order=c(1,1,1),include.mean = TRUE,include.drift=TRUE)
arma11

# over-fitting model
arma12<-Arima(DT.ts,order=c(1,1,2),include.mean = TRUE,include.drift=TRUE)
arma21<-Arima(DT.ts,order=c(2,1,1),include.mean = TRUE,include.drift=TRUE)
arma12 ; arma21

#### Diagnosis model ####
# residual analysis
res<-residuals(arma11)
par(mfrow=c(3,1))
plot(res)
acf(res)
pacf(res)
Box.test(res,lag=10,type="Ljung-Box",fitdf=2)

#### Prediction ####
forecast(arma11,h=12)
plot(forecast(arma11,h=12))
