getwd()
setwd("C:/Users/koh99/바탕 화면/시계열 for github")

df<-read.csv('apart.csv')
head(df)
colnames(df)<-c('year','month','value')

df.ts<-ts(df$value,frequency = 12,start=c(2006,01))
plot(df.ts)


#### variance stationary ####
# box-cox transformation
library(MASS)
bc<-boxcox(df.ts~time(df.ts))
print(bc)
lam<-bc$x[which.max(bc$y)]
plot(df.ts^lam)

# log transformation
par(mfrow=c(3,1))
plot(df.ts)
plot(df.ts^lam)
plot(log(df.ts))
dev.off() 

#### Mean stationary ####
# detrending
DT1<-lm(log(df.ts)~time(df.ts))$residuals
plot.ts(DT1)
lines(lowess(DT1),col="blue")

x<-time(df.ts)
DT2<-lm(log(df.ts)~x+I(x^2))$residuals
plot.ts(DT2)
lines(lowess(DT2),col="blue")

DT3<-lm(log(df.ts)~x+I(x^2)+I(x^3))$residuals
plot.ts(DT3)
lines(lowess(DT3),col="blue")

# seasonal adjustment
# 1) decompose
start(df.ts)
DT.ts<-ts(DT2,start=start(df.ts),frequency = 12)
plot(decompose(DT.ts,type="additive"))

dec<-decompose(DT.ts,type='additive')
SA<-dec$x-dec$seasonal
plot(SA)

# 2) regression
xm<-1:length(df.ts)
x1<-xm%%12==1 ; x2<-xm%%12==2 ; x3<-xm%%12==3
x4<-xm%%12==4 ; x5<-xm%%12==5 ; x6<-xm%%12==6
x7<-xm%%12==7 ; x8<-xm%%12==8 ; x9<-xm%%12==9
x10<-xm%%12==10 ; x11<-xm%%12==11

df.season<-lm(log(df.ts)~xm+I(xm^2)+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11)$residuals
plot.ts(df.season)

#### differencing ####
par(mfrow=c(2,2))
plot(log(df.ts))
plot(diff(log(df.ts),lag=1,differences = 1))
plot(diff(log(df.ts),lag=12,differences = 1))
plot(diff(diff(log(df.ts),lag=12,differences = 1),lag=1,differences = 1))
dev.off()

