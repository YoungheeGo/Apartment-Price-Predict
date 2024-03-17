getwd()
setwd("C:/Users/koh99/바탕 화면/시계열 for github")

df<-read.csv('apart.csv')
head(df)
colnames(df)<-c('year','month','value')

df.ts<-ts(df$value,frequency = 12,start=c(2006,01))
time<-1:nrow(df)

#### Moving average period ####
library(TTR)
m.vec<-36:48 # By kitchin cycles theory
sma.mse.vec<-c()
for (m in m.vec){
  sma.m <- c(NA,SMA(df.ts,n=m))
  end<-length(sma.m)
  sma.mse<-mean((df.ts-sma.m[-end])^2,na.rm=TRUE) 
  sma.mse.vec<-c(sma.mse.vec,sma.mse)
}
m.vec[which.min(sma.mse.vec)]


#### Simple Moving Average (SMA)
plot(df.ts)
sma.vec2<-SMA(df.ts,n=36)
lines(sma.vec2,col='red')

#### MA ####
ma.cen<-ma(df.ts,order=36, centre=TRUE)
head(ma.cen,20)
tail(ma.cen,20)
lines(ma.cen,col='blue')

#### lowess ####
# Locally Weighted Regression and Smoothing
low<-lowess(df.ts,f=1/3)
lines(low,col='green')


#### Prediction -mean ####
df.fore<-c(df.ts,rep(NA,20))
df.fore.ts<-ts(df.fore,start=c(2006,1),frequency = 12)
plot(df.fore.ts)
abline(v=2021.5,col='red',lty=2)
abline(h=mean(df.ts),col='green')

#### Pred - SMA ####
sma.36<-SMA(df.ts,n=36)
n<-length(df.ts)
sma.fore.ts<-ts(c(NA,sma.36,rep(sma.36[n],19)),start=c(2006,1),frequency = 12)
lines(sma.fore.ts,col='blue')

#### Pred - Simple Exponential Smoothing ####
es.auto<- HoltWinters(df.ts,beta=FALSE,gamma=FALSE)
lines(ts(c(NA,es.auto$fitted),start=c(2006,1),frequency = 12),col='purple')
plot(forecast(es.auto))

#### Pred - SMA, DMA ####
dma.36<-SMA(sma.36,n=36)
ma.trend<-c(NA,(2*sma.36-dma.36)+2/35*(sma.36-dma.36),
            (2*sma.36[n]-dma.36[n])+2/35*(sma.36[n]-dma.36[n])*(2:23))
plot(df.fore.ts)
abline(v=2021.5,col='red',lty=2)
abline(h=mean(df.ts),col='green')
lines(sma.fore.ts,col='blue')
lines(ts(ma.trend,start=c(2006,1),frequency = 12),col='red')

