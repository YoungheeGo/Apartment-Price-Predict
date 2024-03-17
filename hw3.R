getwd()
setwd("C:/Users/koh99/바탕 화면/시계열 for github")

df<-read.csv('apart.csv')
head(df)
colnames(df)<-c('year','month','value')

df.ts<-ts(df$value,frequency = 12,start=c(2006,01))
plot(df.ts)


####  Decomposition Method ####
# decompose
dec.df.mul<-decompose(df.ts,type='multiplicative')
plot(dec.df.mul)

# seasonal adjustment
sa.df<-dec.df.mul$x / dec.df.mul$seasonal 

head(sa.df)

par(mfrow=c(2,1))
plot(df.ts)
plot(sa.df)
dev.off()

# prediction
tt<-1:length(sa.df)
reg<-lm(sa.df~tt)
reg

dsn.vec<-predict(reg,newdata=data.frame(tt=182:201))
dsn.vec

predict.vec<-rep(NA,length(dsn.vec))
for (i in 1:length(dsn.vec)){
  rem<-i%%12
  predict.vec[i]<-dsn.vec[i]+dec.df.mul$figure[rem+1]
}
predict.vec


#### Exponential Smoothing method ####
# simple exponential smoothing
hw1<-HoltWinters(df.ts,beta=F,gamma=F) 
forecast(hw1) 
plot(forecast(hw1))

# linear exponential smoothing
hw2<-HoltWinters(df.ts,gamma=FALSE) 
forecast(hw2)
plot(forecast(hw2))

# seasonal exponential smoothing
hw3<-HoltWinters(df.ts,seasonal='multiplicative') 
forecast(hw3)
plot(forecast(hw3))

# Compare the methods.
par(mfrow=c(3,1))

plot(forecast(hw1))
plot(forecast(hw2))
plot(forecast(hw3))

sse.vec<-rep(NA,3)
sse.vec[1]<-hw1$SSE
sse.vec[2]<-hw2$SSE
sse.vec[3]<-hw3$SSE

sse.vec

