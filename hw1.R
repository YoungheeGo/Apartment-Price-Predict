getwd()
setwd("C:/Users/koh99/바탕 화면/시계열 for github")

df<-read.csv('apart.csv')
head(df)
colnames(df)<-c('year','month','value')

df.ts<-ts(df$value,frequency = 12,start=c(2006,01))
time<-1:nrow(df)

plot(time,df.ts)

reg1<-lm(df.ts~time)
summary(reg1)
abline(reg1,col='red')

reg2<-lm(df.ts~time+I(time^2))
summary(reg2)
lines(reg2$fitted.values,col='blue')

reg3<-lm(df.ts~time+I(time^2)+I(time^3))
summary(reg3)
lines(reg3$fitted.values,col='green')


predict(reg3,newdata=data.frame(time=187), interval="confidence") # 2021.07
predict(reg3,newdata=data.frame(time=194), interval="confidence") # 2022.01
