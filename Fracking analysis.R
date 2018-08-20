#Lauren Kendrick
#This script estimates the impact of the number of fracking wells per county on the average price of single family homes in Ohio and Pennsylvania between 1996 and 2013

#Get libraries
library(MASS)
library(forecast) 
library(stats)
library(car)
library(nlme)
library(reshape2)

#Import data and reshape for panel data analysis
setwd("c://Users/kendrick/Documents/Sandy Career Services/Code Samples")
data=read.csv("housingprices.csv",sep=",")
colnames(data)[1]<-'RegionName'
interest=read.csv("interest rate.csv",sep=",")
colnames(interest)[1]<-'t'

#Reshape to long panel data
head(data,n=0)
popdata<- data[,c(1:23)]
welldata<- data[,c(1:5,24:236)]
avgpricedata<- data[,c(1:5,237:449)]
poplong<-melt(popdata,id.vars=c('RegionName','City','State','Metro','County'),variable.name='year',value.name='County_population')
poplong$year<-substr(poplong$year,19,23)
welllong<-melt(welldata,id.vars=c('RegionName','City','State','Metro','County'),variable.name='date',value.name='County_wells')
welllong$date<-substring(welllong$date,26)
x<-colsplit(welllong$date,"_1_",c('month','year'))
welllong<-cbind(welllong,x)
avgpricelong<-melt(avgpricedata,id.vars=c('RegionName','City','State','Metro','County'),variable.name='date',value.name='Avg_price')
avgpricelong$year<-substr(avgpricelong$date,2,5)
avgpricelong$month<-substr(avgpricelong$date,7,8)
avgpricelong$date<-NULL
datalong<-cbind(welllong,avgpricelong)
datalong<-datalong[,c(1:5,7:9,15)]
datafinal<-merge(poplong,datalong,by=c('RegionName','City','State','Metro','County','year'),all.y=TRUE)
datafinal$t<-(as.numeric(datafinal$year)-1996)*12+datafinal$month-3
datafinal<-merge(datafinal,interest,by='t')

#Explore time series behaviour for trend, seasonality?
#Should we transform avg house price to make it more normal?
boxCox(datafinal$Avg_price~datafinal$t, data=datafinal)
lambda <- BoxCox.lambda(datafinal$Avg_price, method="loglik") #-0.15
datafinal$Avg_price_trans<-((datafinal$Avg_price^-0.15)-1)/(-0.15)


#Model avg house price, accounting for correlated errors and time series stability


#First check naive linear models
#Naive linear model with only time
datafinal$t2<-datafinal$t^2
datafinal$t3<-datafinal$t^3
datafinal$t4<-datafinal$t^4

lmfit00<-lm(Avg_price_trans~City,data=datafinal)
summary(lmfit00)#R-squared is 0.8328
lmfit0<-lm(Avg_price_trans~t+t2+t3+t4,data=datafinal)
summary(lmfit0)#R-squared is 0.1097
lmfit1<-lm(Avg_price_trans~City+t+t2+t3+t4,data=datafinal)
summary(lmfit1)#R-squared is 0.9425

lmfit<-lm(Avg_price_trans~City+County_population+County_wells+interestrate+t+t2+t3+t4,data=datafinal)
summary(lmfit) #R-squared is 0.9495
#Coefficient of County_wells (# Fracking wells per county) is +2.419e-04

#Check residuals for correlation
#Box-Ljung white noise test
Box.test(residuals(lmfit), lag=12, type="Ljung-Box")
#p-value < 2.2e-16 rejects hypothesis that residuals are white noise
#So the residuals are correlated and the coefficients of lmfit are meaningless
lmresid<-ts(residuals(lmfit),f=12,start=c(1996,4))
lmresfit<-auto.arima(lmresid)
lmresfit #ARIMA(2,1,0)


#Try GLM model to account for residual correlation structure
#Check that GLS reproduces lm model
glsfit0 = gls(Avg_price_trans~City+County_population+County_wells+interestrate+t+t2+t3+t4, method="ML",data=datafinal)
print(summary(glsfit0))
Box.test(residuals(glsfit0), lag=12, type="Ljung-Box")#166.25
logLik(glsfit0) #81951.94
AIC(summary(glsfit0)) #-163613.9
acf(residuals(glsfit0))
pacf(residuals(glsfit0))
coef(glsfit0)

#Try gls with AR(2) process
glsfit1 = gls(Avg_price_trans~City+County_population+County_wells+interestrate+t+t2+t3+t4, correlation=corAR1(form = ~ 2 | City), method="ML",data=datafinal)
print(summary(glsfit1))
Box.test(residuals(glsfit1), lag=12, type="Ljung-Box")#94.972
logLik(glsfit1) #113630.1
AIC(summary(glsfit1)) #-226968.1
#Better model but residuals are still correlated at p=5.3e-15

#Calculate first difference, since we want to model an ARIMA (2,1,0) process
cities=unique(datafinal$City)
datafinal$Avg_price_trans_d=0
for (r in cities){
for (a in c(2:213)) {
datafinal$Avg_price_trans_d[datafinal$City==r & datafinal$t==a]<-datafinal$Avg_price_trans[datafinal$City==r & datafinal$t==a]-datafinal$Avg_price_trans[datafinal$City==r & datafinal$t==a-1]
}
}
glsfit2 = gls(Avg_price_trans_d~City+County_population+County_wells+interestrate+t+t2+t3+t4, correlation=corAR1(form = ~ 2 | City), method="ML",data=datafinal,na.action=na.omit)
Box.test(residuals(glsfit2), lag=12, type="Ljung-Box")#24.686, p>0.01
logLik(glsfit2) #106624.5
AIC(summary(glsfit2)) #-212957.1
coef(glsfit2)
summary(glsfit2)$tTable
#Coefficient of County_wells (# of fracking wells per county) is 2.354289e-05
#Not a perfect model, but much closer to stationary residuals
#Suggests that an additional fracking well raises the Box-Cox transformed average sale price of homes in the county
#So increased fracking activity correlates to higher local house prices.
