#Load Packages
library("tseries")
library("bvartools")
library(stats)
library(forecast)
library(datasets)
library(tseries)
library(xts)
library(zoo)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(urca)
library(dynlm)
library(vars)
library(strucchange)
library(MARSS)

#---------------------------------------------------------------------------------------------------------------------------
##DATA PREPROCESSING
#---------------------------------------------------------------------------------------------------------------------------
#Loading Data set 
Mtesla <- read.csv("C:\\Users\\USER\\Desktop\\Assignments Dimi\\7th Sem\\Financial Econometrics\\Take home assignment\\VARdata.csv")
head(Mtesla, 10)
Exrate <- read.csv("C:\\Users\\USER\\Desktop\\Assignments Dimi\\7th Sem\\Financial Econometrics\\Take home assignment\\Exchange Rates.csv")
head(Exrate, 10) #Second highest buyer from Tesla is China

#Missing Values
Mtesla$Date = as.Date(Mtesla$Date, format = "%m/%d/%Y")
dates = data.frame(Date = seq(as.Date('2010-12-31'), as.Date('2020-12-31'), by = 'days'))
Mtesla_merge <- merge(Mtesla,dates,by="Date", all = T)
Mtesla <-na.locf(Mtesla_merge, fromLast = TRUE)
head(Mtesla,10)

Exrate$Date = as.Date(Exrate$Date, format = "%m/%d/%Y")
Exrate$Date
dates1 = data.frame(Date = seq(as.Date('2010-12-31'), as.Date('2020-12-31'), by = 'days'))
Exrate_merge <- merge(Exrate,dates1,by="Date", all = T)
Exrate_merge
Exrate<-na.locf(Exrate_merge, fromLast = TRUE)
head(Exrate,10)

#Merge datasets
ECM_tesla<- cbind(Mtesla, Exrate)
ECM_tesla
ECM_tesla1<- ECM_tesla[-c(9)]
ECM_tesla2<- ECM_tesla1[!(ECM_tesla1$Date=="2010-12-31"),]
head(ECM_tesla2, 10)

#Variables to timeseires
##Adjusted close Tesla
Tesla_Adj.close=ECM_tesla2[,"Adj.Close.Tesla"]
Tesla_Adj.close_log = log(Tesla_Adj.close)#log transformation
Tesla_Adj_close <- ts(data = Tesla_Adj.close_log, frequency = 365, start = c(2011,1))

##Adjusted Close GM
GM_Adj.close=ECM_tesla2[,"Adj.Close.GM"]
GM_Adj.close_log = log(GM_Adj.close)#log transformation
GM_Adj_close <- ts(data = GM_Adj.close_log, frequency = 365, start = c(2011,1))

##CNY
Ex.rates=ECM_tesla2[,"CNY"]
Ex.rates_log = log(Ex.rates)#log transformation
USDCNY<- ts(data = Ex.rates_log, frequency = 365, start = c(2011,1))

#plotting

#Tesla
plot(Tesla_Adj_close, type="l", xlab="Date", ylab="Stock Price", col="blue",lwd = 2) 
par(new=T)

#GM
plot(GM_Adj_close, type="l",axes=F, xlab="", ylab="", col = "black") 
par(new=T)

#ExRate
plot(USDCNY, type="l",axes=F, xlab="", ylab="", col = "red") 
legend("topleft",c("Tesla","GM","Exrate"),fill=c("blue","black","red"))
par(new=F) 
#NO relationship to see they follow a pattern in the long-run

#---------------------------------------------------------------------------------------------------------------------------
## STATIONARITY IN SERIES
#---------------------------------------------------------------------------------------------------------------------------
#check for stationary
adf.test(Tesla_Adj_close) 
adf.test(GM_Adj_close)
adf.test(USDCNY)

##Check whether processors are I(1)
#differencing Tesla
d.tesla_adj.close=diff(Tesla_Adj_close,differences = 1)
adf.test(d.tesla_adj.close)
#differencing GM
d.GM_Adj_close=diff(GM_Adj_close,differences = 1)
adf.test(d.GM_Adj_close)
#differencing Exrate
d.USDCNY=diff(USDCNY,differences = 1)
adf.test(d.USDCNY)

#---------------------------------------------------------------------------------------------------------------------------
## VECM FOR MULTIVARIATE APPROACH 
#---------------------------------------------------------------------------------------------------------------------------
##Johansen Test on Financial Data, Multivariate Approach
v1 <- cbind(Tesla_Adj_close, GM_Adj_close,USDCNY)
colnames(v1) <- cbind("Tesla.adj.close","GM.adj.close", "ExRate")

#Lag selection
lagselect <- VARselect(v1, lag.max = 15, type = "const")
lagselect
#AIC and FPE suggests to go for a 14 lag.

#Identify number of cointegrating vectors
vecm <- ca.jo(v1, type="trace",K=14,ecdet="none", spec="longrun")
summary(vecm) #Trace test and how we specify the VEC is longrun
#Vaues of the trace test is most important. How many cointegrating vectors are available
#r <= 2 part
#r=0, test stat = 16.84 and critical values ar 10%, 5%... says that, we can't reject. 
#Thus, there is no cointegration.
#All cases it is the same. Can't reject null. There is no cointegrating vector

vecm1 <- ca.jo(v1, type="eigen",K=14,ecdet="none", spec="longrun")
summary(vecm1) #Lambda max test from eigen type
#Could conclude that there is no cointegrated
#Thus, VECM isn't the best but the VAR. 

