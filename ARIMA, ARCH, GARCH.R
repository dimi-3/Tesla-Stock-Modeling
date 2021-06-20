#Load Packages
install.packages("strucchange")
install.packages("dplyr")
install.packages("dynlm")
install.packages("rlang")
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
#Loading the dataset
tesla <- read.csv("C:\\Users\\USER\\Desktop\\Assignments Dimi\\7th Sem\\Financial Econometrics\\Lab exercises\\TSLA.csv")
head(tesla, 10)

#Missing Values
tesla$Date = as.Date(tesla$Date, format = "%m/%d/%Y")
dates = data.frame(Date = seq(as.Date('2010-06-29'), as.Date('2021-01-25'), by = 'days'))
tesla_merge <- merge(tesla,dates,by="Date", all = T)
tesla <-na.locf(tesla_merge, fromLast = TRUE)
head(tesla,10)

#Defining the time-series
tesla_stock = tesla[,'Adj.Close']
log_tesla_stocks= log(tesla_stock) #Stationary Variance
tesla_stocks <- ts(data =log_tesla_stocks, frequency = 365, start = c(2010,179)) #Time series object
plot.ts(tesla_stocks)
#------------------------------------------------------------------------------------------------------------
## STATIONARITY IN SERIES
#------------------------------------------------------------------------------------------------------------

#Check Stationary
adf.test(tesla_stocks) #Not Stationary, P-Val greater than 0.05.

#Difference and check for stationary, stationary mean
d.tesla<-diff(tesla_stocks, differences=1)

adf.test(d.tesla) #P Val is small, Thus stationary
plot.ts(d.tesla)

#-------------------------------------------------------------------------------------------------------------
## ACF/PACF PLOT AND OPTIMAL PARAMETER IDENTIFICATION
#-------------------------------------------------------------------------------------------------------------
#Identifying P,D,Q
d.tesla %>% ggtsdisplay()
acf(d.tesla, lag=30)
pacf(d.tesla, lag=30)

#Auto ARIMA
fit <- auto.arima(tesla_stocks, seasonal=FALSE)
fit #(4,1,0)


#Estimation of selected models
arima_1 <- Arima(tesla_stocks, order=c(0,0,0)) 

#Model Summary
arima_1 

#Selection of the Best Model
AIC(arima_1, fit) 
BIC(arima_1,fit) 

#-----------------------------------------------------------------------------------------------------------
## BUILD ARIMA MODEL (WITHOUT STRUCTURAL BREAKS)
#-----------------------------------------------------------------------------------------------------------
#We make the fitted model
arima_c = Arima(tesla_stocks, order=c(4,1,0), include.drift = TRUE)
arima_c

#Diagnostic checking
#Check whether estimated residuals are white noise 
checkresiduals(arima_c) #WN
#Forecasting
autoplot(forecast(arima_c))

#------------------------------------------------------------------------------------------------------
## FORECAST USING ARIMA
#------------------------------------------------------------------------------------------------------
#Train test Split
tesla_stocks.train <- window(tesla_stocks, end=c(2018,365))
tesla_stocks.test <- window(tesla_stocks, start=c(2019,1))
#Training the data
arima.train <- Arima(tesla_stocks.train, order=c(4,1,0), include.drift = TRUE)

#Testing accurancy
accuracy(forecast(arima.train, h=771), tesla_stocks.test)

#Forecast and Plot
forecast <-forecast(arima.train, h=771)
autoplot(tesla_stocks) + autolayer(forecast, series = "ARIMA(4,1,0)\nforecasted \nwith drift", alpha = 0.5)

#-------------------------------------------------------------------------------------------------------------
## ARIMA MODEL (WITH STRUCTURAL BREAKS)
#-------------------------------------------------------------------------------------------------------------
## DATA PREPROCESSING
#-------------------------------------------------------------------------------------------------------------
#Defining the Timeseries
tesla_stock = tesla[,'Adj.Close']
tesla_stock1 <- ts(data =tesla_stock, frequency = 365, start = c(2010,179)) #Time series object
plot.ts(tesla_stock1)
#decompose into time series components
timeseriescomponents <- decompose(tesla_stock1)
plot(timeseriescomponents)

#Check for structural breaks
# store the breakpoints
bp.tesla <- breakpoints(tesla_stock1 ~ 1, h=0.1) #breaks in terms of intercept only
summary(bp.tesla)

## the BIC chooses 3 breakpoints; plot the graph with break dates and their confidence intervals
breakdates(bp.tesla, breaks= 3)
plot(bp.tesla)
lines(fitted(bp.tesla, breaks = 3), col = 2)
lines(confint(bp.tesla, breaks =3))

#Let's create dummies for the 3 episodes
tesla$break1 <- 1
tesla$break1[1:1122] <- 0
tesla$break2 <- 1
tesla$break2[1:2416] <- 0
tesla$break3 <- 1
tesla$break3[1:3478] <- 0

#Check whether removing the effect of structural breaks makes the series stationary
lm1 <- lm(tesla_stocks ~ break1 + break2 + break3, data = tesla)
summary(lm1) # All structural breaks are significant

#Check for stationary in residuals
resid1 <- lm1$residuals
adf.test(resid1) #The series is stationary
plot(resid1)

#Split of data
tesla_new <- tesla_stock1[1121:3864]
log_tesla_new <- log(tesla_new)
tesla_stock2 <- ts(data =log_tesla_new, frequency = 365, start = c(2013,205)) 
plot(tesla_stock2,col="black",ylab="Adjusted Close Price")

#---------------------------------------------------------------------------------------------------
## STATIONARITY IN SERIES
#---------------------------------------------------------------------------------------------------
#Check Stationary
adf.test(tesla_stock2) #Not Stationary, Pval greater so can't reject Null

#Create log-returns series
returns <- diff(tesla_stock2, differences = 1)
plot.ts(returns, col = "Black")

#Check for stationary
adf.test(returns) #It is Stationary

#-------------------------------------------------------------------------------------------------------------
## ACF/PACF PLOT AND OPTIMAL PARAMETER IDENTIFICATION
#-------------------------------------------------------------------------------------------------------------
#Initial guess for p,d and q
returns %>% ggtsdisplay()
acf(returns, lag =30 )
pacf(returns, lag =30)

#Auto ARIMA
fit <- auto.arima(tesla_stock2, seasonal=FALSE)
fit #(1,2,0)
fit1<- auto.arima(tesla_stock2, seasonal=TRUE)
fit1#(5,2,0)

#Estimation of selected models
arima_1 <- Arima(tesla_stock2, order=c(0,0,0)) 
arima_2 <- Arima(tesla_stock2, order=c(0,1,0))
arima_3 <- Arima(tesla_stock2, order=c(1,1,1))

#Model Summary
arima_1 
arima_2
arima_3

#Selection of the Best Model
AIC(arima_1, arima_2,arima_3, fit,fit1) 
BIC(arima_1,arima_2, arima_3, fit,fit1) 

#-------------------------------------------------------------------------------------------------------------
## ARIMA MODEL
#-------------------------------------------------------------------------------------------------------------
#We make the fitted model
arima_c = Arima(tesla_stock2, order=c(0,1,0), include.drift = TRUE)
arima_c

#Diagnostic checking
#Check whether estimated residuals are white noise 
checkresiduals(arima_c) #WN
#Forecasting
autoplot(forecast(arima_c))


#-------------------------------------------------------------------------------------------------------------
## FORECAST USING ARIMA
#-------------------------------------------------------------------------------------------------------------
#Train test Split
tesla_stocks.train <- window(tesla_stock2, end=c(2018,365))
tesla_stocks.test <- window(tesla_stock2, start=c(2019,1))
#Training the data
arima.train <- Arima(tesla_stocks.train, order=c(0,1,0), include.drift = TRUE)

#Testing accurancy
accuracy(forecast(arima.train, h=771), tesla_stocks.test) 

#Forecast and Plot
forecast <-forecast(arima.train, h=771)
autoplot(tesla_stocks) + autolayer(forecast, series = "ARIMA(0,1,0)\nforecasted \nwith drift", alpha = 0.5)

#-----------------------------------------------------------------------------------------------------------------
## ARCH AND GARCH
#-----------------------------------------------------------------------------------------------------------------
#Install Packages
library(zoo)
library(aTSA)
library(fGarch)
library(rugarch)

#----------------------------------------------------------------------------------------------------------------
## TESTING FOR ARCH EFFECTS FORMALLY
#----------------------------------------------------------------------------------------------------------------
#Fitting ARIMA(0,1,0) model
m1 <- arima(tesla_stock2, order=c(0,1,0)) 
m1

#The ARCH Engle's test
arch.test(m1, output=TRUE) #ARCH test, p-val = 0. Therefore, have ARCH effects.

#----------------------------------------------------------------------------------------------------------------
## ARCH MODEL
#----------------------------------------------------------------------------------------------------------------
#Fit an ARCH model
model1 <- garchFit( ~arma(0,0)+garch(1,0), returns, include.mean=FALSE, trace=FALSE)
model1 
summary(model1)

model11 <- garchFit( ~arma(0,0)+garch(2,0), returns, include.mean=FALSE, trace=FALSE)
model11 
summary(model11)

model12 <- garchFit( ~arma(0,0)+garch(3,0), returns, include.mean=FALSE, trace=FALSE)
model12 
summary(model12)

model13 <- garchFit( ~arma(0,0)+garch(6,0), returns, include.mean=FALSE, trace=FALSE)
model13 
summary(model13)

model14 <- garchFit( ~arma(0,0)+garch(7,0), returns, include.mean=FALSE, trace=FALSE)
model14 
summary(model14)

#----------------------------------------------------------------------------------------------------------------
## GARCH MODEL
#----------------------------------------------------------------------------------------------------------------
#Fit a GARCH model
model2 <- garchFit( ~arma(0,0)+garch(1,1), returns, include.mean=FALSE, trace=FALSE)
model2 
summary(model2) 

model22 <- garchFit( ~arma(0,0)+garch(1,2), returns, include.mean=FALSE, trace=FALSE)
model22 
summary(model22)

model23 <- garchFit( ~arma(0,0)+garch(1,6), returns, include.mean=FALSE, trace=FALSE)
model23 
summary(model23)


#predictions for GARCH model
prediction <- predict(model23, n.ahead=50, plot=TRUE)

#----------------------------------------------------------------------------------------------------------------
## Extension for the GARCH model
#----------------------------------------------------------------------------------------------------------------

#GJR GARCH model
gjrgarch1 <- garchFit(~arma(0,0)+garch(1,6), leverage=T,returns,trace=F,include.mean=F)
summary(gjrgarch1)

#predictions for GJR GARCH model
prediction2 <- predict(gjrgarch1, n.ahead=50, plot=TRUE)

#Exponential GARCH model
egarch1 <- ugarchfit(ugarchspec(mean.model=list(armaOrder=c(0,0),include.mean=F),variance.model=list(model="eGARCH",garchOrder=c(1,6))),returns)
egarch1 

