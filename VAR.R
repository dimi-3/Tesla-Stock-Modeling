#Load Packages
install.packages("bvartools")
install.packages("vars")
install.packages("mFilter")
install.packages("PerformanceAnalytics")
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
library(tidyverse)
library(mFilter)
library(PerformanceAnalytics)

#---------------------------------------------------------------------------------------------------------------------------
##DATA PREPROCESSING
#---------------------------------------------------------------------------------------------------------------------------
#Loading Data set 
Mtesla <- read.csv("C:\\Users\\USER\\Desktop\\Assignments Dimi\\7th Sem\\Financial Econometrics\\Take home assignment\\VARdata.csv")
head(Mtesla, 10)
Exrate <- read.csv("C:\\Users\\USER\\Desktop\\Assignments Dimi\\7th Sem\\Financial Econometrics\\Take home assignment\\Exchange Rates.csv")
head(Exrate, 10) 

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
VAR_tesla<- cbind(Mtesla, Exrate)
VAR_tesla
VAR_tesla1<- VAR_tesla[-c(9)]
VAR_tesla2<- VAR_tesla1[!(VAR_tesla1$Date=="2010-12-31"),]
head(VAR_tesla2, 10)

#Defining Variables
##Adjusted close Tesla
T_close=VAR_tesla2[,"Adj.Close.Tesla"]
T_close_log = log(T_close)#log transformation
Adj_close_tesla <- ts(data = T_close_log, frequency = 365, start = c(2011,1))

##Adjusted Close GM
GM_close=VAR_tesla2[,"Adj.Close.GM"]
GM_close_log = log(GM_close)#log transformation
Adj_close_GM <- ts(data = GM_close_log, frequency = 365, start = c(2011,1))

##CNY
Ex_rates=VAR_tesla2[,"CNY"]
Ex_rates_log = log(Ex_rates)#log transformation
USDCNY<- ts(data = Ex_rates_log, frequency = 365, start = c(2011,1))


#Plot data
par(mfrow = c(3, 1))
plot.ts(Adj_close_tesla, ylab = expression(paste("Tesla Adjusted Close Price")))
plot.ts(Adj_close_GM, ylab = expression(paste("General Motors Adjusted Close Price")))
plot.ts(USDCNY, ylab = expression(paste("USD to CNY Rate")))

#---------------------------------------------------------------------------------------------------------------------------
# STATIONARITY IN SERIES
#---------------------------------------------------------------------------------------------------------------------------
#Check stationarity in Data
PP.test(Adj_close_tesla) #Not Stationary
d.Adj_Close_tesla=diff(Adj_close_tesla,differences = 1)
PP.test(d.Adj_Close_tesla)

pp.test(Adj_close_GM) #Stationary at 0.05
d.Adj_close_GM=diff(Adj_close_GM,differences = 1)
PP.test(d.Adj_close_GM) #Stationary

pp.test(USDCNY)
d.USDCNY=diff(USDCNY,differences = 1)
PP.test(d.USDCNY)#Stationary

#---------------------------------------------------------------------------------------------------------------------------
## ACF/ PACF PLOT
#---------------------------------------------------------------------------------------------------------------------------
#Tesla
acf(d.Adj_Close_tesla, lag =30 )
pacf(d.Adj_Close_tesla, lag =30)
#GM
acf(d.Adj_close_GM, lag =30 )
pacf(d.Adj_close_GM, lag =30)
#Exrate
acf(d.USDCNY, lag =30 )
pacf(d.USDCNY, lag =30)

#---------------------------------------------------------------------------------------------------------------------------
## BUILDING THE VAR MODEL
#---------------------------------------------------------------------------------------------------------------------------
#Create the VAR dataset
v1 <- cbind(d.Adj_Close_tesla,d.Adj_close_GM,d.USDCNY)
v1 <- v1[-1,]
colnames(v1) <- cbind("d.Adj_Close_tesla","d.Adj_close_GM", "d.USDCNY")
head(v1, 10)

#Lag selection
lagselect <- VARselect(v1, lag.max = 15, type = "const") #lag.max means how many lags to go maximum
                                                        #Have a constant not a trend is meant by type= constant
lagselect$selection #let's go for 1 lags

#Outputs given are based on 4 criteria: Akaike, Hannan-Quinn, Schwarz, and Final Prediction Error

#Model estimation
ModelA <- VAR(v1, p = 1, type = "const", season = NULL, exog = NULL) 
summary(ModelA) 

#---------------------------------------------------------------------------------------------------------------------------
## GRANGER CAUSALITY
#---------------------------------------------------------------------------------------------------------------------------
#Which variables are causing which variables
cause.ex <- causality(ModelA, cause = "d.Adj_Close_tesla") 
cause.ex #There is no impact of Tesla Adjusted Close on other variables

cause.v <- causality(ModelA, cause = "d.Adj_close_GM") #Have an impact
cause.v

cause.y <- causality(ModelA, cause = "d.USDCNY") #Have an impact
cause.y

#---------------------------------------------------------------------------------------------------------------------------
## IMPULSE RESPONSE FUNCTION
#---------------------------------------------------------------------------------------------------------------------------
# Impulse variable = GM
# Check how a shock in GM will affect GM
Mirf <- irf(ModelA, impulse = "d.Adj_close_GM", response = "d.Adj_close_GM", n.ahead = 20, boot = TRUE)
plot(Mirf, ylab = "d.Adj_close_GM", main = "d.Adj_close_GM shock to d.Adj_close_GM")

# Check how a shock in GM will affect ExRate
Forexirf <- irf (ModelA, impulse = "d.Adj_close_GM", response = "d.USDCNY", n.ahead = 20, boot = TRUE)
plot(Forexirf, ylab = "d.USDCNY", main = "d.Adj_close_GM shock to d.USDCNY")

# Check how a shock in GM will affect Tesla
Xirf <- irf(ModelA, impulse = "d.Adj_close_GM", response = "d.Adj_Close_tesla", n.ahead = 20, boot = TRUE)
plot(Xirf, ylab = "d.Adj_Close_tesla", main = "d.Adj_close_GM shock to d.Adj_Close_tesla")

                                                                          #impulse= give the shock to, response= no such shock
# Impulse variable = Tesla
# Check how a shock in Tesla will affect GM
Tirf <- irf(ModelA, impulse = "d.Adj_Close_tesla", response = "d.Adj_close_GM", n.ahead = 20, boot = TRUE)
plot(Mirf, ylab = "d.Adj_close_GM", main = "d.Adj_Close_tesla shock to d.Adj_close_GM")

# Check how a shock in Tesla will affect Tesla
Birf <- irf(ModelA, impulse = "d.Adj_Close_tesla", response = "d.Adj_Close_tesla", n.ahead = 20, boot = TRUE)
plot(Mirf, ylab = "d.Adj_Close_tesla", main = "d.Adj_Close_tesla shock to d.Adj_Close_tesla")

# Check how a shock in Tesla will affect ExRate
Airf <- irf(ModelA, impulse = "d.Adj_Close_tesla", response = "d.USDCNY", n.ahead = 20, boot = TRUE)
plot(Mirf, ylab = "d.USDCNY", main = "d.Adj_Close_tesla shock to d.USDCNY")

# Impulse variable = ExRate
# Check how a shock in Exrate will affect GM
Girf <- irf(ModelA, impulse = "d.USDCNY", response = "d.Adj_close_GM", n.ahead = 20, boot = TRUE)
plot(Mirf, ylab = "d.Adj_close_GM", main = "d.USDCNY shock to d.Adj_close_GM")

# Check how a shock in Exrate will affect Tesla
Hirf <- irf(ModelA, impulse = "d.USDCNY", response = "d.Adj_Close_tesla", n.ahead = 20, boot = TRUE)
plot(Mirf, ylab = "d.Adj_Close_tesla", main = "d.USDCNY to d.Adj_Close_tesla")

# Check how a shock in Exrate will affect Exrate
Qirf <- irf(ModelA, impulse = "d.USDCNY", response = "d.USDCNY", n.ahead = 20, boot = TRUE)
plot(Mirf, ylab = "d.USDCNY", main = "d.USDCNY to d.USDCNY")
#---------------------------------------------------------------------------------------------------------------------------
## VARIABLE DECOMPOSITION
#---------------------------------------------------------------------------------------------------------------------------                                                                          #If there is a shock in one variable how it affects the same variable or another variable
bv.vardec <- fevd(ModelA, n.ahead = 5)
plot(bv.vardec)

#---------------------------------------------------------------------------------------------------------------------------
## FORECASTING
#---------------------------------------------------------------------------------------------------------------------------
forecastresults= predict(ModelA, n.ahead=100,ci=0.95)
forecastresults
plot(forecastresults, "single")


#---------------------------------------------------------------------------------------------------------------------------
## CONTEMPORANEOUS EFFECTS ON STRUCTURAL VAR
#---------------------------------------------------------------------------------------------------------------------------
##This period impacts. 
# Estimate structural coefficients
a <- diag(1, 3)
a[lower.tri(a)] <- NA
a
#We gonna estimate the NA values
svar_est <- SVAR(ModelA, Amat = a, max.iter = 15000)
#---------------------------------------------------------------------------------------------------------------------------
## CONTEMPORANEOUS EFFECTS
#---------------------------------------------------------------------------------------------------------------------------
svar_est
svar_est$Ase #check whether the variables are statistically significant

#---------------------------------------------------------------------------------------------------------------------------
## IMPULSE RESPONSE FUNCTION FOR SVAR
#---------------------------------------------------------------------------------------------------------------------------
#How a shock in GM impacts itself and other variables
Pirf <- irf(svar_est, impulse = "d.Adj_close_GM", response = "d.Adj_close_GM", n.ahead = 20, boot = TRUE)
plot(Pirf, ylab = "d.Adj_close_GM", main = "d.Adj_close_GM shock to d.Adj_close_GM")

Intirf <- irf(svar_est, impulse = "d.Adj_close_GM", response = "d.Adj_Close_tesla shock", n.ahead = 40, boot = TRUE)
plot(Intirf, ylab = "d.Adj_Close_tesla shock", main = "d.Adj_close_GM shock to d.Adj_Close_tesla shock")

Cirf <- irf(svar_est, impulse = "d.Adj_close_GM", response = "d.USDCNY", n.ahead = 40, boot = TRUE)
plot(Cirf, ylab = "d.USDCNY", main = "d.USDCNY")

#---------------------------------------------------------------------------------------------------------------------------
## VARIANCE DECOMPOSITION
#---------------------------------------------------------------------------------------------------------------------------
bvs.vardec <- fevd(svar_est, n.ahead = 10)
plot(bvs.vardec)

#---------------------------------------------------------------------------------------------------------------------------
## MODELING ARCH/GARCH
#---------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
## ARCH MODEL
#----------------------------------------------------------------------------------------------------------------
#Fit a ARCH model
library(fGarch)
library(rugarch)
library(rmgarch)
install.packages("MTS")
library(MTS)

#Heteroskedasticity ARCH-LM test
ARCHtst1 = arch.test(ModelA, multivariate.only=TRUE)
ARCHtst1 #heteroscedasticity in residuals

#Specs for the model
ugarch_t <- ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean= FALSE), variance.model = list(model= "sGARCH", garchOrder= c(1,1)))

#DCC Model
dcc_t<-  dccspec(uspec = multispec(replicate(ugarch_t, n=3)),VAR=TRUE, lag=0, model= "DCC", dccOrder= c(1,1))

##Model Estimation
fit1 = dccfit(dcc_t, data= ModelA, solver= "nlminb")
fit1

#---------------------------------------------------------------------------------------------------------------------------
## PERFORMANCE EVALUATION OF THE MODEL
#---------------------------------------------------------------------------------------------------------------------------
#serial correlation
serialresid= serial.test(ModelA, lags.pt=10, type="PT.asymptotic")
serialresid #No serial correlation

#Heteroskedasticity ARCH-LM test
ARCHtst = arch.test(ModelA, lags.multi=10, multivariate.only=TRUE)
ARCHtst #heteroscedasticity in residuals

#Normal distribution of the residuals
normresid = normality.test(ModelA, multivariate.only = TRUE)
normresid #Residuals don't fit normal distribution

#Structural Breaks in the residuals
stabilityy= stability(ModelA, type= "OLS-CUSUM")
plot(stabilityy)#There are structural breaks
