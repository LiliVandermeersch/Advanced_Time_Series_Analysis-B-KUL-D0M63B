# Advanced Time Series Analysis  -------------------------------------------------------------------------------------------------
# Description: Assignment
# Author: Lili Vandermeersch r0691855
# input: 
# output: ATA_Assignment_Vandermeersch_r0691855_2022
# File history:
#   121022: creation
#   .
#   111222: completion
# Paths ----------------------------------------------------------------------------------------------------
rm(list =ls()) # clears R environment
options(scipen=999) # disables scientific notation

setwd("")
input <- file.path()
output <- file.path()

# Packages -------------------------------------------------------------------------------------------------
library(readxl)
library(tseries)
library(urca)
library(CADFtest)
library(forecast)
library(reshape2) 
library(ggplot2)
library(tidyr)
library(fpp)
library(fGarch)
library(tidyverse)
library(vars)

# Importing data ------------------------------------------------------------------------------------------- 
cpi<- read_excel(file.choose())
View(cpi)
rgdp <- read_excel(file.choose())
View(rgdp)

# Data inspection, data cleansing
nrow(cpi)
nrow(rgdp)

names(cpi)
names(rgdp)

sum(is.na(cpi$CPI))
sum(is.na(rgdp$rgdp))

# Univariate Analysis -------------------------------------------------------------------------------------
cpi_ts <- ts(cpi$CPI,frequency = 4,start = c(1960,1))
rgdp_ts <- ts(rgdp$rgdp,frequency = 4,start = c(1947,1))

# Plot
# ts.plot(cpi_ts)
# ts.plot(rgdp_ts)

# GGplot
autoplot(cpi_ts)
autoplot(rgdp_ts)

# Correlogram
acf(cpi_ts) # => non-stationary
acf(rgdp_ts) # => non-stationary

# Log-transformation
lmwrgdp_ts <- window(rgdp_ts, c(1960,1), c(2022,3))
lmwcpi_ts <- window(cpi_ts, c(1960,1), c(2022,3))
logcpi_ts <- log(lmwrgdp_ts)
logrgdp_ts<- log(lmwcpi_ts)

autoplot(logcpi_ts)
autoplot(logrgdp_ts)

# Linear Regression log(RGDP)-------------------------------------------------------------------------------
logrgdp_cut_ts <- window(logrgdp_ts, start=c(1960,4), c(2022,3)) # length of regressors need to be equal (i.e., length of ts must be divisible by 4).
length(logrgdp_cut_ts)
TREND<-1:248
Q1<-c(rep(c(1,0,0,0),62))
Q2<-c(rep(c(0,1,0,0),62))
Q3<-c(rep(c(0,0,1,0),62))
Q4<-c(rep(c(0,0,0,1),62))
fit<-tslm(logrgdp_cut_ts~TREND+Q1+Q2+Q3+Q4)
summary(fit) 
  # Assuming the model valid, we can interpret the output as follows:
# R^2: 89.19% of all variance in the logarithmic transformation of GDP is explained by my model (by the regressors).
# p-value of F-stat: all predictor variables/regressors jointly are highly significant.
# Info from parameters: 
# Trend is significant. Ceteris paribus (= other things equal), Log(CPI) increases by 0.0072053 = 1.3% quarterly, on average. 
# Intercept significant but we would leave it in, even if it wasn't.
# No seasonal effect. Seasonal dummies not significant.
# Q4 is omitted to avoid perfect multi-collinearity.

ts.plot(fit$residuals,main = "Residual plot after linear regression") 
# High R^2 and high residual autocorrelation can be signs of spurious regression.
checkresiduals(fit) # Residuals clearly not white noise. 
# They violate homoscedasticity assumptions. => model performs well but is invalid!
# The mean of the residuals is close to zero but there is significant correlation in the residuals series. 
# The time plot of the residuals shows that the variation of the residuals differ quite a bit across the historical data.
# There also seems to be an extreme value in Q2 2020 most probably due to Covid. The residual variance cannot be treated as a constant. 
# This can also be seen in the histogram of the residuals. The histogram suggests that the residuals are not normal.
# Consequently, forecasts based on this model should not be made.


# Linear regression removing the outlier of 2020 (Covid)
logrgdp_cut2_ts <- window(logrgdp_ts, start=c(1995,1), c(2019,4)) # length of regressors need to be equal (i.e., length of ts must be divisible by 4).
length(logrgdp_cut2_ts)
TREND2<-1:100
Q1<-c(rep(c(1,0,0,0),25))
Q2<-c(rep(c(0,1,0,0),25))
Q3<-c(rep(c(0,0,1,0),25))
Q4<-c(rep(c(0,0,0,1),25))
fit<-lm(logrgdp_cut2_ts~TREND2+Q1+Q2+Q3+Q4)
summary(fit) 

ts.plot(fit$residuals,main = "Residual plot after linear regression") 
# High R^2 and high residual autocorrelation can be signs of spurious regression.
checkresiduals(fit) 

# Linear regression from 2008 financial crisis till Covid 2020
logrgdp_cut3_ts <- window(logrgdp_ts, start=c(1960,1), c(2019,4)) # length of regressors need to be equal (i.e., length of ts must be divisible by 4).
length(logrgdp_cut3_ts)
TREND3<-1:240
Q1<-c(rep(c(1,0,0,0),60))
Q2<-c(rep(c(0,1,0,0),60))
Q3<-c(rep(c(0,0,1,0),60))
Q4<-c(rep(c(0,0,0,1),60))
fit<-lm(logrgdp_cut3_ts~TREND3+Q1+Q2+Q3+Q4)
summary(fit) 

checkresiduals(fit)
Box.test(fit$residuals, lag = round(sqrt(length(fit$residuals))), type = "Ljung-Box") # <5% => We reject H0 and conclude
# that the time series of residuals is not white noise => model invalid

# qqnorm(fit$residuals, main='Q-Q Plot Residuals')
# qqline(fit$residuals) # => residuals not normally distributed => cannot be used for forecasting 

# Linear Regression CPI-----------------------------------------------------------------------------------------
logcpi_cut_ts <- window(logcpi_ts, start=c(1960,4), c(2022,3)) # length of regressors need to be equal (i.e., length of ts must be divisible by 4).
length(logcpi_cut_ts)
TREND<-1:248
Q1<-c(rep(c(1,0,0,0),62))
Q2<-c(rep(c(0,1,0,0),62))
Q3<-c(rep(c(0,0,1,0),62))
Q4<-c(rep(c(0,0,0,1),62))
fit<-lm(logcpi_cut_ts~TREND+Q1+Q2+Q3+Q4)
summary(fit) 
# Assuming the model valid, we can interpret the output as follows:
# R^2: 98.66% of all variance in the logarithmic transformation of CPI is explained by my model (by the regressors).
# p-value of F-stat: all predictor variables/regressors jointly are highly significant.
# Info from parameters: 
# Trend is significant. Ceteris paribus (= other things equal), Log(CPI) increases by 0.0072053 = 0.72% quarterly, on average. 
# Intercept significant but we would leave it in, even if it wasn't.
# No seasonal effect -> seasonal dummies are not significant. % change in CPI compared to Q4.
# Q4 is omitted to avoid perfect multi-collinearity.

ts.plot(fit$residuals,main = "Residual plot after linear regression") 
# High R^2 and high residual autocorrelation can be signs of spurious regression.
checkresiduals(fit) # Residuals clearly not white noise. 
# They violate homoscedasticity assumptions. => model performs well but is invalid!
# The mean of the residuals is close to zero but there is significant correlation in the residuals series. 
# The time plot of the residuals shows that the variation of the residuals differ quite a bit across the historical data.
# There also seems to be an extremr value in Q2 2020 most probably due to Covid. The residual variance cannot be treated as a constant. 
# This can also be seen in the histogram of the residuals. The histogram suggests that the residuals are not normal. 
# Consequently, forecasts based on this model should not be made.

# Seasonal differences
scpi_ts <- diff(cpi_ts, lag = 4)
drgdp_ts <- diff(rgdp_ts, lag = 4)
autoplot(scpi_ts)
autoplot(drgdp_ts)
acf(scpi_ts) # The slow decrease in the ACF as the lags increase is due to the trend, while the “scalloped” shape is due to the seasonality.
acf(drgdp_ts) # => MA(3)

# Log - seasonal differences
slogcpi_ts <- diff(log(cpi_ts), lag = 4)
slogrgdp_ts <- diff(log(rgdp_ts), lag = 4)
autoplot(slogcpi_ts)
autoplot(slogrgdp_ts)
acf(slogcpi_ts) # The slow decrease in the ACF as the lags increase is due to the trend, while the “scalloped” shape is due to the seasonality.
acf(slogrgdp_ts) # => MA(3)

# Log-differences 
infl_ts <- diff(log(cpi_ts)) # The slow decrease in the ACF as the lags increase is due to the trend, while the “scalloped” shape is due to the seasonality.
growth_ts<- diff(log(rgdp_ts))
autoplot(infl_ts)
autoplot(growth_ts)

ggAcf(infl_ts)
ggAcf(growth_ts) # looks stationary
monthplot(growth_ts) # mean effects differ slightly
monthplot(infl_ts) # mean effects differ => seasonal effect => i.e, not stationary

ggseasonplot(growth_ts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Growth") +
  ggtitle("Seasonal plot: Growth in the UK")
ggseasonplot(infl_ts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Inflation") +
  ggtitle("Seasonal plot: Inflation in the UK")

ggAcf(growth_ts) # => looks stationary (possibly even white noise), max MA(2)
ggPacf(growth_ts) # => AR(1) or AR(2)
acf(infl_ts) # => doesn't look stationary 

# Augmented Dickey-Fuller (ADF) test - Testing non-stationarity - diff(log(RGDP))=GROWTH
maxlag <- round(sqrt(length(growth_ts)))
CADFtest(growth_ts, type = "drift", criterion = "BIC", max.lag.y = maxlag) # => <5% We reject H0 and conclude
# that the time series is stationary. => i.e., integrated of order one (not stationary in levels, but stationary in differences).

# Augmented Dickey-Fuller (ADF) test - Testing non-stationarity - diff(log(CPI))=INFL
maxlag <- round(sqrt(length(infl_ts)))
CADFtest(infl_ts, type = "drift", criterion = "BIC", max.lag.y = maxlag) # => >5% We cannot reject H0 and conclude
# that the time series is probably not stationary (borderline value)

# Seasonal diff Infl.
sinfl_ts <- diff(infl_ts, lag = 4)
autoplot(sinfl_ts)
ggAcf(sinfl_ts)
ggPacf(sinfl_ts)
  
# Augmented Dickey-Fuller (ADF) test - Testing non-stationarity - diff(log(CPI))=INFL
maxlag <- round(sqrt(length(sinfl_ts)))
CADFtest(sinfl_ts, type = "drift", criterion = "BIC", max.lag.y = maxlag) # => <5% We reject H0 and conclude
# that the time series is  stationary.

# Model Specification, key correlograms-------------------------------------------------------------
ggAcf(growth_ts) # => ARIMA(0,1,2)(0,0,0)[4], ARIMA(0,1,1)(0,0,0)[4]
ggPacf(growth_ts) # => ARIMA(1,1,0)(0,0,0)[4], ARIMA(2,1,0)(0,0,0)[4], ARIMA(1,1,1)

ggAcf(sinfl_ts) # => Still some seasonality in the data => MA(2) repeats seasonally
ggPacf(sinfl_ts) # Significant autocorr. at lag 1, 3, 4, 5, 8, 12 => SARIMA => AR(2) or AR(3) repeats seasonally 
# => ARIMA(2,1,0)(2,0,0)[4], ARIMA(1,1,0)(1,0,0)[4], ARIMA(1,1,0)(2,0,0)[4], etc.

# R Auto-specification
auto.arima(log(rgdp_ts)) # ARIMA(0,2,3)(1,0,1)[4] 
auto.arima(log(cpi_ts)) # ARIMA(3,2,1)(0,0,1)[4] 

# **RGDP** ARIMA models (We cannot compare AIC values between models with different orders of differencing)------------------------
#            Real GDP ***** Differencing - Level 1 ************
 
# ARIMA(1,1,0)(0,0,0)[4]
### Estimation
model110 <-arima(log(rgdp_ts),order=c(1,1,0),seasonal=c(0,0,0))
model110 # => coefficient significant
### Validation
autoplot(model110$residuals) 
acf(model110$res, plot=T, main="residual correlogram") # => first res. has high autocorr.
Box.test(model110$res, lag=round(sqrt(length(model110$residuals))), type="Ljung-Box") # => < 5% Model not valid
### Evaluation
AIC(model110) 
AIC(model110,k = log(length(log(rgdp_ts))))

# ARIMA(0,1,1)(0,0,0)[4]
### Estimation
model011 <-arima(log(rgdp_ts),order=c(0,1,1))
model011 # => coefficient significant
### Validation
autoplot(model011$residuals) 
# acf(model011$res, plot=T, main="residual correlogram") # => first two res.corr. significant.
ggAcf(model011$res, plot=T, main="residual correlogram")
Box.test(model011$res, lag=round(sqrt(length(model011$residuals))), type="Ljung-Box") # > 5% => Model valid
### Evaluation
AIC(model011) 
AIC(model011,k = log(length(log(rgdp_ts))))

# ARIMA(1,1,1)(0,0,0)[4]
### Estimation
model111 <-arima(log(rgdp_ts),order=c(1,1,1))
model111 # => coefficient significant
### Validation
autoplot(model111$residuals) 
# acf(model111$res, plot=T, main="residual correlogram") 
ggAcf(model111$res, plot=T, main="residual correlogram")
Box.test(model111$res, lag=round(sqrt(length(model111$residuals))), type="Ljung-Box") # > 5% => Model valid
### Evaluation
AIC(model111) 
AIC(model111,k = log(length(log(rgdp_ts))))

# ARIMA(0,1,2)(0,0,0)[4]
### Estimation
model012 <-arima(log(rgdp_ts),order=c(0,1,2))
model012 # => coefficient significant
### Validation
autoplot(model012$residuals) 
# acf(model111$res, plot=T, main="residual correlogram") 
ggAcf(model012$res, plot=T, main="residual correlogram")
Box.test(model012$res, lag=round(sqrt(length(model012$residuals))), type="Ljung-Box") # > 5% => Model valid
### Evaluation
AIC(model012) 
AIC(model012,k = log(length(log(rgdp_ts))))

# ARIMA(2,1,0)(0,0,0)[4]
### Estimation
model210 <-arima(log(rgdp_ts),order=c(2,1,0))
model210 # => coefficient significant
### Validation
autoplot(model210$residuals) 
# acf(model210$res, plot=T, main="residual correlogram") 
ggAcf(model210$res, plot=T, main="residual correlogram")
Box.test(model210$res, lag=round(sqrt(length(model210$residuals))), type="Ljung-Box") # > 5% => Model valid
### Evaluation
AIC(model210) 
AIC(model210,k = log(length(log(rgdp_ts))))

# Forecasting-----------------------------------------------------------------------------------------------------
# Forecasting ARIMA(0,1,1)(0,0,0)[4]
forecast011<-predict(model011,n.ahead=8) 
expected=forecast011$pred 
lower=forecast011$pred-qnorm(0.975)*forecast011$se
upper=forecast011$pred+qnorm(0.975)*forecast011$se; 
exp(cbind(lower,expected,upper))

plot.ts(rgdp_ts, xlim=c(2018, 2025), ylim=c(5000,30000))
lines(exp(expected), col="red")
lines(exp(upper), col="blue")
lines(exp(lower), col="blue")

  # Alternative plot (log(rgdp))
forecast011_plot <- forecast(model011, level=c(95), h=8)
autoplot(forecast011_plot)

# Forecasting ARIMA(1,1,1)(0,0,0)[4]
forecast111<-predict(model111,n.ahead=8) 
expected=forecast111$pred 
lower=forecast111$pred-qnorm(0.975)*forecast111$se
upper=forecast111$pred+qnorm(0.975)*forecast111$se; 
exp(cbind(lower,expected,upper))

plot.ts(rgdp_ts, xlim=c(2018, 2025), ylim=c(5000,30000))
lines(exp(expected), col="red")
lines(exp(upper), col="blue")
lines(exp(lower), col="blue")

# Forecasting ARIMA(0,1,2)(0,0,0)[4]
forecast012<-predict(model012,n.ahead=8) 
expected=forecast012$pred 
lower=forecast012$pred-qnorm(0.975)*forecast012$se
upper=forecast012$pred+qnorm(0.975)*forecast012$se; 
exp(cbind(lower,expected,upper))

plot.ts(rgdp_ts, xlim=c(2018, 2025), ylim=c(5000,30000))
lines(exp(expected), col="red")
lines(exp(upper), col="blue")
lines(exp(lower), col="blue")

# Forecasting ARIMA(2,1,0)(0,0,0)[4]
forecast210<-predict(model210,n.ahead=8) 
expected=forecast210$pred 
lower=forecast210$pred-qnorm(0.975)*forecast210$se
upper=forecast210$pred+qnorm(0.975)*forecast210$se; 
exp(cbind(lower,expected,upper))

plot.ts(rgdp_ts, xlim=c(2018, 2025), ylim=c(5000,30000))
lines(exp(expected), col="red")
lines(exp(upper), col="blue")
lines(exp(lower), col="blue")

# Comparing different models--------------------------------------------------------------------------------------
# In-sample Criteria 
AIC(model110) 
AIC(model110,k = log(length(log(rgdp_ts))))

AIC(model011) 
AIC(model011,k = log(length(log(rgdp_ts))))

AIC(model111) 
AIC(model111,k = log(length(log(rgdp_ts))))

AIC(model012) 
AIC(model012,k = log(length(log(rgdp_ts))))

AIC(model210) 
AIC(model210,k = log(length(log(rgdp_ts))))

# Out-of-sample Criteria----------------------------------------------------------------
# Comparing 2 models with highest BIC
# Expanding window for calculating performance errors
y<-log(cpi_ts) 
S=round(0.75*length(y)) 
h=1 
error1.h<-c() 
for (i in S:(length(y)-h)) 
{ 
  mymodel.sub<-arima(y[1:i], order = c(1,1,1),seasonal=c(0,0,0)) 
  predict.h<-predict(mymodel.sub,n.ahead=h)$pred[h] 
  error1.h<-c(error1.h,y[i+h]-predict.h) 
} 
error2.h<-c() 
for (i in S:(length(y)-h)) 
{ 
  mymodel.sub<-arima(y[1:i], order = c(2,1,0),seasonal=c(0,0,0)) 
  predict.h<-predict(mymodel.sub,n.ahead=h)$pred[h] 
  error2.h<-c(error2.h,y[i+h]-predict.h) 
}

mean(abs(error1.h)) 
mean(abs(error2.h))

# Diebold Mariano test (h=1) -> Is the forecast performance of the two models, using absolute value loss, significantly different?
dm.test(error1.h,error2.h,h=h,power=1) # No significant difference
# For absolute value loss, use power=1. (For squared value loss you would need to use power=2).
# We obtain a p-value = 0.00 > 5%, thus we cannot reject H0 and conclude that the forecast performance of the two models, 
# using absolute value loss, is not significantly different.

mean(error1.h^2)
mean(error2.h^2) 
dm.test(error1.h,error2.h,h=h,power=2) # Weobtain a p-value = 0.00 > 5%, thus we cannot reject H0 
# and conclude that the forecast performance of the two models, using the squared value loss, is not significantly different.

# Comparing valid models with highest and lowest BIC----------------------------
# Expanding window for calculating performance errors

y<-log(cpi_ts) 
S=round(0.75*length(y)) 
h=1 
error1.h<-c() 
for (i in S:(length(y)-h)) 
{ 
  mymodel.sub<-arima(y[1:i], order = c(1,1,1),seasonal=c(0,0,0)) 
  predict.h<-predict(mymodel.sub,n.ahead=h)$pred[h] 
  error1.h<-c(error1.h,y[i+h]-predict.h) 
} 
error2.h<-c() 
for (i in S:(length(y)-h)) 
{ 
  mymodel.sub<-arima(y[1:i], order = c(0,1,1),seasonal=c(0,0,0)) 
  predict.h<-predict(mymodel.sub,n.ahead=h)$pred[h] 
  error2.h<-c(error2.h,y[i+h]-predict.h) 
}

mean(abs(error1.h)) 
mean(abs(error2.h))

# Diebold Mariano test (h=1) -> Is the forecast performance of the two models, using absolute value loss, significantly different?
dm.test(error1.h,error2.h,h=h,power=1) # Significantly different.
# For absolute value loss, use power=1. (For squared value loss you would need to use power=2).
# We obtain a p-value = 0.00 < 5%, thus we reject H0 and conclude that the forecast performance of 
# the two models, using the absolute value loss, is significantly different.

mean(error1.h^2)
mean(error2.h^2) 
dm.test(error1.h,error2.h,h=h,power=2) # We obtain a p-value = 0.00 < 5%, thus wereject H0 and conclude 
# that the forecast performance of the two models, using the squared value loss, is significantly different.

# Multivariate time series analysis-------------------------------------------------------------------------------------------

rgdp_ts <- window(rgdp_ts, c(1960,1), c(2022,3))
cpi_ts <- window(cpi_ts, c(1960,1), c(2022,3))

growth_ts <-diff(log(rgdp_ts))
infl_ts <- diff(log(cpi_ts))

growth_ts <- window(growth_ts, c(1960,2), c(2022,3))
infl_ts <- window(infl_ts, c(1960,2), c(2022,3))

cpitail <- tail((cpi$CPI), 251)
rgdptail <- (tail(rgdp$rgdp, 251))
newdf <- data.frame(diff(log(cpitail)), diff(log(rgdptail)))
names(newdf) = c("Inflation", "Growth")
View(newdf)
Combi_mts <- ts(newdf, start=c(1960,1), frequency=4)
autoplot(Combi_mts) + guides(colour = guide_legend(title = "Inflation and real GDP Growth in the UK between 1960 and 2022")) 
autoplot(infl_ts)
autoplot(growth_ts)

# Univariate models for sintfl_ts ------------------------------------------------------------
ggAcf(sinfl_ts) # => MA(2) repeats seasonally
ggPacf(sinfl_ts) # => AR(2) repeats seasonally

# ARIMA(0,1,1)(0,1,1)[4]
### Estimation
modelcpi1 <-arima(log(cpi_ts),order=c(0,1,1), seasonal=c(0,1,0))
modelcpi1 # => coefficient significant
### Validation
autoplot(modelcpi1$residuals) 
ggAcf(modelcpi1$res, plot=T, main="residual correlogram")
Box.test(modelcpi1$res, lag=round(sqrt(length(modelcpi1$residuals))), type="Ljung-Box") # < 5% => Model not valid

# ARIMA(0,1,2)(0,1,2)[4]
### Estimation
modelcpi2 <-arima(log(cpi_ts),order=c(0,1,2), seasonal=c(0,1,2))
modelcpi2 # => highest order coefficients all significant
### Validation
autoplot(modelcpi2$residuals) 
ggAcf(modelcpi2$res, plot=T, main="residual correlogram")
Box.test(modelcpi2$res, lag=round(sqrt(length(modelcpi2$residuals))), type="Ljung-Box") # < 5% => Model not valid
### Evaluation
AIC(modelcpi2) 
AIC(modelcpi2,k = log(length(log(cpi_ts))))

# ARIMA(1,1,1)(2,1,1)[4]
### Estimation
modelcpi3 <-arima(log(cpi_ts),order=c(1,1,1), seasonal=c(2,1,1))
modelcpi3 # => highest order coefficients significant
### Validation
autoplot(modelcpi3$residuals) 
ggAcf(modelcpi3$res, plot=T, main="residual correlogram")
Box.test(modelcpi3$res, lag=round(sqrt(length(modelcpi3$residuals))), type="Ljung-Box") # > 5% => Model valid but not parsimonious
### Evaluation
AIC(modelcpi3) 
AIC(modelcpi3,k = log(length(log(cpi_ts))))

# ARIMA(1,1,1)(1,1,1)[4]
### Estimation
modelcpi4 <-arima(log(cpi_ts),order=c(1,1,1), seasonal=c(1,1,1))
modelcpi4 # => highest order coefficients significant
### Validation
autoplot(modelcpi4$residuals) 
ggAcf(modelcpi4$res, plot=T, main="residual correlogram")
Box.test(modelcpi4$res, lag=round(sqrt(length(modelcpi4$residuals))), type="Ljung-Box") # > 5% => Model valid 
### Evaluation
AIC(modelcpi4) 
AIC(modelcpi4,k = log(length(log(cpi_ts))))

# ARIMA(2,1,2)(1,1,1)[4]
### Estimation
modelcpi5 <-arima(log(cpi_ts),order=c(2,1,2), seasonal=c(1,1,1))
modelcpi5 # => highest order coefficients significant
### Validation
autoplot(modelcpi5$residuals) 
ggAcf(modelcpi5$res, plot=T, main="residual correlogram")
Box.test(modelcpi5$res, lag=round(sqrt(length(modelcpi5$residuals))), type="Ljung-Box") # > 5% => Model valid but not parsimonious 
### Evaluation
AIC(modelcpi5) 
AIC(modelcpi5,k = log(length(log(cpi_ts))))

# ARIMA(1,2,1)(1,0,1)[4]
### Estimation
modelcpi6 <-arima(log(cpi_ts),order=c(1,2,1), seasonal=c(1,0,1))
modelcpi6 # => highest order coefficients significant
### Validation
autoplot(modelcpi5$residuals) 
ggAcf(modelcpi6$res, plot=T, main="residual correlogram")
Box.test(modelcpi6$res, lag=round(sqrt(length(modelcpi6$residuals))), type="Ljung-Box") # > 5% => Model valid 
### Evaluation
AIC(modelcpi6) 
AIC(modelcpi6,k = log(length(log(cpi_ts))))

# ARMA-GARCH

# Linear regression -----------------------------------------------------------------------------------
fit <-tslm(rgdp_ts ~ cpi_ts) 
summary(fit)
# R^2: 96.96% of all variance in CPI is explained by my model.
# p-value of F-stat: all predictor variables/regressors are jointly highly significant
# This model seems to perform very well, nevertheless it is not a valid model. The figure below plots the residuals, which are clearly not white noise.
# Linear regression residual plot
ts.plot(fit$residuals) # not white noise
acf(fit$residuals) # significant aurocorrelations
Box.test(fit$residuals, lag = round(sqrt(length(fit$residuals))), type = "Ljung-Box") # => not white noise => Model cannot be validated.

# Before covid
fit2 <- tslm(growth_ts ~ infl_ts)
summary(fit2)
# => They don't seem to move together at all
ts.plot(fit2$residuals,main = "Residual plot after linear regression") 
# High R^2 and high residual autocorrelation can be signs of spurious regression.
checkresiduals(fit2) # Residuals clearly not white noise. 
# They violate homoscedasticity assumptions. => model performs well but is invalid!
# The mean of the residuals is close to zero but there is significant correlation in the residuals series. 
# The time plot of the residuals shows that the variation of the residuals differ quite a bit across the historical data.
# There also seems to be an extreme value in Q2 2020 most probably due to Covid. The residual variance cannot be treated as a constant. 
# This can also be seen in the histogram of the residuals. The histogram suggests that the residuals are not normal.
# Consequently, forecasts based on this model should not be made.
Box.test(fit2$residuals, lag = round(sqrt(length(fit2$residuals))), type = "Ljung-Box") # => white noise => model valid

# Excluding Covid and the crisis of 2001
short_growth <- window(growth_ts,c(2002,1), c(2019,1))
short_infl <- window(infl_ts,c(2002,1), c(2019,1))
fit3 <- tslm(short_growth ~ short_infl)
summary(fit3)
ts.plot(fit3$residuals) 
checkresiduals(fit3)
Box.test(fit3$residuals, lag = round(sqrt(length(fit3$residuals))), type = "Ljung-Box") # => not white noise => Model cannot be validated.

# Seasonal growth and inflation
sgrowth <- diff(short_growth, lag = 4)
sinfl <- diff(short_infl, lag = 4)
fit4 <- tslm(sgrowth ~ sinfl)
summary(fit4)
ts.plot(fit4$residuals) 
checkresiduals(fit4)
Box.test(fit4$residuals, lag = round(sqrt(length(fit4$residuals))), type = "Ljung-Box") # => not white noise => Model cannot be validated.

## Cross-Correlation

ccf(growth_ts, infl_ts)

# DLM ---------------------------------------------------------------------------------------
# 
maxlag <- round(sqrt(length(growth_ts)))
CADFtest(growth_ts, type = "drift", criterion = "BIC", max.lag.y = maxlag) # => <5% We reject H0 and conclude
# that the time series is stationary. => i.e., integrated of order one (not stationary in levels, but stationary in differences).

maxlag <- round(sqrt(length(infl_ts)))
CADFtest(infl_ts, type = "drift", criterion = "BIC", max.lag.y = maxlag) # => >5% We cannot reject H0 therefore conclude
# that the time series is not stationary. 
autoplot(infl_ts)
acf(infl_ts)
pacf(infl_ts)

sinfl_ts <- diff(infl_ts, lag = 4)
autoplot(infl_ts)
acf(sinfl_ts)
pacf(sinfl_ts)
maxlag <- round(sqrt(length(sinfl_ts)))
CADFtest(sinfl_ts, type = "drift", criterion = "BIC", max.lag.y = maxlag) # => <5% We reject H0 and conclude
# that the time series is stationary. => integrated of order 2.

length(growth_ts)
length(sinfl_ts)
growth_ts <- window(growth_ts, start=c(1961,2))

# DLM(1)
lag <- 1
n <- length(growth_ts)
growth.0 <- growth_ts[(lag+1):n]
sinfl.0 <- sinfl_ts[(lag+1):n]
sinfl.1 <- sinfl_ts[lag:(n-1)]
fit_dlm1 <- lm(growth.0 ~ sinfl.0 + sinfl.1)
summary(fit_dlm1)
plot.ts(fit_dlm1$residuals)
acf(fit_dlm1$residuals) 
Box.test(fit_dlm1$residuals, lag = round(sqrt(length(fit_dlm1$residuals))), type = "Ljung-Box") # valid model!

# DLM(2)
lag <- 2
n <- length(growth_ts)
growth.0 <- growth_ts[(lag+1):n]
sinfl.0 <- sinfl_ts[(lag+1):n]
sinfl.1 <- sinfl_ts[lag:(n-1)]
sinfl.2 <- sinfl_ts[(lag-1):(n-2)]
fit_dlm2 <- lm(growth.0 ~ sinfl.0 + sinfl.1 + sinfl.2)
summary(fit_dlm2)
plot.ts(fit_dlm2$residuals)
acf(fit_dlm2$residuals) 
Box.test(fit_dlm2$residuals, lag = round(sqrt(length(fit_dlm1$residuals))), type = "Ljung-Box") # valid model!

# ADLM of order 1 (i.e. ADLM(1)): 
lag <- 1 
growth.0 <- growth_ts[(lag+1):n] 
sinfl.0 <- sinfl_ts[(lag+1):n] 
growth.1 <- growth_ts[lag:(n-1)] 
sinfl.1 <- sinfl_ts[lag:(n-1)] 
fit_adlm1 <- lm(growth.0 ~ growth.1 +sinfl.0 + sinfl.1)
summary(fit_adlm1)

acf(fit_adlm1$residuals) 
Box.test(fit_adlm1$residuals, lag = round(sqrt(length(fit_adlm1$residuals))), type = "Ljung-Box") # => white noise => Model validated.


# ADLM of order 2 (i.e. ADLM(2)): 
lag <- 2 
growth.0 <- growth_ts[(lag+1):n] 
sinfl.0 <- sinfl_ts[(lag+1):n] 
growth.1 <- growth_ts[lag:(n-1)] 
sinfl.1 <- sinfl_ts[lag:(n-1)] 
growth.2 <- growth_ts[(lag-1):(n-2)] 
sinfl.2 <- sinfl_ts[(lag-1):(n-2)]
fit_adlm2 <- lm(growth.0 ~ growth.1+growth.2+sinfl.0 + sinfl.1+sinfl.2)
summary(fit_adlm2)

acf(fit_adlm2$residuals) 
Box.test(fit_adlm2$residuals, lag = round(sqrt(length(fit_adlm2$residuals))), type = "Ljung-Box") # => white noise => Model validated.


# ADLM of order 3 (i.e. ADLM(3)): 
lag <- 3 
growth.0 <- growth_ts[(lag+1):n] 
sinfl.0 <- sinfl_ts[(lag+1):n] 
growth.1 <- growth_ts[lag:(n-1)] 
sinfl.1 <- sinfl_ts[lag:(n-1)] 
growth.2 <- growth_ts[(lag-1):(n-2)] 
sinfl.2 <- sinfl_ts[(lag-1):(n-2)]
growth.3 <- growth_ts[(lag-2):(n-3)] 
sinfl.3 <- sinfl_ts[(lag-2):(n-3)] 
fit_adlm3 <- lm(growth.0 ~ growth.1 + growth.2 + growth.3 + sinfl.0 + sinfl.1 + sinfl.2 + sinfl.3)
summary(fit_adlm3)

acf(fit_adlm3$residuals) 
Box.test(fit_adlm3$residuals, lag = round(sqrt(length(fit_adlm3$residuals))), type = "Ljung-Box") # => white noise => Model validated.

# ADLM of order 4 (i.e. ADLM(4)): 
lag <- 4 
growth.0 <- growth_ts[(lag+1):n] 
sinfl.0 <- sinfl_ts[(lag+1):n] 
growth.1 <- growth_ts[lag:(n-1)] 
sinfl.1 <- sinfl_ts[lag:(n-1)] 
growth.2 <- growth_ts[(lag-1):(n-2)] 
sinfl.2 <- sinfl_ts[(lag-1):(n-2)]
growth.3 <- growth_ts[(lag-2):(n-3)] 
sinfl.3 <- sinfl_ts[(lag-2):(n-3)] 
growth.4 <- growth_ts[(lag-3):(n-4)] 
sinfl.4 <- sinfl_ts[(lag-3):(n-4)]
fit_adlm4 <- lm(growth.0 ~ growth.1 + growth.2 + growth.3 + growth.4 + sinfl.0 + sinfl.1 + sinfl.2 + sinfl.3 + sinfl.4)
summary(fit_adlm4)

acf(fit_adlm4$residuals) 
Box.test(fit_adlm4$residuals, lag = round(sqrt(length(fit_adlm4$residuals))), type = "Ljung-Box") # => white noise => Model validated.

# ADLM of order 5 (i.e. ADLM(5)): 
lag <- 5 
growth.0 <- growth_ts[(lag+1):n] 
sinfl.0 <- sinfl_ts[(lag+1):n] 
growth.1 <- growth_ts[lag:(n-1)] 
sinfl.1 <- sinfl_ts[lag:(n-1)] 
growth.2 <- growth_ts[(lag-1):(n-2)] 
sinfl.2 <- sinfl_ts[(lag-1):(n-2)]
growth.3 <- growth_ts[(lag-2):(n-3)] 
sinfl.3 <- sinfl_ts[(lag-2):(n-3)] 
growth.4 <- growth_ts[(lag-3):(n-4)] 
sinfl.4 <- sinfl_ts[(lag-3):(n-4)]
growth.5 <- growth_ts[(lag-4):(n-5)] 
sinfl.5 <- sinfl_ts[(lag-4):(n-5)] 
fit_adlm5 <- lm(growth.0 ~ growth.1 + growth.2 + growth.3 + growth.4 + growth.5 + sinfl.0 + sinfl.1 +sinfl.2 + sinfl.3 + sinfl.4 + sinfl.5)
summary(fit_adlm5)
 
acf(fit_adlm5$residuals) 
Box.test(fit_adlm5$residuals, lag = round(sqrt(length(fit_adlm5$residuals))), type = "Ljung-Box") # => white noise => Model validated.

## How do you interpret the R2? => % total variation explained in growth by the regressors in the model. Seasonal inflation lag x is significant.
# Regressors are jointly (not) significant.

# Granger causality - comparing the ADLM(3) with the model without lagged explanatory variables: 
fit_adlm_nox <- lm(growth.0 ~ growth.1 + growth.2 + growth.3) 
summary(fit_adlm_nox)
anova(fit_adlm3,fit_adlm_nox) 
# thus we do not reject H0 of no Granger Causality. We conclude that seasonal inflation has 
# no incremental explanatory power in real GDP growth.
# All models are validated because there are no significant correlations of 
# residuals and residuals are all white noise.

# Cointegration --------------------------------------------------------------------------------
wlogrgdp <- window((log(rgdp_ts)), start=c(1960, 2))
fit_ci <- tslm(wlogrgdp ~ infl_ts)
res_fit_ci <- fit_ci$residuals
# unit root test on the residuals to check their stationarity
CADFtest(res_fit_ci,type="drift",criterion="BIC",max.lag.y=round(sqrt(length(res_fit_ci))))
# We obtain a test-statistics -1.3538 which is larger that the Engle-Granger ADF test statistics 
# for one explanatory variable −3.41. Thus, we cannot reject H0 of no cointegration and 
# therefore conclude that log real GDP and inflation are not cointegrated.                                                           

# Cointegration-Johansen test -----------------------------------------------------------------
mydata <- data.frame(wlogrgdp, infl_ts)
names(mydata)<-c("logrGDP","Infl")
attach(mydata)
mymax.lag=round(sqrt(length(logrGDP))) 
CADFtest(logrGDP,type="drift",criterion="BIC",max.lag.y=mymax.lag) 
CADFtest(Infl,type="drift",criterion="BIC",max.lag.y=mymax.lag) 
Growth_var <-diff(logrGDP) 
dInfl_var <-diff(Infl) 
CADFtest(Growth_var,type="drift",criterion="BIC",max.lag.y=mymax.lag) 
CADFtest(dInfl_var,type="drift",criterion="BIC",max.lag.y=mymax.lag) 
# We conclude that both series are I(1).

logdata<-data.frame(logrGDP,Infl) 
names(logdata)<-c("logrGDP","Infl") 
attach(logdata) 
VARselect(logdata,lag.max=round(sqrt(length(Infl))),type="const")
# The order of the VAR model in levels selected by Schwarz Information Criterion is 5.

# Test for cointegration using Johansen’s trace test statistic:
trace_test<-ca.jo(logdata,type="trace",K=5,ecdet="const",spec="transitory") 
summary(trace_test)

# 99% Conf. Interval
# For r=0, the test statistics is larger than then the critical value (52.31 > 24.60), thus 
# there is at least one cointegrating relation. For r=1, the test statistics is smaller
# then the critical value (10.21 <12.97).Ergo there is one cointegrating relation =>
# Hence, log(rGDP) and Infl are cointegrated.

# There is cointegrating relation. 
# Let δt be a stationary time series, the cointegrating equation is
# -10.75265 +logGDP(t)+30.41Infl(t)

# If you reject both r = 0 and r <= 1 that means r = 2 (r cannot be greater than the number of 
# series in the system, which is 2). 
# That implies there are at least two different linear combinations of the variables that are stationary 
# (among combinations where the first weight is normalized to 1). That means the two series must be stationary to begin with, 
# and hence there is no cointegration.

# Same procedure using Johansen’s maximum eigenvalue test statistic
maxeigen_test<-ca.jo(logdata,type="eigen",K=5,ecdet="const",spec="transitory") 
summary(maxeigen_test) # We conclude that there is cointegration present.

# VECM Model ------------------------------------------------------------------------------------------
fit_vecm<-cajorls(trace_test,r=1) # number of cointegration equations (cfr. r=1) 
# and the function returns the estimated VECM.
fit_vecm
# # The cointegrating equation, which corresponds to the one reported above, is a stationary 
# linear combination of log(rGPS) and Infl. 
fit_vecm2<-cajorls(maxeigen_test,r=1)
fit_vecm2

fit_var<-vec2var(trace_test,r=1) 
myforecast<-predict(fit_var,n.ahead=6) 

par(mfrow=c(2,1)) 
ts.plot(logrGDP) 
logrGDP_forecast<-ts(myforecast$fcst$logrGDP[,1],frequency=4,start=c(2022,4)) 
logrGDP_lower<-ts(myforecast$fcst$logrGDP[,2],frequency=4,start=c(2022,4)) 
logrGDP_upper<-ts(myforecast$fcst$logrGDP[,3],frequency=4,start=c(2022,4)) 
ts.plot(logrGDP, logrGDP_forecast,logrGDP_lower,logrGDP_upper, col=c("blue","black","red","red"), xlim=c(2018, 2025), ylim=c(9,12))
ts.plot(logrGDP_forecast,logrGDP_lower,logrGDP_upper, col=c("black","red","red"))
title(main = "6-step-ahead forecast of logrGDP") 

plot.ts(Infl) 
Infl_forecast<-ts(myforecast$fcst$Infl[,1],frequency=4,start=c(2022,4)) 
Infl_lower<-ts(myforecast$fcst$Infl[,2],frequency=4,start=c(2022,4)) 
Infl_upper<-ts(myforecast$fcst$Infl[,3],frequency=4,start=c(2022,4)) 
ts.plot(Infl,Infl_forecast,Infl_lower,Infl_upper,col=c("blue","black","red","red"), xlim=c(2018, 2025)) 
ts.plot(Infl_forecast,Infl_lower,Infl_upper,col=c("black","red","red")) 
title(main = "6-step-ahead forecast of Infl")

detach(mydata)
