##################################################################
# This file loads daily historical stock prices for MSFT (Microsoft Corporation).
# This data will forecast the future daily closing stock price for the following 50 days. 
# The 50 day prediction stock price will be compared with the actual data for those days.
# 
#
# @author: Valentina Gonzalez Bohorquez 12/04/2020
##################################################################

#clear all variables in workspace
rm(list = ls())
install.packages("quantmod")
install.packages("tseries")
install.packages("timeSeries")
install.packages("forecast")
install.packages("xts")
install.packages("fpp2")
#Load the packages 
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)
library(fpp2)


stockSymbol <- c("MSFT") 

#Extract the MSFT dataset from Yahoo finance, by using getSymbols function
stock.symbReturned <- getSymbols(stockSymbol, from = "2000-01-01", to = "2020-01-01")

stock.data <- get(stock.symbReturned[1])

stock.data

corestockdata <- coredata(stock.data) #Saving the time series data in a matrix form for easy access and manipulation


chartSeries(stock.data, theme = "white" , name = stockSymbol[1], legend="top")
#green=closing price for the above graph
#orange=opening price for the above graph
#Bottom graph is displaying the MSFT volume that was traded during each particular day

#Analyze the MSFT closing prices, in order to predict the following 50 days closing prices.
#The fourth column displays the closing price
MSFT_ClosePrice = MSFT[,4]
#class(MSFT_ClosePrice)
plot(MSFT_ClosePrice)

#Test for Stationary
#Perform the Augmented Dickey-Filler (ADF) test for stationarity and to find the p-value.
#The null hypothesis states that large p values indicate non-stationarity and smaller p values indicate stationarity.
#The smaller the p-value, the more accurate it is.

adf.test(MSFT_ClosePrice)
#The p-value= 0.99. 
#Dickey-Fuller= 1.6659
#Lag order = 17
#Alternative Hypothesis: Stationary

#You can see our p value for the ADF test is relatively high. 
#For that reason, we need to do some further visual inspection.

##########################################################################
#Graph the ACF and PACF to analyze the lags of the MSFT-Closeprice.
# ACF= q and PACF= p
par(mfrow=c(1,2))
Acf(MSFT_ClosePrice, main= "ACF") #The linear decline in ACF plot also indicates non-stationarity
Pacf(MSFT_ClosePrice, main= 'PACF')
##########################################################################

#Taking the first difference of the MSFT Closing price
diffMSFTCloseprice1 <- diff(MSFT_ClosePrice)

par(mfrow=c(1,1))
plot(diffMSFTCloseprice1) #The plot is almost horizontal indicating d=1 achieves stationarity

ggtsdisplay(diffMSFTCloseprice1)
#plotted the difference, the acf and pacf of the diffMSFTCloseprice1

##########################################################################
#Let's use the auto.arima() to create one model.

summary(auto.arima(MSFT_ClosePrice, seasonal = FALSE))

#auto.arima() suggests a (1,2,0) model to fit the data. So, let's consider this as one of our models.

##########################################################################
#Create 4 models

arimaFit = auto.arima(MSFT_ClosePrice, seasonal = FALSE) #Using auto.arima() to get the suggested model
tsdisplay(residuals(arimaFit), lag.max = 40, main = '(1,2,0) Model Residuals') #Analyze its ACF and PACF plots

#We see that on the ACF plot, the blue dotted line is crossed at multiple places right from the beginning
#So, we choose p values based on these positions
#Model2: p = 7 because that's the position after which there is a significantly discernible cutoff beyond the blue line
#Model3: p = 26 because that's the right most position immediately after which there is a cutoff beyond the blue line (but barely)
#Model4: p = 1 because the cut off starts right at the beginning in the ACF plot

arimaFit2 = arima(MSFT_ClosePrice, order = c(1,1,7))
tsdisplay(residuals(arimaFit2), lag.max = 40, main = '(1,1,7) Model Residuals')
#We see that the model residuals plot is almost horizontal. So, d=1 suffices and we do not need to use d=2. 
#We still see some cutoffs in the ACF and PACF plots. But we are going to test the accuracy of this model for our predictions.


arimaFit3 = arima(MSFT_ClosePrice, order = c(1,1,26))
tsdisplay(residuals(arimaFit3), lag.max = 40, main = '(1,1,26) Model Residuals')
#We see that the ACF and PACF plots are quite well fit now. In fact, they seem to be fit bit too much.
#This could very likely be a classic case of over-fitting. We'll test this model's accuracy nonetheless.


arimaFit4 = arima(MSFT_ClosePrice, order = c(1,1,1))
tsdisplay(residuals(arimaFit4), lag.max = 40, main = '(1,1,1) Model Residuals')
#We still see quite a few cut off points in the ACF and PACF plots.
#We are going to test this model's accuracy and compare it against the remaining.


##########################################################################

#Getting the stockdata for the next few days to compare to the predictions of our models
stock.symbReturned <- getSymbols(stockSymbol, from="2020-01-02", to="2020-03-31")

stock.data.next50 <- get(stock.symbReturned[1])

stockdatanext50 <- coredata(stock.data.next50) #converting the ts data into a matrix

stock.datanext50<- c(stockdatanext50[,4]) #Convert the pertinent column in the matrix to a vector for easy manipulation

##########################################################################
#ARIMA(1,2,0): Model1  Predictions for the MSFT Closing Prices for the next 50 days 
fit1next50 <- predict(arimaFit, n.ahead = 50) #Using predict function to get next 50 predictions for the first model

corefit1next50 <- as.numeric(unlist(fit1next50)) #Convert the predictions into a numeric vector

fit1next50predictions <- corefit1next50[1:50] #Choosing only the first 50 values which are pertinent for our purpose

difference120 <- vector(mode="numeric", length=50) #Vector to store the difference between the model prediction and the actual stock closing value

errorpercent120 <- vector(mode="numeric", length=50) #Vector to store the errors as a percent of the actual closing value of the stock price

accuracypercent120 <- vector(mode="numeric", length=50) #Vector to store the per-day accuracy of the model for the next 50 days


for (i in 1:50){
  difference120[i] <- abs(fit1next50predictions[i] - stock.datanext50[i])
  errorpercent120[i] <- (difference120[i]/stock.datanext50[i])* 100
  accuracypercent120[i] <- 100 - errorpercent120[i]
}

difference120
errorpercent120
accuracypercent120

plot(errorpercent120)
plot(accuracypercent120, main = "Day-wise prediction accuracy for ARIMA(1,2,0)") #Model 1 has a very a good prediction accuracy for the initial few days but it drops sharply later. 
min(accuracypercent120)
which.min(accuracypercent120)

#The accuracy is >95% for the first 7 days. 
#But drops sharply and raeches a minimum of 77.2% around the 33rd day mark

##########################################################################
#ARIMA(1,1,7): Model2  Predictions for the MSFT Closing Prices for the next 50 days 
fit2next50 <- predict(arimaFit2, n.ahead = 50) #Using predict function to get next 50 predictions for the first model


corefit2next50 <- as.numeric(unlist(fit2next50)) #Convert the predictions into a numeric vector

fit2next50predictions <- corefit2next50[1:50] #Choosing only the first 50 values which are pertinent for our purpose

difference117 <- vector(mode="numeric", length=50) #Vector to store the difference between the model prediction and the actual stock closing value

errorpercent117 <- vector(mode="numeric", length=50) #Vector to store the errors as a percent of the actual closing value of the stock price

accuracypercent117 <- vector(mode="numeric", length=50) #Vector to store the per-day accuracy of the model for the next 50 days


for (i in 1:50){
  difference117[i] <- abs(fit2next50predictions[i] - stock.datanext50[i])
  errorpercent117[i] <- (difference117[i]/stock.datanext50[i])* 100
  accuracypercent117[i] <- 100 - errorpercent117[i]
}

difference117
errorpercent117
accuracypercent117


plot(errorpercent117)
plot(accuracypercent117, main = "Day-wise prediction accuracy for ARIMA(1,1,7)") #Model 2 has a very a good prediction accuracy for the initial 18 days but it drops sharply later. 
min(accuracypercent117)
which.min(accuracypercent117)

#The accuracy is >95% for the first 10 days, around 94.5% for the next three days, and then again >95% until day 18 (except on day 15) 
#it drops sharply after that and reaches a minimum of 83.6% around the 27th day mark

##########################################################################
#ARIMA(1,1,26): Model3  Predictions for the MSFT Closing Prices for the next 50 days 
fit3next50 <- predict(arimaFit3, n.ahead = 50) #Using predict function to get next 50 predictions for the first model

corefit3next50 <- as.numeric(unlist(fit3next50)) #Convert the predictions into a numeric vector

fit3next50predictions <- corefit3next50[1:50] #Choosing only the first 50 values which are pertinent for our purpose

difference1126 <- vector(mode="numeric", length=50) #Vector to store the difference between the model prediction and the actual stock closing value

errorpercent1126 <- vector(mode="numeric", length=50) #Vector to store the errors as a percent of the actual closing value of the stock price

accuracypercent1126 <- vector(mode="numeric", length=50) #Vector to store the per-day accuracy of the model for the next 50 days


for (i in 1:50){
  difference1126[i] <- abs(fit3next50predictions[i] - stock.datanext50[i])
  errorpercent1126[i] <- (difference1126[i]/stock.datanext50[i])* 100
  accuracypercent1126[i] <- 100 - errorpercent1126[i]
}

difference1126
accuracypercent1126
errorpercent1126

plot(errorpercent1126)
plot(accuracypercent1126, main = "Day-wise prediction accuracy for ARIMA(1,1,26)") #Model 3 has a very a good prediction accuracy for the initial 17 days but it drops sharply later. 
min(accuracypercent1126)
which.min(accuracypercent1126)

#The accuracy is >95% for the first 11 days, around 94.5% for the next two days, and then again >95% until day 18 (except day 15) 
#it drops sharply after that and reaches a minimum of 83.57% around the 27th day mark

##########################################################################
#ARIMA(1,1,1): Model4  Predictions for the MSFT Closing Prices for the next 50 days 
fit4next50 <- predict(arimaFit4, n.ahead = 50) #Using predict function to get next 50 predictions for the first model

corefit4next50 <- as.numeric(unlist(fit4next50)) #Convert the predictions into a numeric vector

fit4next50predictions <- corefit4next50[1:50] #Choosing only the first 50 values which are pertinent for our purpose

difference111 <- vector(mode="numeric", length=50) #Vector to store the difference between the model prediction and the actual stock closing value

errorpercent111 <- vector(mode="numeric", length=50) #Vector to store the errors as a percent of the actual closing value of the stock price

accuracypercent111 <- vector(mode="numeric", length=50) #Vector to store the per-day accuracy of the model for the next 50 days


for (i in 1:50){
  difference111[i] <- abs(fit4next50predictions[i] - stock.datanext50[i])
  errorpercent111[i] <- (difference111[i]/stock.datanext50[i])* 100
  accuracypercent111[i] <- 100 - errorpercent111[i]
}

accuracypercent111
errorpercent111

plot(errorpercent111)
plot(accuracypercent111, main = "Day-wise prediction accuracy for ARIMA(1,1,1)") #Model 4 has a very a good prediction accuracy for the initial 18 days but it drops sharply later. 
min(accuracypercent111)
which.min(accuracypercent111)

#The accuracy is >95% for the first 10 days, around 94.5% for the next three days, and then again >95% until day 18 (except on day 15) 
#it drops sharply after that and reaches a minimum of 83.59% around the 27th day mark

##########################################################################
#Clearly Model 4 (ARIMA(1,1,1)) works quite comparably to Models 2 and 3 which we created after analyzing the ACF and PACF plots

#And clearly, Models 2,3,4 work quite better than Model 1 which was created using the auto.arima() function.

#Conclusions: 
#1. For the purpose of predicting for future values of stock price within the initial 6 days, the model fit generated by auto.arima would suffice.
#2. For accurate predictions beyond initial 6 days upto 10-11 days into the future, a simple ARIMA (1,1,1) model has a very good and comparable accuracy to the ARIMA(1,1,7) and ARIMA(1,1,26) models which were generated after analyzing the ACF plots

##########################################################################

accuracytable = matrix(, nrow = 50, ncol = 4)

colnames(accuracytable) = c("(1,2,0)", "(1,1,7)", "(1,1,26)", "(1,1,1)")

for (i in 1:50){
  accuracytable[i,1] <- accuracypercent120[i]
}

for (i in 1:50){
  accuracytable[i,2] <- accuracypercent117[i]
}

for (i in 1:50){
  accuracytable[i,3] <- accuracypercent1126[i]
}

for (i in 1:50){
  accuracytable[i,4] <- accuracypercent111[i]
}
accuracytable

