# Ensemble of Time Series Models  
# 1. ARIMA (AR+I+MA models) 
# 2. Neural Network 
# 3. Smooting Method ETS
# 4. Random- walk on Hadoop


#Autoregressive Models (AR), Moving Average Models (MA) & Seasonal Regression Models
#Distributed Lags Models (I) & Neural Net Models & ETS Models

#The ARIMA model combines three basic methods:
  
#AutoRegression (AR) – In auto-regression the values of a given time series data are
#                      regressed on their own lagged values, which is indicated by the
#                      “p” value in the model.
#Differencing (I-for Integrated) – This involves differencing the time series data to
#                      remove the trend and convert a non-stationary time series to a
#                      stationary one. This is indicated by the “d” value in the model.
#                      If d = 1, it looks at the difference between two time series 
#                      entries, if d = 2 it looks at the differences of the differences
#                      obtained at d =1, and so forth.
#Moving Average (MA) – The moving average nature of the model is represented by the “q”
#                      value which is the number of lagged values of the error term.
library(quantmod);
library(tseries);
library(ggplot2)
library(timeSeries);
library(forecast);
library(xts);

# Pull data from Yahoo finance 
AAPL = getSymbols('AAPL', from='2012-01-01', to='2018-04-06',auto.assign = FALSE)
AAPL = na.omit(AAPL)

# Select the relevant close price series
stock_prices = AAPL[,4]

#Autoplot

# Complete plots
plot(stock_prices,main="Historical data of Stock Prices")

# Ensemble of time series models.

#ARIMA Model
# Compute the log returns for the stock
stock = diff(log(stock_prices),lag=1)
stock = stock[!is.na(stock)]

# Plot log returns 
plot(stock,type='l', main='log returns plot')

# Conduct ADF test on log returns series
# Checking for stationarity in the data
print(adf.test(stock))

# Split the dataset in two parts - training and testing
breakpoint = floor(nrow(stock)*(2.9/3))

# Initialzing an xts object for Actual log returns
Actual_series = xts(0,as.Date("2014-11-25","%Y-%m-%d"))

# Initialzing a dataframe for the forecasted return series
forecasted_series = data.frame(Forecasted = numeric())
# Initialzing an xts object for Actual log returns
Actual_series = xts(0,as.Date("2014-11-25","%Y-%m-%d"))

# Initialzing a dataframe for the forecasted return series
forecasted_series = data.frame(Forecasted = numeric())

# get p, d & q's most likely values
# auto arima selects the best model using smallest AIC, AICC or BIC value.
# AIC, AICC or BIC = penalized prediction error. More complex models are penalized more
fitauto = auto.arima(stock_prices)
fitauto

# Apply the ACF and PACF functions
# model is adequate if residuals contain no pattern, i.e. residuals are independent

res <- residuals(fitauto)
tsdisplay(res)

for (b in breakpoint:(nrow(stock)-1)) {
  
  stock_train = stock[1:b, ]
  stock_test = stock[(b+1):nrow(stock), ]
  
  # Summary of the ARIMA model using the determined (p,d,q) parameters
  fit = arima(stock_train, order = arimaorder(fitauto),include.mean=FALSE)
  summary(fit)
  
  # Forecasting the log returns
  arima.forecast = forecast(fit, h = 10,level=99)
  summary(arima.forecast)
  
  # plotting the forecast
  par(mfrow=c(1,1))
  plot(arima.forecast, main = "ARIMA Forecast")
  
  # Creating a series of forecasted returns for the forecasted period
  forecasted_series = rbind(forecasted_series,arima.forecast$mean[1])
  colnames(forecasted_series) = c("Forecasted")
  
  # Creating a series of actual returns for the forecasted period
  Actual_return = stock[(b+1),]
  Actual_series = c(Actual_series,xts(Actual_return))
  rm(Actual_return)
  
  print(stock_prices[(b+1),])
  print(stock_prices[(b+2),])
  
}

# plotting arima plot of the residuals
plot(arima.forecast, main = "ARIMA Forecast")

# Predicting accuracy by comaparing to actual returns
# Adjust the length of the Actual return series
Actual_series = Actual_series[-1]

# Create a time series object of the forecasted series
forecasted_series = xts(forecasted_series,index(Actual_series))

# Create a plot of the two return series - Actual versus Forecasted
plot(Actual_series,type='l',lwd=2,col='red',main='Actual Returns Vs Forecasted Returns')
lines(forecasted_series,lwd=2,col='blue')
legend('bottom',title="Line types",box.lwd=2, box.col="darkgray", text.font=4, bg='lightgreen',cex=0.8,legend=c("Actual","Forecasted"),lty=c(1,1),lwd=c(2,2),col=c('red','blue'),xpd=TRUE)

# Create a table for the accuracy of the forecast
comparsion = merge(Actual_series,forecasted_series)
comparsion$Accuracy = sign(comparsion$Actual_series)==sign(comparsion$Forecasted)
print(comparsion)

# Make prediction

# Forecast of closing price of next 50 days
plot(forecast(fitauto,h=50))
points(1:length(stock_prices),fitted(fitauto),type="l",col="red")
# Compute the accuracy percentage metric
Accuracy_percentage = sum(comparsion$Accuracy == 1)*100/length(comparsion$Accuracy)
print(Accuracy_percentage)

#Neural Network

#Neural Net prediction for closing price of next 50 days
cp_nnet = nnetar(stock_prices,p=arimaorder(fitauto)[1])
plot(forecast(cp_nnet,h=50))
points(1:length(stock_prices),fitted(cp_nnet),type="l",col="red")

#Neural Net prediction for returns of next 10 days
pred_ret_nnet = nnetar(Actual_series,p=arimaorder(fitauto)[1])
plot(forecast(pred_ret_nnet,h=10))
points(1:length(Actual_series),fitted(pred_ret_nnet),type="l",col="red")

#Accuracy Calculation

#Accuracy in Stock Closing Price
stock_prices2 <- window(stock_prices,start='2012-01-01',end='2018-03-20')
stock_prices3 <- window(stock_prices,start='2018-03-20',end='2018-04-06')
print(cpnnet)

#Comparison of accuracy of ARIMA Vs Ensemble of Neural Net & ARIMA
accuracy(forecast(nnetar(stock_prices2,p=arimaorder(fitauto)[1]),h=12),stock_prices3)
accuracy(forecast(auto.arima(stock_prices2),h=12),stock_prices3)

#Closing Price
forecast(cp_nnet,h=2)

forecast(fitauto,h=2)


#ETS Smoothing model MAN and AAN

#ETS Smoothing Prediction for closing price of next 50 days
fit2 <- ets(model="MAN",stock_prices)
fc <- forecast(fit2,h=50)
plot(fc)
points(1:length(stock_prices),fitted(fit2),type="l",col="red")

#ETS Smoothing Prediction for returns of next 10 days
fit3 <- ets(model="AAN",Actual_series)
fc2 <- forecast(fit3,h=10)
plot(fc2)
points(1:length(Actual_series),fitted(fit3),type="l",col="red")

#Random Walk can now be used for finding 95% most likely percentage change