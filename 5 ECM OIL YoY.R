#clean space
rm(list = ls())
# ##import data in excell format from the first table only
if(!require(readxl)) install.packages("readxl")
if(!require(reshape2)) install.packages("reshape2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(lubridate)) install.packages("lubridate")
if(!require(urca)) install.packages("urca")
if(!require(ecm)) install.packages("ecm")
if(!require(forecast)) install.packages("forecast")
if(!require(tseries)) install.packages("tseries")
if(!require(lubridate)) install.packages("lubridate")
if(!require(zoo)) install.packages("zoo")
if(!require(tempdisagg)) install.packages("tempdisagg")
if(!require(openxlsx)) install.packages("openxlsx")
library(readxl)
library(reshape2)
library(ecm)
library(urca)
library(dplyr)
library(lubridate)
library(zoo)
library(forecast)
library(tseries)
library(lubridate)
library(tempdisagg)
library(openxlsx)

## import the data ##

######
# 1 Crude oil price
######
#import MCOILWTICO.csv
ECM_Data <- read.csv("MCOILWTICO.csv", header = TRUE, sep = ",")
#convert oil_price$Date to R format from YYYY.MM.DD to monthly format
ECM_Data$Date <- as.Date(ECM_Data$DATE, format = "%Y-%m-%d")
#remove DATE and order columns with date first
ECM_Data <- ECM_Data[,c(3,2)]
#import usdchf.csv AFTER line 16
exchange_rate <- read.csv("EXSZUS.csv", header = TRUE, sep = ",")
exchange_rate$USD_to_CHF <- 1 / exchange_rate$EXSZUS
exchange_rate$Date_c <- as.Date(exchange_rate$DATE, format = "%Y-%m-%d")
#import USD_to_CHF.csv and Date_c to ECM data by subseting the dates by the earliest and latest date of the ECM_data
exchange_rate <-  subset(exchange_rate, Date_c >= "1986-01-01")
exchange_rate <-  subset(exchange_rate, Date_c <= "2023-09-01")
#merge oil_price and exchange_rate
ECM_Data <- merge(ECM_Data, exchange_rate, by.x = "Date", by.y = "Date_c", all.x = TRUE)
#if ECM_Data$Date == ECM_Data$DATE
ECM_Data$Date == ECM_Data$DATE
#remove exchange_rate
rm(exchange_rate)
#remove DATE and EXSZUS from ECM_Data
ECM_Data <- ECM_Data[,c(1,2,5)]
# convert oil_price$MCOILWTICO to swiss francs
ECM_Data$OIL_CHF <- ECM_Data$MCOILWTICO * ECM_Data$USD_to_CHF
# convert OIL_CHF in proportion of 2010-12-01 prices
ECM_Data$B20 <- (ECM_Data$OIL_CHF / ECM_Data$OIL_CHF[which(ECM_Data$Date == "2020-12-01")] ) * 100

######
# 2 Petroleum Products
######
load("CPIs.RData")

#remove CPIs dates below the first date of ECM_Data
CPIs <- subset(CPIs, CPIs$Year >= "1986-01-01")
# Bring Petroleum in ecm data
ECM_Data <- merge(ECM_Data, CPIs, by.x = "Date", by.y = "Year", all.x = TRUE)

## plot B20 and oil
plot(ECM_Data$B20, type = "l", col = "red")
lines(ECM_Data$Petroleum.products, type = "l", col = "blue")


## Convert on YoY inflation
ECM_Data$B20 <- log(ECM_Data$B20/lag(ECM_Data$B20,12))
ECM_Data$Petroleum.products <- log(ECM_Data$Petroleum.products/lag(ECM_Data$Petroleum.products,12))

plot(ECM_Data$B20, type = "l", col = "red")
lines(ECM_Data$Petroleum.products, type = "l", col = "blue")


#select from 2000
ECM_Data <- ECM_Data[169:nrow(ECM_Data),]
ECM_Data$B20ts <- ts(ECM_Data$B20, start = c(2000,1), frequency = 12)
ECM_Data$Petroleum.productsts <- ts(ECM_Data$Petroleum.products, start = c(2000,1), frequency = 12)
plot(ECM_Data$B20ts , type = "l", col = "red")
lines(ECM_Data$Petroleum.productsts, type = "l", col = "blue")

######
# 4 perfom checks before ECM
######
#check for stationarity of the data
adf.test(ECM_Data$B20)
#non stationnarity satisfied

adf.test(ECM_Data$Petroleum.products)
#non stationnarity satisfied

#check for cointegration between the two time series using urca package
test <- ca.jo(ECM_Data[,c(5,6)], type = "trace", ecdet = "const", K = 2, spec = "transitory")
#display the results
summary(test)


######
# 5 estimate the ecm
######
the_famous_ECM <- function (data){
lm1 <- lm(data$Petroleum.products~data$B20) #Create the linear regression

#create a lag ofe one for OIL and B10
data$B20_lag1 <- lag(data$B20,1)
data$Petroleum.products_lag1 <- lag(data$Petroleum.products,1)
#create a delta of OIL and B10
data$B20_delta <- (data$B20 - data$B20_lag1)
data$Petroleum.products_delta <- (data$Petroleum.products - data$Petroleum.products_lag1)
#create long term correction
data$long_term_correction <- data$Petroleum.products_lag1 - lm1$coefficients[1] - lm1$coefficients[2] * data$B20_lag1

lm2 <- lm(data$Petroleum.products_delta~data$B20_delta + data$long_term_correction ) #Create the linear regression

return(data)
}

the_famous_ECM_coeff <- function (data){
lm1 <- lm(data$Petroleum.products~data$B20) #Create the linear regression

#create a lag ofe one for OIL and B10
data$B20_lag1 <- lag(data$B20,1)
data$Petroleum.products_lag1 <- lag(data$Petroleum.products,1)
#create a delta of OIL and B10
data$B20_delta <- (data$B20 - data$B20_lag1)
data$Petroleum.products_delta <- (data$Petroleum.products - data$Petroleum.products_lag1)
#create long term correction
data$long_term_correction <- data$Petroleum.products_lag1 - lm1$coefficients[1] - lm1$coefficients[2] * data$B20_lag1

lm2 <- lm(data$Petroleum.products_delta~data$B20_delta + data$long_term_correction ) #Create the linear regression
#store the coefficionts in a dataframe
  lm1_1 <- lm1$coefficients[1]
  lm1_2 <- lm1$coefficients[2]
  lm2_1 <- lm2$coefficients[1]
  lm2_2 <- lm2$coefficients[2]
  lm2_3 <- lm2$coefficients[3]
  lm2_4 <- lm2$coefficients[4]
df <- data.frame(lm1_1, lm1_2, lm2_1, lm2_2, lm2_3, lm2_4)
return(df)
}
ECM_Data <- the_famous_ECM(ECM_Data)
coeff <- the_famous_ECM_coeff(ECM_Data)


#marche pas mais pas grave pour l'instant

######
# 5 Predictions
######

#create function to create data_forcast from different lenghts of row ECM_Data
create_data_forecast <- function(data_to_use, end_row, steps_ahead) {
  data_forecast <- data_to_use[1:end_row,]
  ll <- length(data_forecast$B20)
  new_rows <- data.frame(matrix(NA, nrow = 36, ncol = ncol(data_forecast)))
  colnames(new_rows) <- colnames(data_forecast)
  data_forecast <- rbind(data_forecast, new_rows)
  data_forecast$B20[ll+1:steps_ahead] <- rep(tail(data_forecast$B20[ll], 1), steps_ahead)
  data_forecast$B20_lag1[ll+1:steps_ahead] <- rep(tail(data_forecast$B20[ll], 1), steps_ahead)
  data_forecast$B20_delta[ll+1:steps_ahead] <- rep(0, steps_ahead)
  #data_forecast$B10_lag1[ll+1:12] <- tail(ECM_Data$B10, 12)
  #continue the date column
  #data_forecast$Date[ll+1:steps_ahead] <- seq(as.Date("2023-10-01"), by = "1 months", length.out = steps_ahead)
  return(data_forecast)
}


  # Function to forecast future values
forecast_ECM <- function(data_to_use,starting_row, steps_ahead,results_coeff) {
  temp <- data_to_use
  # Initialize the forecast dataframe with the last row of ECM_Data
  l_base <- starting_row
  # Iterate for the number of steps you want to forecast
  for(i in 1:steps_ahead-1) {
    # Create oil  lag 1

    temp$Petroleum.products_lag1[l_base + i] <- temp$Petroleum.products[l_base + i - 1]
    # Calculate long term correction
    temp$long_term_correction[l_base + i] <- temp$Petroleum.products_lag1[l_base + i] - results_coeff$lm1_1 -results_coeff$lm1_2* temp$B20_lag1[l_base + i]

    # Calculate OIL delta
    temp$Petroleum.products_delta[l_base + i] <- results_coeff$lm2_1  + results_coeff$lm2_3 * temp$long_term_correction[l_base + i]

    # Update the value of oil
    temp$Petroleum.products[l_base + i] <- temp$Petroleum.products_lag1[l_base + i] + temp$Petroleum.products_delta[l_base + i]


  }
  return(temp)
}

italian_dish <- function(end, data,steps_ahead, step,results_coeff){
  #loop on 4 by 4
  for(i in seq(from = steps_ahead, to=end, by=step)){
    # truncate the number of lines of data
    cherpa <- create_data_forecast(data, i, steps_ahead)
    forecast <- forecast_ECM(cherpa, i ,steps_ahead, results_coeff)
    toplot <- ts(forecast$Petroleum.products, start = c(2000,1), frequency = 12)
    lines(toplot, type = "l", col = "blue")
    }
}

test <- create_data_forecast(ECM_Data, nrow(ECM_Data),36)
test <- forecast_ECM(test, nrow(ECM_Data),36,coeff)
plot(test$Petroleum.products_delta, type = "l", col = "blue")
lines(ECM_Data$Petroleum.products_delta, type = "l", col = "red")
test2 <- create_data_forecast(ECM_Data, 222,36)
plot(test2$B20, type = "l", col = "blue")
plot(test$Petroleum.products, type = "l", col = "blue")
plot(test$B20, type = "l", col = "blue")

Petro_plot <- ts(ECM_Data$Petroleum.products, start = c(2000,1), frequency = 12)
plot(Petro_plot, type = "l", col = "red")
# Example usage: Forecasting 36
italian_dish(nrow(ECM_Data),ECM_Data,36,12,coeff)
lines(Petro_plot, type = "l", lwd = 2 ,col = "red")

######
# 6 assess the model
######


#generate and store for ECM
italian_dish_serving <- function(end, data,steps_ahead, step,results_coeff){
  out_of_sample_b <- data.frame(matrix(ncol = 1, nrow = 36))
  #loop on 4 by 4
  for(i in seq(from = steps_ahead, to=end, by=step)){
    # truncate the number of lines of data
    cherpa <- create_data_forecast(data, i, steps_ahead)
    forecast <- forecast_ECM(cherpa, i ,steps_ahead, results_coeff)
    toplot <- ts(forecast$Petroleum.products, start = c(2000,1), frequency = 12)
    lines(toplot, type = "l", col = "blue")
    out_of_sample_b <- data.frame(out_of_sample_b, tail(as.numeric(forecast$Petroleum.products),36))
    }
  return(out_of_sample_b)
}

toto <- italian_dish_serving(nrow(ECM_Data),ECM_Data,36,1,coeff)

#remove first column
Squared <- toto[,-1]
MSFE_by_time <- colMeans(Squared[1,] , na.rm = TRUE)
for (i in 2:36){
    MSFE_by_time <- rbind(MSFE_by_time, colMeans(Squared[1:i,] , na.rm = TRUE))
}
MSFE_Total <- rowMeans(MSFE_by_time, na.rm = TRUE)
#rm(end, end_month, end_year, fore, i, mean_of_fit, out_of_sample, print, temporary, to_save)




### compare with ar1 model

### out of sample forecast for benchmark model

Petroleo <- ts(ECM_Data$Petroleum.products,start = c(2000,1), frequency = 12)
out_of_sample_b <- data.frame(matrix(ncol = 1, nrow = 36))
mean_of_fit_b <- data.frame(matrix(ncol = 1, nrow = 36))
end_b <- nrow(ECM_Data)
end_b <- end_b - 36
plot(Petroleo , type = "l", col = "red")

#iterate from line 36 to the en of CPIs
for (i in 37:end_b){
    temporary <- Petroleo[1:i-1]
    temporary <- ts(temporary, start = c(2000,1), frequency = 12)
    end_year <- end(temporary)[1]
    end_month <- end(temporary)[2]
    #fit arima model on the first i-1 observations
    fit <- arima(temporary, order = c(1,0,0), method = "ML")

    #forecast the i-th observation
    fore <- forecast(fit, h = 36)
    print <- ts(fore$mean, start = c(end_year, end_month + 1 ), frequency = 12)
    if (i %in% seq(from = 1, to=end_b, by=10)){
        lines(print, col="yellow")
    }
    #calculate the out of sample forecast
    to_save <- (as.numeric(fore$mean) - Petroleo[i:i+36])^2
    out_of_sample_b <- data.frame(out_of_sample_b, to_save)
    mean_of_fit_b <- data.frame(mean_of_fit_b, fore$mean)

}

#Create MSFE by Time
Squared_b <- out_of_sample_b[,-1]

MSFE_by_time_b <- colMeans(Squared_b[1,] , na.rm = TRUE)
for (i in 2:36){
    MSFE_by_time_b <- rbind(MSFE_by_time_b, colMeans(Squared_b[1:i,] , na.rm = TRUE))
}
MSFE_Total_b <- rowMeans(MSFE_by_time_b, na.rm = TRUE)

rm(end_b, end_month, end_year, fore, i, mean_of_fit_b, out_of_sample_b, print, temporary, to_save)

MSFE_pred_by_time <- 1 - (MSFE_Total/MSFE_Total_b)
MSFE_pred <- 1 - (MSFE_Total[36]/MSFE_Total_b[36])

plot(MSFE_pred_by_time, type = "l", col = "red", xlab = "Time", ylab = "MSFE", main = "MSFE by time")









