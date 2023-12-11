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

################### code quartely ############################


rm(list = ls())
#load data : CPIs.RData
load("Rent_fore_q.RData")
CPIs <- Rent_fore_q
rm(Rent_fore_q)
#
#rename the columns
colnames(CPIs) <- c( "Mortgage", "Rent", "Date")
#change to numeric
CPIs$Rent <- as.numeric(CPIs$Rent)
CPIs$Mortgage <- as.numeric(CPIs$Mortgage)

# #calculate the Ren and Mortgage.rate over the year inflation rate
Rent <- log(CPIs$Rent/lag(CPIs$Rent,4))
Mortgage <- log(CPIs$Mortgage/lag(CPIs$Mortgage,4))
#Rent <- ts(CPIs$Rent, start = c(2008,9), frequency = )
#Mortgage <- ts(CPIs$Mortgage, start = c(2008,9), frequency = 12)
#plot the inflation rate
#plot(Rent, type = "l", col = "red", xlab = "Year", ylab = "Inflation rate", main = "Inflation rate of rent")
#plot(Mortgage, type = "l", col = "red", xlab = "Year", ylab = "Inflation rate", main = "Inflation rate of mortgage")

length(Rent)
length(Mortgage)
Mortgage <- ts(Mortgage, start = c(2009,9), frequency = 4)
Rent <- ts(Rent, start = c(2009,9), frequency = 4)
Rent <- na.omit(Rent)
Mortgage <- na.omit(Mortgage)
Mortgage <- ts(Mortgage, start = c(2009,9), frequency = 4)
Rent <- ts(Rent, start = c(2009,9), frequency = 4)


# Create the fourth lag of this difference
Mortgage <- as.numeric(Mortgage)
mortgage.rate.lag4 <- lag(Mortgage, 4)

Mortgage <- ts(mortgage.rate.lag4, start = c(2009,9), frequency = 4)
 #Remove the NA values that come from lagging
Mortgage <- na.omit(Mortgage)
Mortgage <- ts(Mortgage, start = c(2010,9), frequency = 4)

Rent <- as.numeric(Rent)
Rent <- Rent[-c(1:4)]
Rent <- ts(Rent, start = c(2010,9), frequency = 4)

length(Rent)
length(Mortgage)

fit <- auto.arima(Rent, xreg = Mortgage, seasonal = FALSE, approximation = FALSE, trace=TRUE)

fit2 <- auto.arima(Mortgage, seasonal = FALSE, approximation = FALSE, trace=TRUE)
forecast_mortgage <- forecast(fit2, h = 12)
plot(forecast_mortgage)
#forecast_mortgage <- ts(forecast_mortgage$mean, start = c(2025,1), frequency = 4)
plot(forecast_mortgage)

#forecast CPIs$Rent using CPIs$Mortgage as exogenous variable
forecast_rent <- forecast(fit, xreg = forecast_mortgage$mean, h = 12)
plot(forecast_rent)


######
# In sample tests
######

#calculate the in sample residuals
in_sample_residuals <- fit$residuals
#calculate the in sample RMSE
in_sample_RMSE <- sqrt(mean(in_sample_residuals^2))
#calculate the in sample MAE
in_sample_MAE <- mean(abs(in_sample_residuals))

base_stat <- data.frame(in_sample_RMSE, in_sample_MAE)
rm(in_sample_MAE, in_sample_RMSE)
# Ljung Box-Q Test
Ljung <- Box.test(in_sample_residuals, lag = 10, type = "Ljung-Box", fitdf = 3)
# White Test
Pierce <- Box.test(in_sample_residuals, lag = 10, type = "Box-Pierce", fitdf = 3)
# jarque bera test
Jarques <- jarque.bera.test(in_sample_residuals)
# White Test
White <- white_test(fit)

in_sample_tests <- data.frame(Ljung$p.value, White$p_value, Jarques$p.value,Pierce$p.value)
rm(Ljung, Pierce, Jarques, White)








### out of sample forecast for benchmark model

the_Rent <- ts(Rent, start = c(2009,9), frequency = 4)
the_Mort <- ts(Mortgage, start = c(2009,9), frequency = 4)
out_of_sample <- data.frame(matrix(ncol = 1, nrow = 12))
mean_of_fit <- data.frame(matrix(ncol = 1, nrow = 12))
end <- length(the_Rent)
end <- end - 12
plot(the_Rent)

#iterate from line 36 to the en of CPIs
for (i in 13:end){
    temporary <- the_Rent[1:i-1]
    temporary <- ts(temporary, start = c(2009,9), frequency = 4)
    temporary_m <- the_Mort[1:i-1]
    temporary_m <- ts(temporary_m, start = c(2009,9), frequency = 4)
    end_year <- end(temporary)[1]
    end_month <- end(temporary)[2]
    #fit arima model on the first i-1 observations
    fit <- arima(temporary, order = c(1,0,1), xreg = temporary_m)
    fit_m <- arima(temporary_m, order = c(2,0,1))
    forecast_fit <- forecast(fit_m, h = 12)
    #forecast the i-th observation
    fore <- predict(fit, newxreg=forecast_fit$mean, h = 12)
    print <- ts(fore$pred, start = c(end_year, end_month + 1 ), frequency = 4)
    lines(print, col="red")
    #calculate the out of sample forecast
    to_save <- (as.numeric(fore$pred) - the_Mort[i:i+12])^2
    out_of_sample <- data.frame(out_of_sample, to_save)
    mean_of_fit <- data.frame(mean_of_fit, fore$pred)

}

#dev.off()
#remove first column
Squared <- out_of_sample[,-1]
MSFE_by_time <- colMeans(Squared[1,] , na.rm = TRUE)
for (i in 2:12){
    MSFE_by_time <- rbind(MSFE_by_time, colMeans(Squared[1:i,] , na.rm = TRUE))
}
MSFE_Total <- rowMeans(MSFE_by_time, na.rm = TRUE)
#rm(end, end_month, end_year, fore, i, mean_of_fit, out_of_sample, print, temporary, to_save)


### out of sample forecast for benchmark model

the_Rent_b <- ts(Rent, start = c(2009,9), frequency = 4)
the_Mort_b <- ts(Mortgage, start = c(2009,9), frequency = 4)
out_of_sample_b <- data.frame(matrix(ncol = 1, nrow = 12))
mean_of_fit_b <- data.frame(matrix(ncol = 1, nrow = 12))
end_b <- length(the_Rent_b)
end_b <- end_b - 12
plot(the_Rent_b)

#iterate from line 36 to the en of CPIs
for (i in 13:end_b){
    temporary <- the_Rent_b[1:i-1]
    temporary <- ts(temporary, start = c(2009,9), frequency = 4)
    temporary_m <- the_Mort_b[1:i-1]
    temporary_m <- ts(temporary_m, start = c(2009,9), frequency = 4)
    end_year <- end(temporary)[1]
    end_month <- end(temporary)[2]
    #fit arima model on the first i-1 observations
    fit <- arima(temporary, order = c(1,0,0), xreg = temporary_m)
    fit_m <- arima(temporary_m, order = c(1,0,1))
    forecast_fit <- forecast(fit_m, h = 12)
    #forecast the i-th observation
    fore <- predict(fit, newxreg=forecast_fit$mean, h = 12)
    print <- ts(fore$pred, start = c(end_year, end_month + 1 ), frequency = 4)
    lines(print, col="red")
    #calculate the out of sample forecast
    to_save_b <- (as.numeric(fore$pred) - the_Mort[i:i+12])^2
    out_of_sample_b <- data.frame(out_of_sample_b, to_save_b)
    mean_of_fit_b <- data.frame(mean_of_fit_b, fore$pred)

}

#dev.off()
#remove first column
Squared_b <- out_of_sample_b[,-1]
MSFE_by_time_b <- colMeans(Squared_b[1,] , na.rm = TRUE)
for (i in 2:12){
    MSFE_by_time_b <- rbind(MSFE_by_time_b, colMeans(Squared_b[1:i,] , na.rm = TRUE))
}
MSFE_Total_b <- rowMeans(MSFE_by_time_b, na.rm = TRUE)
#rm(end, end_month, end_year, fore, i, mean_of_fit, out_of_sample, print, temporary, to_save)



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
