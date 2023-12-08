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

rm(list = ls())
#load data : CPIs.RData
load("Rent_fore.RData")
CPIs <- Rent_fore
rm(Rent_fore)
#
#rename the columns
colnames(CPIs) <- c("Date", "Mortgage", "Rent")
#change to numeric
CPIs$Rent <- as.numeric(CPIs$Rent)
CPIs$Mortgage <- as.numeric(CPIs$Mortgage)

# #calculate the Ren and Mortgage.rate over the year inflation rate
Rent <- log(CPIs$Rent/lag(CPIs$Rent,12))
Mortgage <- log(CPIs$Mortgage/lag(CPIs$Mortgage,12))
#Rent <- ts(CPIs$Rent, start = c(2008,9), frequency = )
#Mortgage <- ts(CPIs$Mortgage, start = c(2008,9), frequency = 12)
#plot the inflation rate
#plot(Rent, type = "l", col = "red", xlab = "Year", ylab = "Inflation rate", main = "Inflation rate of rent")
#plot(Mortgage, type = "l", col = "red", xlab = "Year", ylab = "Inflation rate", main = "Inflation rate of mortgage")

length(Rent)
length(Mortgage)

# Create the fourth lag of this difference
mortgage.rate.lag4 <- lag(CPIs$Mortgage, 4)
 #Remove the NA values that come from lagging
Mortgage <- na.omit(mortgage.rate.lag4)
length(Mortgage)
Mortgage <- ts(Mortgage, start = c(2010,1), frequency = 12)

Rent <- Rent[-c(1:4)]
Rent <- ts(Rent, start = c(2010,1), frequency = )

length(Rent)
length(Mortgage)

fit <- auto.arima(Rent, xreg = Mortgage, seasonal = FALSE, approximation = FALSE, trace=TRUE)

fit2 <- auto.arima(Mortgage, seasonal = TRUE, approximation = FALSE, trace=TRUE)
forecast_mortgage <- forecast(fit2, h = 36)
forecast_mortgage <- ts(forecast_mortgage$mean, start = c(2010,1), frequency = 12)
plot(forecast_mortgage)

#forecast CPIs$Rent using CPIs$Mortgage as exogenous variable
forecast_rent <- forecast(fit, xreg = forecast_mortgage, h = 36)
plot(forecast_rent)


#Inflation.withoutRI_log <- ts(CPIs$Inflation.withoutRI_log,start = c(1984,1), frequency = 12)
#out_of_sample <- data.frame(matrix(ncol = 1, nrow = 36))
#mean_of_fit <- data.frame(matrix(ncol = 1, nrow = 36))
end <- length(Rent)
end <- end - 36
plot(Rent)
#iterate from line 36 to the en of CPIs
for (i in 37:end){

    temporary_r <- Rent[1:i-1]
    temporary_m <- Mortgage[1:i-1]
    temporary_r <- ts(temporary_r, start = c(2010,1), frequency = 36)
    end_year <- end(temporary_r)[1]
    end_month <- end(temporary_r)[2]
    #fit arima model on the first i-1 observations
    fit4 <- arima(temporary_r, xreg = temporary_m, order = c(1,0,0))
    fit5 <- arima(temporary_m, order = c(2,0,0))
    forecast_mortgage <- forecast(fit5, h = 12)
    forecast_mortgage <- ts(forecast_mortgage$mean, start = c(2010,1), frequency = 36)
    #forecast the i-th observation
    fore <- forecast(fit4, xreg = forecast_mortgage,h = 36)
    fore <- fore$mean
    print <- ts(fore, start = c(end_year, end_month + 1 ), frequency = 12)
    lines(print, col="red")
}




