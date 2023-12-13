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
Rent <- ts(CPIs$Rent, start = c(2009,3), frequency = 4)
Mortgage <- ts(CPIs$Mortgage, start = c(2009,3), frequency = 4)

plot(Rent, type = "l", col = "red", xlab = "Year", ylab = "Rent Inflation", main = "CPIs Rent YoY")
plot(Mortgage, type = "l", col = "red", xlab = "Year", ylab = "Mortgage Inflation", main = "CPIs Mortgage YoY")

fit <- auto.arima(Rent, stepwise=FALSE, seasonal = FALSE, approximation = FALSE, trace=TRUE, xreg = Mortgage)

fit3 <- arima(Rent, order = c(3,1,0), xreg = Mortgage)
fit4 <- auto.arima(Mortgage, stepwise=FALSE, seasonal = FALSE, approximation = FALSE, trace=TRUE)

tati <- Arima(Rent, model=fit3, xreg=Mortgage)
Mortgage2 <- forecast(fit4, h = 12)
Mortgage2 <- as.numeric(Mortgage2$mean)
test <- forecast(tati, xreg = Mortgage2)
plot(test)

Mortgage <- as.numeric(Mortgage2$mean)
Mortgage <- ts(Mortgage, start = c(2023,4), frequency = 4)
#rename the columns
colnames(toto) <- c( "Mortgage")
test <- forecast(fit3, Mortgage)
plot(test)



plot(forecast_rent,type = "l", col = "red", xlab = "Year", ylab = "Rent Inflation", main = "CPIs Rent YoY forecast")
difffff <- (diff(Rent))

adf.test(difffff, alternative = "stationary", k = 1)

forecast_rent <- forecast(fit, h = 12)
fit_m <- auto.arima(Mortgage, stepwise=FALSE, seasonal = FALSE, approximation = FALSE, trace=TRUE)
forecast_mortgage <- forecast(fit_m, h = 12)
plot(forecast_mortgage,type = "l", col = "red", xlab = "Year", ylab = "Rent Inflation", main = "CPIs Rent YoY forecast")

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



######
# Out of sample tests comparative
######

### out of sample forecast for our model

out_of_sample <- data.frame(matrix(ncol = 1, nrow = 4))
mean_of_fit <- data.frame(matrix(ncol = 1, nrow = nrow(CPIs)))
end <- nrow(CPIs)
end <- end - 12
# Line types
#iterate from line 36 to the en of CPIs
for (i in 30:end+1){
    temporary <- Rent[1:i-1]
    temporary <- ts(temporary, start = c(2009,4), frequency = 4)
    temporary_m <- Mortgage[1:i-1]
    temporary_m <- ts(temporary_m, start = c(2009,4), frequency = 4)
    end_year <- end(temporary)[1]
    end_month <- end(temporary)[2]
    #fit arima model on the first i-1 observations
    fit <- arima(temporary, order = c(3,1,0), xreg = temporary_m)
    model <- Arima(temporary, model=fit, xreg=temporary_m)
    fit_m <- arima(temporary_m, order = c(0,2,1))
    fore_m <- forecast(fit_m, h = 12)
    #forecast the i-th observation
    fore <- forecast(model, xreg = fore_m$mean, h = 12)
    #save the mean of the fit
    to_save <- c(temporary, fore$mean, rep(NA, end - i + 1))
    mean_of_fit <- data.frame(mean_of_fit, to_save)
    #save the error
    to_save_2 <- (fore$mean - Rent[i:i+12])
    out_of_sample <- data.frame(out_of_sample, as.numeric(to_save_2))
}
Error <- out_of_sample[,-1]
Error_mean_by_time <- rowMeans(Error, na.rm = TRUE)
Squared <- Error_mean_by_time^2
rm(end, end_month, end_year, fore, i, temporary, to_save, to_save_2, Error_mean_by_time)


### out of sample forecast for our model

out_of_sample_b <- data.frame(matrix(ncol = 1, nrow = 12))
mean_of_fit_b <- data.frame(matrix(ncol = 1, nrow = nrow(CPIs)))
end_b <- nrow(CPIs)
end_b <- end_b - 12
# Line types
#iterate from line 36 to the en of CPIs
for (i in 30:end_b+1){
    temporary <- Rent[1:i-1]
    temporary <- ts(temporary, start = c(2009,3), frequency = 4)
    end_year <- end(temporary)[1]
    end_month <- end(temporary)[2]
    #fit arima model on the first i-1 observations
    fit <- arima(temporary, order = c(1,1,0))
    fore <- forecast(fit, h = 12)
    #save the mean of the fit
    to_save <- c(temporary, fore$mean, rep(NA, end_b - i + 1))
    mean_of_fit_b <- data.frame(mean_of_fit_b, to_save)
    #save the error
    to_save_2 <- (fore$mean - Rent[i:i+12])
    out_of_sample_b <- data.frame(out_of_sample_b, as.numeric(to_save_2))
}
Error_b <- out_of_sample_b[,-1]
Error_mean_by_time_b <- rowMeans(Error_b, na.rm = TRUE)
Squared_b <- Error_mean_by_time_b^2

rm(end_b, end_month, end_year, fore, i, temporary, to_save, to_save_2, Error_mean_by_time_b)



MSFE_pred_by_time <- 1 - (Squared/Squared_b)


pdf(paste(getwd(), "/Graphs/double minus/predictive_r_double_minus.pdf", sep=""), width = 13, height = 5)

barplot(MSFE_pred_by_time,names.arg = 1:12,main = "Predictive R_Squared by period" )

dev.off()


## plot spaghetti graph

temp <- Rent[1:length(Rent)]
cpi_ohne_diff <- log(temp /lag(temp ,4))
cpi_ohne_diff <- cpi_ohne_diff[13:length(cpi_ohne_diff)]
cpi_ohne_diff <- ts(cpi_ohne_diff, start = c(2009,4), frequency = 4)
pdf(paste(getwd(), "/Graphs/double minus/spag.pdf", sep=""))
dev.off()
plot(cpi_ohne_diff, type = "l", col = "red", xlab = "Year", ylab = "Inflation", main = "Spaghetti graph CPIs YoY without rent and without petroleum products")
legend("topleft",           # Position of the legend
       legend = c("ARIMA(3,0,0)", "ARIMA(1,0,0)"),  # Legend labels
       col = c("Blue", "Green"),       # Colors
       lty = 1)

#remove first column of mean_of_fit
mean_of_fit <- mean_of_fit[,-1]
mean_of_fit_b <- mean_of_fit_b[,-1]

for (i in seq(from = 1, to = 19, by = 3)){
        print <- mean_of_fit[,i]
        print <- log(print /lag(print ,4))
        print <- print[13:length(print)]
        print <- ts(print, start = c(2009,4), frequency = 4)
        print <- tail(print, 31 - i  + 1)
        lines(print, col="blue")

        print <- mean_of_fit_b[,i]
        print <- log(print /lag(print ,4))
        print <- print[13:length(print)]
        print <- ts(print, start = c(2009,4), frequency = 4)
        print <- tail(print, 31 - i  + 1)
        lines(print, col="green")
}
































### out of sample forecast for benchmark model

the_Rent <- ts(Rent, start = c(2009,9), frequency = 4)
the_Mort <- ts(Mortgage, start = c(2009,9), frequency = 4)
out_of_sample <- data.frame(matrix(ncol = 1, nrow = 12))
mean_of_fit <- data.frame(matrix(ncol = 1, nrow = 12))
end <- length(the_Rent)
end <- end - 12
pdf(paste(getwd(), "/Graphs/double minus/rent_spag.pdf", sep=""))
plot(the_Rent, type = "l", col = "red", xlab = "Year", ylab = "Inflation", main = "Spaghetti graph Rent YoY")

legend("bottomright",           # Position of the legend
       legend = c("ARMAX", "ARIMA(1,0,0)"),  # Legend labels
       col = c("Blue", "Green"),       # Colors
       lty = 1)                      # Line types
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
    print <- temporary[i-1]
    print <- c(print, fore$pred)
    print <- as.data.frame(print)
    print <- ts(print, start = c(end_year, end_month), frequency = 4)
    lines(print, col="blue")
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
    print <- temporary[i-1]
    print <- c(print, fore$pred)
    print <- as.data.frame(print)
    print <- ts(print, start = c(end_year, end_month), frequency = 4)
    lines(print, col="green")
    #calculate the out of sample forecast
    to_save_b <- (as.numeric(fore$pred) - the_Mort[i:i+12])^2
    out_of_sample_b <- data.frame(out_of_sample_b, to_save_b)
    mean_of_fit_b <- data.frame(mean_of_fit_b, fore$pred)

}

dev.off()
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


pdf(paste(getwd(), "/Graphs/double minus/predictive_rent.pdf", sep=""), width = 13, height = 5)


barplot(MSFE_pred_by_time,names.arg = 1:36,main = "Predictive R_Squared by period ARMAX" )


dev.off()
