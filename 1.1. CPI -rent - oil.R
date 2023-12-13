
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
if(!require(whitestrap)) install.packages("whitestrap")
if(!require(lmtest)) install.packages("lmtest")
library(lmtest)

library(whitestrap)
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

#clean space
rm(list = ls())
# Get the current working directory
current_directory <- getwd()

######
# DATA import and tranformation
######

#load data : CPIs.RData
load("CPIs_double_minus.RData")


rm(dataw, monthly_data )

######
# 2 Fitting ARIMA model
######
#plot
cpi_ohne <- ts(CPIs$our, start = c(2000,1), frequency = 12)
plot(cpi_ohne, type = "l", col = "red", xlab = "Year", ylab = "Index", main = "CPIs without rent and without petroleum products")
cpi_total <- ts(CPIs$Total, start = c(2000,1), frequency = 12)
lines(cpi_total, col = "blue")
rm(cpi_total)
#plot(CPIs$Year, CPIs$Inf_Total, type = "l", col = "red", xlab = "Year", ylab = "Index", main = "CPIs without rent and without petroleum products")

#auto arima fit CPIs$Inflation.withoutRI
fit <- auto.arima(cpi_ohne, seasonal = FALSE, approximation = FALSE, trace=TRUE, stepwise=FALSE)
#check the residuals
checkresiduals(fit)
#plot the forecast
tosee <- forecast(fit, h = 36)
plot(tosee)


#check stationnarity manually
cpi_ohne_diff <- diff(cpi_ohne)[2:length(cpi_ohne)-1]
plot(cpi_ohne_diff, type = "l", col = "red", xlab = "Year", ylab = "Inflation", main = "CPIs YoY without rent and without petroleum products")
#check for d with dickey fuller test
adf.test(cpi_ohne_diff, alternative = "stationary", k = 1)
## is stqtionnary
# check with KPSS
kpss.test(cpi_ohne_diff, null = "Trend", lshort = TRUE)
## is stationnary

rm(cpi_ohne_diff, tosee)
######
# 3 model quality tes
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
rm(Ljung, Pierce, Jarques, White, in_sample_residuals, fit)



######
# Out of sample tests comparative
######

### out of sample forecast for our model

out_of_sample <- data.frame(matrix(ncol = 1, nrow = 36))
mean_of_fit <- data.frame(matrix(ncol = 1, nrow = nrow(CPIs)))
end <- nrow(CPIs)
end <- end - 36
# Line types
#iterate from line 36 to the en of CPIs
for (i in 150:end+1){
    temporary <- cpi_ohne[1:i-1]
    temporary <- ts(temporary, start = c(2000,1), frequency = 12)
    end_year <- end(temporary)[1]
    end_month <- end(temporary)[2]
    #fit arima model on the first i-1 observations
    fit <- arima(temporary, order = c(3,1,0))
    fore <- forecast(fit, h = 36)
    #save the mean of the fit
    to_save <- c(temporary, fore$mean, rep(NA, end - i + 1))
    mean_of_fit <- data.frame(mean_of_fit, to_save)
    #save the error
    to_save_2 <- (fore$mean - cpi_ohne[i:i+36])
    out_of_sample <- data.frame(out_of_sample, as.numeric(to_save_2))
}
Error <- out_of_sample[,-1]
Error_mean_by_time <- rowMeans(Error, na.rm = TRUE)
Squared <- Error_mean_by_time^2
rm(end, end_month, end_year, fore, i, temporary, to_save, to_save_2, Error_mean_by_time)


### out of sample forecast for our model

out_of_sample_b <- data.frame(matrix(ncol = 1, nrow = 36))
mean_of_fit_b <- data.frame(matrix(ncol = 1, nrow = nrow(CPIs)))
end_b <- nrow(CPIs)
end_b <- end_b - 36
# Line types
#iterate from line 36 to the en of CPIs
for (i in 150:end_b+1){
    temporary <- cpi_ohne[1:i-1]
    temporary <- ts(temporary, start = c(2000,1), frequency = 12)
    end_year <- end(temporary)[1]
    end_month <- end(temporary)[2]
    #fit arima model on the first i-1 observations
    fit <- arima(temporary, order = c(1,1,0))
    fore <- forecast(fit, h = 36)
    #save the mean of the fit
    to_save <- c(temporary, fore$mean, rep(NA, end_b - i + 1))
    mean_of_fit_b <- data.frame(mean_of_fit_b, to_save)
    #save the error
    to_save_2 <- (fore$mean - cpi_ohne[i:i+36])
    out_of_sample_b <- data.frame(out_of_sample_b, as.numeric(to_save_2))
}
Error_b <- out_of_sample_b[,-1]
Error_mean_by_time_b <- rowMeans(Error_b, na.rm = TRUE)
Squared_b <- Error_mean_by_time_b^2

rm(end_b, end_month, end_year, fore, i, temporary, to_save, to_save_2, Error_mean_by_time_b)



MSFE_pred_by_time <- 1 - (Squared/Squared_b)


pdf(paste(getwd(), "/Graphs/double minus/predictive_r_double_minus.pdf", sep=""), width = 13, height = 5)

barplot(MSFE_pred_by_time,names.arg = 1:36,main = "Predictive R_Squared by period" )

dev.off()


## plot spaghetti graph

temp <- cpi_ohne[1:length(cpi_ohne)]
cpi_ohne_diff <- log(temp /lag(temp ,12))
cpi_ohne_diff <- cpi_ohne_diff[13:length(cpi_ohne_diff)]
cpi_ohne_diff <- ts(cpi_ohne_diff, start = c(2001,1), frequency = 12)
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

for (i in seq(from = 1, to = 100, by = 20)){
        print <- mean_of_fit[,i]
        print <- log(print /lag(print ,12))
        print <- print[13:length(print)]
        print <- ts(print, start = c(2001,1), frequency = 12)
        print <- tail(print, 136 - i  + 1)
        lines(print, col="blue")

        print <- mean_of_fit_b[,i]
        print <- log(print /lag(print ,12))
        print <- print[13:length(print)]
        print <- ts(print, start = c(2001,1), frequency = 12)
        print <- tail(print, 136 - i  + 1)
        lines(print, col="green")
}



#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww



######
# Out of sample tests Diebold
######
Diebold <- dm.test(Squared , Squared_b, alternative = "two.sided", h = 36, power = 1,varestimator = "bartlett")
Diebold <- dm.test(Squared[1] , Squared_b[1], alternative = "two.sided", h = 1, power = 1,varestimator = "bartlett")


dm.test(Squared , Squared_b, alternative = "two.sided", h = 36, power = 1,varestimator = "bartlett")



#Dieblod Mariano test
num_rows <- nrow(MSFE_by_time)
num_cols <- ncol(MSFE_by_time_b)
Diebold <- as.data.frame(matrix(ncol = num_cols, nrow = num_rows))
names(Diebold) <- names(MSFE_by_time)

for(u in 2:num_cols){
    for(t in 2:num_rows){
        Diebold[t,u] <- dm.test(MSFE_by_time[1:t,u] , MSFE_by_time_b[1:t,u], alternative = "two.sided", h = t, power = 2,varestimator = "bartlett")
    }
}

rm(num_cols, num_rows, t, u)


######
# True inflation
######




