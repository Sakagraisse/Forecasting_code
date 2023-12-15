
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
# DATA import
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

#auto arima fit CPIs$Inflation.withoutRI
fit <- auto.arima(cpi_ohne, seasonal = TRUE, approximation = FALSE, trace=TRUE, stepwise=FALSE)
#check the residuals
fore <- forecast(fit, h = 36)



checkresiduals(fit)
#store specification and serie
spec <- c(4,1,1)
#fit <- arima(cpi_ohne, order = spec)
fore <- forecast(fit, h = 36)
plot(fore)
serie <- c(cpi_ohne, fore$mean)
plot(serie)

#check stationnarity manually
cpi_ohne_diff <- diff(cpi_ohne)[2:length(cpi_ohne)-1]
plot(cpi_ohne_diff, type = "l", col = "red", xlab = "Year", ylab = "Inflation", main = "CPIs YoY without rent and without petroleum products")
#check for d with dickey fuller test
adf.test(cpi_ohne_diff, alternative = "stationary")
## is stqtionnary
# check with KPSS
kpss.test(cpi_ohne_diff, null = "Trend", lshort = TRUE)
## is stationnary

rm(cpi_ohne_diff, fore  )

######
# 3 model quality test
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
Ljung <- Box.test(in_sample_residuals, type = "Ljung-Box", lag = 8, fitdf = 5)
# White Test
Pierce <- Box.test(in_sample_residuals, lag = 10, type = "Box-Pierce", fitdf = 5)
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
for (i in 151:(end+1)){
    temporary <- cpi_ohne[1:(i-1)]
    temporary <- ts(temporary, start = c(2000,1), frequency = 12)
    #fit arima model on the first i-1 observations
    fit <- arima(temporary, order = spec)
    fore <- forecast(fit, h = 36)
    #save the mean of the fit
    #convert monthly to quarterly
    to_save <- c(temporary, fore$mean, rep(NA, (end - i + 1)))
    mean_of_fit <- data.frame(mean_of_fit, to_save)
    #save the error
    to_save_2 <- as.numeric(fore$mean) -  as.numeric(cpi_ohne[(i):(i+35)])
    out_of_sample <- data.frame(out_of_sample, as.numeric(to_save_2))
}
mean_of_fit <- mean_of_fit[,-1]
# and first column
Error <- out_of_sample[,-1]
#remove last line
Error_ag <- rowMeans(Error, na.rm = TRUE)
Error_sq <- rowMeans(Error^2, na.rm = TRUE)


rm(end, fore, fit,  i, temporary, to_save, to_save_2, out_of_sample)


### out of sample forecast for our model

out_of_sample_b <- data.frame(matrix(ncol = 1, nrow = 36))
mean_of_fit_b <- data.frame(matrix(ncol = 1, nrow = nrow(CPIs)))
end_b <- nrow(CPIs)
end_b <- end_b - 36
# Line types
#iterate from line 36 to the en of CPIs
for (i in 151:(end_b+1)){
    temporary <- cpi_ohne[1:(i-1)]
    temporary <- ts(temporary, start = c(2000,1), frequency = 12)
    #fit arima model on the first i-1 observations
    fit <- arima(temporary, order = c(1,1,0))
    fore <- forecast(fit, h = 36)
    #save the mean of the fit
    to_save <- c(temporary, fore$mean, rep(NA, (end_b - i + 1)))
    mean_of_fit_b <- data.frame(mean_of_fit_b, to_save)
    #save the error
    to_save_2 <- as.numeric(fore$mean) -  as.numeric(cpi_ohne[(i):(i+35)])
    out_of_sample_b <- data.frame(out_of_sample_b, as.numeric(to_save_2))
}
mean_of_fit_b <- mean_of_fit_b[,-1]
Error_b <- out_of_sample_b[,-1]
Error_b_ag <- rowMeans(Error_b, na.rm = TRUE)
Error_b_sq <- rowMeans(Error_b^2, na.rm = TRUE)


rm(end_b, fit, fore, i, temporary, to_save, to_save_2, out_of_sample_b)


######
# 4 model quality test
######
MSFE_pred_by_time <- 1 - (Error_sq/Error_b_sq)
pdf(paste(getwd(), "/Graphs/double minus/predictive_r_double_minus.pdf", sep=""), width = 13, height = 5)
#keep one value each 3 values
MSFE_pred_by_time <- MSFE_pred_by_time[seq(1, length(MSFE_pred_by_time), 3)]
barplot(MSFE_pred_by_time,names.arg = 1:12,main = "Predictive R_Squared by period" )

dev.off()


## plot spaghetti graph

temp <- cpi_ohne[1:(length(cpi_ohne))]
cpi_without_approx <- (temp / lag(temp, 12) - 1) * 100
cpi_without_approx <- cpi_without_approx[13:length(cpi_without_approx)]
cpi_without_approx <- ts(cpi_without_approx, start = c(2001,1), frequency = 12)
cpi_without_approx <- aggregate(cpi_without_approx, nfrequency = 4, FUN = mean)

pdf(paste(getwd(), "/Graphs/double minus/spag.pdf", sep=""))
dev.off()


plot(cpi_without_approx , type = "l", col = "red", xlab = "Year", ylab = "Inflation", main = "Spaghetti graph CPIs YoY without rent and without petroleum products")
legend("topleft",           # Position of the legend
       legend = c("ARIMA(3,0,0)", "ARIMA(1,0,0)"),  # Legend labels
       col = c("Blue", "Green"),       # Colors
       lty = 1)

# "to" needs to be the leght of the series
for (i in seq(from = 1, to = 100, by = 6)){

        print <- mean_of_fit[,i]
        print <- (print / lag(print ,12) - 1) * 100
        print[1:(150+i -4)] <- NA
        print <- print[13:length(print)]
        print <- ts(print, start = c(2001,1), frequency = 12)
        #replace the first 150 values by NA
        print <- aggregate(print, nfrequency = 4, FUN = mean)
        lines(print, col="blue")


        print <- mean_of_fit_b[,i]
        print <- (print / lag(print ,12) - 1) * 100
        print[1:(150+i -4)] <- NA
        print <- print[13:length(print)]
        print <- ts(print, start = c(2001,1), frequency = 12)
        #replace the first 150 values by NA
        print <- aggregate(print, nfrequency = 4, FUN = mean)
        #lines(print, col="green")
}


#####
# Out of sample tests Diebold
######
Diebold_DM<- c()
Diebold_p<- c()
for(i in 1:36){
    Diebold_DM[i] <- dm.test(Error_ag, Error_b_ag, alternative = "two.sided", h = i, power = 2,varestimator = "bartlett")$statistic
    Diebold_p[i] <- dm.test(Error_ag, Error_b_ag, alternative = "two.sided", h = i, power = 2,varestimator = "bartlett")$p.value
}

barplot(Diebold_DM,names.arg = 1:36,main = "Diebold Mariano test by period" )
barplot(Diebold_p,names.arg = 1:36,main = "Diebold Mariano test by period" )


#####
# save ARIMA
#####

arima_error <- Error
#Las column of mean_of_fit
arima_forecast <- serie
arima_out <- mean_of_fit

#save
save(arima_error, arima_forecast, arima_out , file = "arima_forecast.RData")


plot(arima_forecast)
AAAAA <- arima_out[,1]
AAAAA <- AAAAA[1:151]
AAAAA <- ts(AAAAA, start = c(2000,1), frequency = 12)