
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
#
######
# clean space
######

rm(list = ls())
# Get the current working directory
current_directory <- getwd()

######
# DATA importation
######

rm(list = ls())
#load data : CPIs.RData
load("CPIs_double_minus.RData")


######
# Create Rrent CPI and Mortgage time series
######

Rent <- ts(CPIs$Housing.rental.1, start = c(2000,1), frequency = 12)
Rent <- aggregate(Rent,4,mean)
plot(Rent, type = "l", col = "red", xlab = "Year", ylab = "Rent Inflation", main = "CPIs Rent YoY")

#import excel file with mortgage data
Mortgage <- read_excel("mortgage_rate_c.xlsx", sheet = 1, col_names = TRUE)
#invert the order of the rows
Mortgage <- Mortgage[rev(row.names(Mortgage)),]
#keep firt column only
Mortgage <- Mortgage[,1]
#change name of column
colnames(Mortgage) <- c("Mortgage")
#convert to vector
Mortgage <- as.numeric(Mortgage$Mortgage)
#duplicate each value 3 times to have monthly frequency
Mortgage2 <- rep(Mortgage, each = 3)
#remove first value
Mortgage2 <- Mortgage2[-1]
#remove 2 last values
Mortgage2 <- Mortgage2[-c(length(Mortgage2), length(Mortgage2)-1)]
Mortgage2 <- ts(Mortgage2, start = c(2008,10), frequency = 12)
Mortgage <- aggregate(Mortgage2,4,mean)
plot(Mortgage, type = "l", col = "red", xlab = "Year", ylab = "Mortgage Inflation", main = "CPIs Mortgage YoY")

Rent <- tail(Rent, length(Mortgage))
length(Rent)
length(Mortgage)

#plot rent with line mortgage with y absice on te right for mortgage rate


plot(Rent)
lines(Mortgage)


######
# Fitting ARIMA model
######

#auto arima fit using mortgage as exogenous variable
fit_X <- auto.arima(Rent, stepwise=FALSE, seasonal = FALSE, approximation = FALSE, trace=TRUE, xreg = Mortgage)
fit_M <- auto.arima(Mortgage, stepwise=FALSE, seasonal = FALSE, approximation = FALSE, trace=TRUE)
#set the specification
spec_rent <- c(2,2,0)
spec_mortgage <- c(2,2,2)

######
# Manual Fitting ARIMA model
######
#fit arima model
fit_X <- arima(Rent, order = spec_rent, xreg = Mortgage)
#fit arima model on mortgage
fit_M <- arima(Mortgage, order = spec_mortgage)
# create a model to comply with the forecast function
Model_M <- Arima(Rent, model=fit_X, xreg=Mortgage)
#forecast mortgage
Mortgage2 <- forecast(fit_M, h = 12)
Mortgage2 <- as.numeric(Mortgage2$mean)
#forecast rent
Forecast_rent <- forecast(Model_M, xreg = Mortgage2)
#plot forecast
plot(Forecast_rent)

# store for future aggregation
serie <- c(Rent, Forecast_rent$mean)

rm(Mortgage2, Forecast_rent, Model_M, fit_M)
######
# check stationnarity manually
######

#compute the difference
Rent_diff <- tail(diff(Rent,difference = 2),(length(Rent)-1))
plot(Rent_diff, type = "l", col = "red", xlab = "Year", ylab = "Inflation", main = "CPIs YoY without rent and without petroleum products")

# check with ADF
adf.test(Rent, alternative = "stationary")

# check with KPSS
kpss.test(Rent, null = "T", lshort = TRUE)


rm(Rent_diff)
######
# Model quality test
######

#calculate the in sample residuals
in_sample_residuals <- fit_X$residuals
#calculate the in sample RMSE
in_sample_RMSE <- sqrt(mean(in_sample_residuals^2))
#calculate the in sample MAE
in_sample_MAE <- mean(abs(in_sample_residuals))

base_stat <- data.frame(in_sample_RMSE, in_sample_MAE)
rm(in_sample_MAE, in_sample_RMSE)

# Ljung Box-Q Test
Ljung <- Box.test(in_sample_residuals, lag = 12, type = "Ljung-Box", fitdf = 3)
# White Test
Pierce <- Box.test(in_sample_residuals, lag = 12, type = "Box-Pierce", fitdf = 3)
# jarque bera test
Jarques <- jarque.bera.test(in_sample_residuals)
# White Test
White <- white_test(fit_X)

in_sample_tests <- data.frame(Ljung$p.value, White$p_value, Jarques$p.value,Pierce$p.value)
rm(Ljung, Pierce, Jarques, White)



######
# Out of sample tests comparative
######

### out of sample forecast for our model

#create matrix to store the datas
out_of_sample <- data.frame(matrix(ncol = 1, nrow = 12))
mean_of_fit <- data.frame(matrix(ncol = 1, nrow = length(Rent)))
end <- length(Rent)
end <- end - 12

#forecast from line 21 to the end of date
for (i in 20:(end +1)){
    #store the truncated serie for fitting
    temporary <- Rent[1:(i-1)]
    temporary <- ts(temporary, start = c(2008,4), frequency = 4)
    temporary_m <- Mortgage[1:(i-1)]
    temporary_m <- ts(temporary_m, start = c(2008,4), frequency = 4)
    #fit arimaX model on the first i-1 observations
    fit <- arima(temporary, order = spec_rent, xreg = temporary_m)
    model <- Arima(temporary, model=fit, xreg=temporary_m)
    fit_m <- arima(temporary_m, order = spec_mortgage)
    #forecast mortgage
    fore_m <- forecast(fit_m, h = 12)
    #forecast the i-th observation
    fore <- forecast(model, xreg = fore_m$mean, h = 12)
    #save the mean of the fit
    to_save <- c(temporary, fore$mean, rep(NA, end - i + 1))
    mean_of_fit <- data.frame(mean_of_fit, to_save)
    #save the error
    to_save_2 <- fore$mean - as.numeric(Rent[(i):(i+11)])
    out_of_sample <- data.frame(out_of_sample, as.numeric(to_save_2))
}
## remove place holder columns
mean_of_fit <- mean_of_fit[,-1]
Error <- out_of_sample[,-1]

#Prepare series for predictive rsquared and Diebold Mariano test
Error_ag <- rowMeans(Error, na.rm = TRUE)
Error_sq <- rowMeans(Error^2, na.rm = TRUE)
rm(end, fore, fit,  i, temporary, temporary_m, to_save, to_save_2, out_of_sample, fit_m, fore_m)


### out of sample forecast for benchmark model

#create matrix to store the datas
out_of_sample_b <- data.frame(matrix(ncol = 1, nrow = 12))
mean_of_fit_b <- data.frame(matrix(ncol = 1, nrow = length(Rent)))
end_b <-length(Rent)
end_b <- end_b - 12


#forecast from line 21 to the end of date benchmark model
for (i in 20:(end_b+1)){
    #store the truncated serie for fitting
    temporary <- Rent[1:i-1]
    temporary <- ts(temporary, start = c(2008,4), frequency = 4)
    #fit arima model on the first i-1 observations
    fit <- arima(temporary, order = c(1,2,0))
    fore <- forecast(fit, h = 12)
    #save the mean of the fit
    to_save <- c(temporary, fore$mean, rep(NA,( end_b - i + 1)))
    mean_of_fit_b <- data.frame(mean_of_fit_b, to_save)
    #save the error
    to_save_2 <- as.numeric(fore$mean)  - as.numeric(Rent[(i):(i+11)])
    out_of_sample_b <- data.frame(out_of_sample_b, as.numeric(to_save_2))
}

## remove place holder columns
mean_of_fit_b <- mean_of_fit_b[,-1]
Error_b <- out_of_sample_b[,-1]

#Prepare series for predictive rsquared and Diebold Mariano test
Error_b_ag <- rowMeans(Error_b, na.rm = TRUE)
Error_b_sq <- rowMeans(Error_b^2, na.rm = TRUE)

rm(end_b, fore, fit,  i, temporary, to_save, to_save_2, out_of_sample_b)


######
# Predictive R squared
######

#calculate the predictive R squared
MSFE_pred_by_time <- 1 - (Error_sq/Error_b_sq)


pdf(paste(getwd(), "/Graphs/double minus/predictive_r_double_minus.pdf", sep=""), width = 13, height = 5)
barplot(MSFE_pred_by_time,names.arg = 1:12,main = "Predictive R_Squared by period" )

dev.off()


######
# Plot spaghetti graph
######

#create the YoY series
temp <- Rent[1:length(Rent)]
Rent_Y <- (temp / lag(temp, 4) - 1) * 100
Rent_Y <- Rent_Y[5:length(Rent_Y)]
Rent_Y <- ts(Rent_Y, start = c(2009,4), frequency = 4)

pdf(paste(getwd(), "/Graphs/double minus/spag.pdf", sep=""))
dev.off()
plot(Rent_Y, type = "l", col = "red", xlab = "Year", ylab = "Inflation", main = "Spaghetti graph CPIs YoY without rent and without petroleum products")
legend("topleft",           # Position of the legend
       legend = c("ARIMA(3,0,0)", "ARIMA(1,0,0)"),  # Legend labels
       col = c("Blue", "Green"),       # Colors
       lty = 1)

#plot the out of sample forecast
for (i in seq(from = 1, to = 30, by = 3)){
        #keep the i'th column of mean_of_fit
        print <- mean_of_fit[,i]
        #calculate the YoY
        print <- (print / lag(print ,4) - 1) * 100

        print[1:(19+i-2)] <- NA
        #create a time series
        print <- ts(print, start = c(2008,4), frequency = 4)
        lines(print, col="blue")


        #same for benchmark model
        #uncomment to plot it
        print <- mean_of_fit_b[,i]
        #calculate the YoY
        print <- (print / lag(print ,4) - 1) * 100
        print[1:(19+i -2)] <- NA
        #create a time series
        print <- ts(print, start = c(2008,4), frequency = 4)
        lines(print, col="green")

}


#####
# Out of sample tests Diebold
######

Diebold_DM<- c()
Diebold_p<- c()

#calculate the Diebold Mariano test for each period
for(i in 1:12){
    Diebold_DM[i] <- dm.test(Error_ag, Error_b_ag, alternative = "two.sided", h = i, power = 2,varestimator = "bartlett")$statistic
    Diebold_p[i] <- dm.test(Error_ag, Error_b_ag, alternative = "two.sided", h = i, power = 2,varestimator = "bartlett")$p.value
}

barplot(Diebold_DM,names.arg = 1:12,main = "Diebold Mariano test by period" )
barplot(Diebold_p,names.arg = 1:12,main = "Diebold Mariano test by period" )


#####
# save ARIMA
#####

arimaX_error <- Error
#Las column of mean_of_fit
arimaX_forecast <- serie
arimaX_out <- mean_of_fit

#save
save(arimaX_error, arimaX_forecast, arimaX_out , file = "arimaX_forecast.RData")