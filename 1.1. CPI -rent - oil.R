
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
if(!require(xtable)) install.packages("xtable")
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}
library(car)
library(xtable)
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


######
# clean space
######

rm(list = ls())
# Get the current working directory
current_directory <- getwd()

######
# DATA importation
######

#load data : CPIs.RData
load("CPIs_double_minus.RData")

######
# Check Auto Fitting ARIMA model
######
#extract the serie of interest
cpi_ohne <- ts(CPIs$our, start = c(2000,1), frequency = 12)
plot(cpi_ohne, type = "l", col = "red", xlab = "Year", ylab = "Index", main = "CPIs without rent and without petroleum products")


#auto arima fit CPI - oi - rent
fit <- auto.arima(cpi_ohne, seasonal = FALSE, approximation = FALSE, trace=TRUE, stepwise=FALSE)
#check residuals
checkresiduals(fit)
# forecast using the model
fore <- forecast(fit, h = 36)
#plot the forecast
plot(fore)



# Utilisation de auto.arima avec des options spécifiques
fit <- auto.arima(cpi_ohne, max.P=10, max.Q=10)

# fit contiendra le meilleur modèle selon les critères AIC et BIC


#set specification
spec <- c(4,1,1)

######
# Manual Fitting ARIMA model
######
fit <- arima(cpi_ohne, order = spec)
checkresiduals(fit)
fore <- forecast(fit, h = 36)
plot(fore)

# store for future aggregation
serie <- c(cpi_ohne, fore$mean)
plot(serie)



# store for future aggregation
serie <- c(cpi_ohne, fore$mean)
plot(serie)


######
# check stationnarity manually
######

#compute the difference
cpi_ohne_diff <- tail(diff(cpi_ohne),(length(cpi_ohne)-1))
plot(cpi_ohne_diff, type = "l", col = "red", xlab = "Year", ylab = "Inflation", main = "CPIs YoY without rent and without petroleum products")

# check with ADF
adf.test(cpi_ohne_diff, alternative = "stationary")

# check with KPSS
kpss.test(cpi_ohne_diff, null = "T", lshort = TRUE)


rm(cpi_ohne_diff, fore  )

######
# Model quality test
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
Ljung <- Box.test(in_sample_residuals, type = "Ljung-Box")
# White Test
Pierce <- Box.test(in_sample_residuals, lag = 12, type = "Box-Pierce", fitdf = 5)
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

#create matrix to store the datas
out_of_sample <- data.frame(matrix(ncol = 1, nrow = 36))
mean_of_fit <- data.frame(matrix(ncol = 1, nrow = nrow(CPIs)))
end <- nrow(CPIs)
end <- end - 36

#forecast from line 151 to the end of CPIs
for (i in 151:(end+1)){
    #store the truncaed serie for fitting
    temporary <- cpi_ohne[1:(i-1)]
    temporary <- ts(temporary, start = c(2000,1), frequency = 12)
    #fit arima model on the first i-1 observations
    fit <- arima(temporary, order = spec)
    fore <- forecast(fit, h = 36)
    #save the mean of the fit
    to_save <- c(temporary, fore$mean, rep(NA, (end - i + 1)))
    mean_of_fit <- data.frame(mean_of_fit, to_save)
    #save the error
    to_save_2 <- as.numeric(fore$mean) -  as.numeric(cpi_ohne[(i):(i+35)])
    out_of_sample <- data.frame(out_of_sample, as.numeric(to_save_2))
}

## remove place holder columns
mean_of_fit <- mean_of_fit[,-1]
Error <- out_of_sample[,-1]

#Prepare series for predictive rsquared and Diebold Mariano test
Error_ag <- rowMeans(Error, na.rm = TRUE)
Error_sq <- rowMeans(Error^2, na.rm = TRUE)


rm(end, fore, fit,  i, temporary, to_save, to_save_2, out_of_sample)


### out of sample forecast for benchmark model

#create matrix to store the datas
out_of_sample_b <- data.frame(matrix(ncol = 1, nrow = 36))
mean_of_fit_b <- data.frame(matrix(ncol = 1, nrow = nrow(CPIs)))
end_b <- nrow(CPIs)
end_b <- end_b - 36

#forecast from line 151 to the end of CPIs benchmark model
for (i in 151:(end_b+1)){
    #store the truncated serie for fitting
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

## remove place holder columns
mean_of_fit_b <- mean_of_fit_b[,-1]
Error_b <- out_of_sample_b[,-1]

#Prepare series for predictive rsquared and Diebold Mariano test
Error_b_ag <- rowMeans(Error_b, na.rm = TRUE)
Error_b_sq <- rowMeans(Error_b^2, na.rm = TRUE)

rm(end_b, fit, fore, i, temporary, to_save, to_save_2, out_of_sample_b)

######
# Predictive R squared
######

#calculate the predictive R squared
MSFE_pred_by_time <- 1 - (Error_sq/Error_b_sq)

pdf(paste(getwd(), "/Graphs/double minus/predictive_r_double_minus.pdf", sep=""), width = 13, height = 5)
# plot it for each quarter
MSFE_pred_by_time <- MSFE_pred_by_time[seq(1, length(MSFE_pred_by_time), 3)] # first or last month ?
barplot(MSFE_pred_by_time,names.arg = 1:12,main = "Predictive R_Squared by period CPI without oil and rent" )

dev.off()


######
# Plot spaghetti graph
######

#create the YoY series
temp <- cpi_ohne[1:(length(cpi_ohne))]
cpi_without_approx <- (temp / lag(temp, 12) - 1) * 100
cpi_without_approx <- cpi_without_approx[13:length(cpi_without_approx)]
cpi_without_approx <- ts(cpi_without_approx, start = c(2001,1), frequency = 12)
cpi_without_approx <- aggregate(cpi_without_approx, nfrequency = 4, FUN = mean)

pdf(paste(getwd(), "/Graphs/double minus/spag_double_minus.pdf", sep=""), width = 13, height = 5)


#plot the series
plot(cpi_without_approx , type = "l", col = "blue", xlab = "Year", ylab = "Inflation", main = "Spaghetti graph CPIs YoY without rent and without petroleum products (1 over 12")
abline(h = mean(cpi_without_approx, na.rm = TRUE), col = "Black")
legend("topleft",           # Position of the legend
       legend = c("Observed", "Out-of-Sample Forecast"),  # Legend labels
       col = c("Blue", "Red"),       # Colors
       lty = 1)

#plot the out of sample forecast
#from : choose the first month starting for a quarter in the out of sample forecast
#to : size of the out of sample forecast (columns of mean_of_fit)
#by : step of multiple of 6 to get a full quarter each time
for (i in seq(from = 1, to = 100, by = 12)){
        #keep the i'th column of mean_of_fit
        print <- mean_of_fit[,i]
        #calculate the YoY
        print <- (print / lag(print ,12) - 1) * 100
        #remove the first 150 values from displaying
        print[1:(150+i -4)] <- NA
        print <- ts(print, start = c(2000,1), frequency = 12)
        # aggregate to quarterly
        print <- aggregate(print, nfrequency = 4, FUN = mean)
        #plot
        lines(print, col="red")

        #same for benchmark model
        #uncomment to plot it
        #print <- mean_of_fit_b[,i]
        #print <- (print / lag(print ,12) - 1) * 100
        #print[1:(150+i -4)] <- NA

        #print <- ts(print, start = c(2000,1), frequency = 12)
        #print <- aggregate(print, nfrequency = 4, FUN = mean)
        #lines(print, col="green")
}
dev.off()

#####
# Out of sample tests Diebold
######

Diebold_DM<- c()
Diebold_p<- c()

#calculate the Diebold Mariano test for each period
for(i in 1:36){
    Diebold_DM[i] <- dm.test(Error_ag, Error_b_ag, alternative = "two.sided", h = i, power = 2,varestimator = "bartlett")$statistic
    Diebold_p[i] <- dm.test(Error_ag, Error_b_ag, alternative = "two.sided", h = i, power = 2,varestimator = "bartlett")$p.value
}

#plot the results
# keep only the first month of each quarter
Diebold_DM <- Diebold_DM[seq(1, length(Diebold_DM), 3)] # first or last month ?
barplot(Diebold_DM,names.arg = 1:12,main = "Diebold Mariano test by period" )
Diebold_p <- Diebold_p[seq(1, length(Diebold_p), 3)]
barplot(Diebold_p,names.arg = 1:12,main = "Diebold Mariano test by period" )

diebold_table <- data.frame(seq(1,12,1),Diebold_DM, Diebold_p)
colnames(diebold_table) <- c("Period", "Diebold Mariano", "p-value")
latex_table <- xtable(diebold_table)
print(latex_table, type = "latex", floating = FALSE, file = (paste(getwd(), "/Graphs/double minus/diebold_ohne.txt", sep="")))


#####
# save ARIMA
#####

arima_error <- Error
#Last column of mean_of_fit
arima_forecast <- serie
arima_out <- mean_of_fit

#save
save(arima_error, arima_forecast, arima_out , file = "arima_forecast.RData")


#####
# plot forecast
#####

pdf(paste(getwd(), "/Graphs/double minus/forecast_double_minus.pdf", sep=""), width = 10, height = 5)
to_plot <- (arima_forecast / lag(arima_forecast, 12) - 1) * 100
to_plot <- ts(to_plot, start = c(2000,1), frequency = 12)
to_plot <- aggregate(to_plot, nfrequency = 4, FUN = mean)
to_plot_2 <- tail(to_plot,13)
to_plot[(length(to_plot)-11):length(to_plot)] <- NA
plot(to_plot, type = "l", col = "blue", xlab = "Year", ylab = "Inflation YoY", main = "CPI without oil and rent ARIMA (4,1,1)")
lines(to_plot_2, col = "red")
abline(h = mean(to_plot,, na.rm = TRUE), col = "Black")
legend("topleft",           # Position of the legend
       legend = c("Observed", "Forecasted", "Mean"),  # Legend labels
       col = c("Blue", "Red", "Black"),       # Colors
       lty = 1)
dev.off()