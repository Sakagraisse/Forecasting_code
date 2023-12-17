
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

######
# clean space
######

rm(list = ls())
# Get the current working directory
current_directory <- getwd()

######
# 1 DATA importation
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
# convert OIL_CHF in proportion of 2020-12-01 prices
ECM_Data$B20 <- (ECM_Data$OIL_CHF / ECM_Data$OIL_CHF[which(ECM_Data$Date == "2020-12-01")] ) * 100

######
# Petroleum Products
######
load("CPIs.RData")

#remove CPIs dates below the first date of ECM_Data
CPIs <- subset(CPIs, CPIs$Year >= "1986-01-01")
# Bring Petroleum in ecm data
ECM_Data <- merge(ECM_Data, CPIs, by.x = "Date", by.y = "Year", all.x = TRUE)

## plot B20 and oil
plot(ECM_Data$B20, type = "l", col = "red")
lines(ECM_Data$Petroleum.products, type = "l", col = "blue")

#select from 2000
ECM_Data <- ECM_Data[169:nrow(ECM_Data),]
ECM_Data$B20ts <- ts(ECM_Data$B20, start = c(2000,1), frequency = 12)
ECM_Data$Petroleum.productsts <- ts(ECM_Data$Petroleum.products, start = c(2000,1), frequency = 12)
plot(ECM_Data$B20ts , type = "l", col = "red")
lines(ECM_Data$Petroleum.productsts, type = "l", col = "blue")

######
# perfom checks before ECM
######
#check for stationarity of the data
adf.test(ECM_Data$B20)
#non stationnarity satisfied

adf.test(ECM_Data$Petroleum.products)
#non stationnarity satisfied

#check for cointegration between the two time series using urca package
test_co_integration <- ca.jo(data.frame(ECM_Data$B20,ECM_Data$Petroleum.products), type = "trace", ecdet = "const", K = 2, spec = "transitory")
#display the results
summary(test_co_integration)


######
# Estimate the ecm
######

# Create a function that estimates the ecm
# and output the results of the dataframe
the_famous_ECM <- function (data){
lm1 <- lm(data$Petroleum.products~data$B20) #Create the linear regression

#create a lag of one for OIL and B10
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
# Create a function that estimates the ecm
# and output the coefficients of each regression
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
df <-  data.frame(lm1_1, lm1_2, lm2_1, lm2_2, lm2_3, lm2_4)
return(df)
}
ECM_Data <- the_famous_ECM(ECM_Data)
coeff <- the_famous_ECM_coeff(ECM_Data)

######
# Predictions
######

#create function to create data_forcast from different lenghts of row ECM_Data
# in other word, create a dataframe with the last row of ECM_Data and the number of rows you want to forecast
# using a random walk for the last row of B20
# and leave empty the rest of the rows
create_data_forecast <- function(data_to_use, end_row, steps_ahead) {
  data_forecast <- data_to_use[1:end_row,]
  # add the required number of rows by empty rows
  ll <- length(data_forecast$B20)
  new_rows <- data.frame(matrix(NA, nrow = steps_ahead, ncol = ncol(data_forecast)))
  colnames(new_rows) <- colnames(data_forecast)

  #fill the empty rows with the last row of ECM_Data for B20
  data_forecast <- rbind(data_forecast, new_rows)
  data_forecast$B20[(ll+1:steps_ahead)] <- rep(tail(data_forecast$B20[ll], 1), steps_ahead)
  data_forecast$B20_lag1[(ll+1:steps_ahead)] <- rep(tail(data_forecast$B20[ll], 1), steps_ahead)
  data_forecast$B20_delta[(ll+1:steps_ahead)] <- rep(0, steps_ahead)

  return(data_forecast)
}



# Function to forecast future values
forecast_ECM <- function(data_to_use,starting_row, steps_ahead,results_coeff) {
  # copy to a temporary dataframe
  temp <- data_to_use
  # Initialize the forecast dataframe with the last row of ECM_Data
  l_base <- starting_row
  # Iterate for the number of steps you want to forecast
  for(i in 1 :(steps_ahead)) {
    # Create petrol  lag 1
    temp$Petroleum.products_lag1[(l_base + i)] <- temp$Petroleum.products[(l_base + i - 1)]
    # Calculate long term correction
    temp$long_term_correction[(l_base + i)] <- temp$Petroleum.products_lag1[(l_base + i)] - results_coeff$lm1_1 -results_coeff$lm1_2* temp$B20_lag1[(l_base + i)]
    # calculate B20 delta
    temp$B20_delta[(l_base + i)] <- temp$B20[(l_base + i)] - temp$B20_lag1[(l_base + i)]
    # Calculate petroleum products delta
    temp$Petroleum.products_delta[(l_base + i)] <- results_coeff$lm2_1 + results_coeff$lm2_2 * temp$B20_delta[(l_base + i)] + results_coeff$lm2_3 * temp$long_term_correction[(l_base + i)]

    # Update the value of oil
    temp$Petroleum.products[(l_base + i)] <- temp$Petroleum.products_lag1[(l_base + i)] + temp$Petroleum.products_delta[(l_base + i)]


  }
  return(temp)
}

#create the main forecast and store it
pourvoir <- create_data_forecast(ECM_Data, nrow(ECM_Data), 36)
#remove index due to specificities of R dataframe storing row truncated form ts
rownames(pourvoir) <- NULL
pourvoir2 <- forecast_ECM(pourvoir, nrow(ECM_Data), 36, coeff)


### out of sample forecast for benchmark model

#create matrix to store the datas
out_of_sample <- data.frame(matrix(ncol = 1, nrow = 36))
mean_of_fit <- data.frame(matrix(ncol = 1, nrow = 285))
end <- nrow(ECM_Data)
end <- end - 36

#iterate from line 151 to the en of CPIs (not the same index as other function...
#due to personnalized function)
for(i in (150):(end)){
    # truncate the number of lines of data
    cherpa <- create_data_forecast(ECM_Data, i, 36)
    rownames(cherpa) <- NULL
    #fit arima model on the first i-1 - 37 observations
    results_coeff <- the_famous_ECM_coeff(head(cherpa, i))
    forecast <- forecast_ECM(cherpa, i ,36, results_coeff)
    mean_of_fit <- data.frame(mean_of_fit, c(forecast$Petroleum.products,rep(NA,(end + 36 - length(forecast$Petroleum.products)))))
    #calculate the out of sample residuals
    to_save <- forecast$Petroleum.products[(i+1):(i+36)] - ECM_Data$Petroleum.products[(i+1):(i+36)]
    out_of_sample <- data.frame(out_of_sample, to_save)
}
## remove place holder columns
mean_of_fit <- mean_of_fit[,-1]
Error <- out_of_sample[,-1]

#Prepare series for predictive rsquared and Diebold Mariano test
Error_ag <- rowMeans(Error, na.rm = TRUE)
Error_sq <- rowMeans(Error^2, na.rm = TRUE)





### out of sample forecast for benchmark model

#create matrix to store the datas
temp <- ECM_Data$Petroleum.products
rownames(temp) <- NULL
out_of_sample_b <- data.frame(matrix(ncol = 1, nrow = 36))
mean_of_fit_b <- data.frame(matrix(ncol = 1, nrow = length(temp)))
end_b <- length(temp)
end_b <- end_b - 36
# Line types
#forecast from line 151 to the end of CPIs benchmark model
for (i in 151:(end_b+1)){
    #store the truncated serie for fitting
    temporary <- temp[1:(i-1)]
    temporary <- ts(temporary, start = c(2000,1), frequency = 12)
    #fit arima model on the first i-1 observations
    fit <- arima(temporary, order = c(1,1,0))
    fore <- forecast(fit, h = 36)
    #save the mean of the fit
    to_save <- c(temporary, fore$mean, rep(NA, (end_b - i + 1)))
    mean_of_fit_b <- data.frame(mean_of_fit_b, to_save)
    #save the error
    to_save_2 <- fore$mean -  as.numeric(temp[(i):(i+35)])
    out_of_sample_b <- data.frame(out_of_sample_b, as.numeric(to_save_2))
}
## remove place holder columns
mean_of_fit_b <- mean_of_fit_b[,-1]
Error_b <- out_of_sample_b[,-1]

#Prepare series for predictive rsquared and Diebold Mariano test
Error_b_ag <- rowMeans(Error_b, na.rm = TRUE)
Error_b_sq <- rowMeans(Error_b^2, na.rm = TRUE)

rm(end_b, fore, i, temporary, to_save, to_save_2)

MSFE_pred_by_time <- 1 - (Error_sq/Error_b_sq)


pdf(paste(getwd(), "/Graphs/ECM/predictive_r_ECM.pdf", sep=""), width = 13, height = 5)

MSFE_pred_by_time <- MSFE_pred_by_time[seq(1, length(MSFE_pred_by_time), 3)] # first or last month ?
barplot(MSFE_pred_by_time,names.arg = 1:12,main = "Predictive R_Squared by period ECM against AR(1)" )

dev.off()


## plot spaghetti graph
temp <- ECM_Data$Petroleum.products
petro <- (temp / lag(temp, 12) - 1) * 100
petro <- petro[13:length(petro)]
petro <- ts(petro, start = c(2001,1), frequency = 12)
petro <- aggregate(petro, nfrequency = 4, FUN = mean)



pdf(paste(getwd(), "/Graphs/ECM/spag_ECM.pdf", sep=""), width = 8, height = 5)



plot(petro , type = "l", col = "blue", xlab = "Year", ylab = "Inflation", main = "Spaghetti graph CPIs Petroleum Products ECM (1 over 3)")
abline(h = mean(petro, na.rm = TRUE), col = "Black")
legend("topleft",           # Position of the legend
       legend = c("Observed", "Out-of-Sample Forecast"),  # Legend labels
       col = c("Blue", "Red"),       # Colors
       lty = 1)


#plot the out of sample forecast
#from : choose the first month starting for a quarter in the out of sample forecast
#to : size of the out of sample forecast (columns of mean_of_fit)
#by : step of multiple of 6 to get a full quarter each time
for (i in seq(from = 1, to = 100, by = 3)){
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
        print <- mean_of_fit_b[,i]
        print <- (print / lag(print ,12) - 1) * 100
        print[1:(150+i -4)] <- NA

        print <- ts(print, start = c(2000,1), frequency = 12)
        print <- aggregate(print, nfrequency = 4, FUN = mean)
        lines(print, col="green")
}
dev.off()

#####
# Out of sample tests Diebold
######

Diebold_DM<- c()
Diebold_p<- c()

#calculate the Diebold Mariano test for each period power = 1 due to alread aggregated data of squared errors
for(i in 1:36){
    Diebold_DM[i] <- dm.test(Error_sq, Error_b_sq, alternative = "two.sided", h = i, power = 1,varestimator = "bartlett")$statistic
    Diebold_p[i] <- dm.test(Error_sq, Error_b_sq, alternative = "two.sided", h = i, power = 1,varestimator = "bartlett")$p.value
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
print(latex_table, type = "latex", floating = FALSE, file = (paste(getwd(), "/Graphs/ECM/diebold_ECM.txt", sep="")))



#####
# save ecm
#####

ecm_error <- Error
#Last column of mean_of_fit
ecm_forecast <- pourvoir2$Petroleum.products
ecm_out <- mean_of_fit

#save
save(ecm_error, ecm_forecast, ecm_out, file = "ecm_forecast.RData")

#####
# plot forecast
#####

pdf(paste(getwd(), "/Graphs/ECM/forecast_ECM.pdf", sep=""), width = 8, height = 5)
to_plot <- (ecm_forecast / lag(ecm_forecast, 12) - 1) * 100
to_plot <- ts(to_plot, start = c(2000,1), frequency = 12)
to_plot <- aggregate(to_plot, nfrequency = 4, FUN = mean)
to_plot_2 <- tail(to_plot,13)
to_plot[(length(to_plot)-11):length(to_plot)] <- NA
plot(to_plot, type = "l", col = "blue", xlab = "Year", ylab = "Inflation YoY", main = "CPI Petroleum Products ECM forecast")
lines(to_plot_2, col = "red")
abline(h = mean(to_plot,, na.rm = TRUE), col = "Black")
legend("topleft",           # Position of the legend
       legend = c("Observed", "Forecasted", "Mean"),  # Legend labels
       col = c("Blue", "Red", "Black"),       # Colors
       lty = 1)
dev.off()