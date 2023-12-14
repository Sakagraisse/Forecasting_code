
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

#clean space
rm(list = ls())
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
test <- ca.jo(ECM_Data[,c(5,9)], type = "trace", ecdet = "const", K = 2, spec = "transitory")
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
df <-  data.frame(lm1_1, lm1_2, lm2_1, lm2_2, lm2_3, lm2_4)
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
  new_rows <- data.frame(matrix(NA, nrow = steps_ahead, ncol = ncol(data_forecast)))
  colnames(new_rows) <- colnames(data_forecast)
  data_forecast <- rbind(data_forecast, new_rows)
  data_forecast$B20[(ll+1:steps_ahead)] <- rep(tail(data_forecast$B20[ll], 1), steps_ahead)
  data_forecast$B20_lag1[(ll+1:steps_ahead)] <- rep(tail(data_forecast$B20[ll], 1), steps_ahead)
  data_forecast$B20_delta[(ll+1:steps_ahead)] <- rep(0, steps_ahead)

  return(data_forecast)
}



# Function to forecast future values
forecast_ECM <- function(data_to_use,starting_row, steps_ahead,results_coeff) {
  temp <- data_to_use
  # Initialize the forecast dataframe with the last row of ECM_Data
  l_base <- starting_row
  # Iterate for the number of steps you want to forecast
  for(i in 1 :(steps_ahead-1)) {
    # Create oil  lag 1

    temp$Petroleum.products_lag1[(l_base + i)] <- temp$Petroleum.products[(l_base + i - 1)]
    # Calculate long term correction
    temp$long_term_correction[(l_base + i)] <- temp$Petroleum.products_lag1[(l_base + i)] - results_coeff$lm1_1 -results_coeff$lm1_2* temp$B20_lag1[(l_base + i)]

    # Calculate OIL delta
    temp$Petroleum.products_delta[(l_base + i)] <- results_coeff$lm2_1  + results_coeff$lm2_3 * temp$long_term_correction[(l_base + i)]

    # Update the value of oil
    temp$Petroleum.products[(l_base + i)] <- temp$Petroleum.products_lag1[(l_base + i)] + temp$Petroleum.products_delta[(l_base + i)]


  }
  return(temp)
}

pourvoir <- create_data_forecast(ECM_Data, nrow(ECM_Data), 37)
rownames(pourvoir) <- NULL
pourvoir2 <- forecast_ECM(pourvoir, nrow(ECM_Data), 37, coeff)
#generate and store for ECM

out_of_sample <- data.frame(matrix(ncol = 1, nrow = 36))
mean_of_fit <- data.frame(matrix(ncol = 1, nrow = 286))
end <- nrow(ECM_Data)
end <- end - 36

for(i in (150):(end)){
    # truncate the number of lines of data
    cherpa <- create_data_forecast(ECM_Data, i, 37)
    rownames(cherpa) <- NULL
    #fit arima model on the first i-1 - 37 observations
    results_coeff <- the_famous_ECM_coeff(head(cherpa, i))
    forecast <- forecast_ECM(cherpa, i ,37, results_coeff)
    mean_of_fit <- data.frame(mean_of_fit, c(forecast$Petroleum.products,rep(NA,(end + 36 + 1 - length(forecast$Petroleum.products)))))
    #calculate the out of sample residuals
    to_save <- forecast$Petroleum.products[(i+1):(i+36)] - ECM_Data$Petroleum.products[(i+1):(i+36)]
    out_of_sample <- data.frame(out_of_sample, to_save)
}
# and first column
Error <- out_of_sample[,-1]
#remove last line
Error_ag <- rowMeans(Error, na.rm = TRUE)
Error_sq <- rowMeans(Error^2, na.rm = TRUE)





### out of sample forecast for dumb

temp <- ECM_Data$Petroleum.products
rownames(temp) <- NULL
out_of_sample_b <- data.frame(matrix(ncol = 1, nrow = 36))
mean_of_fit_b <- data.frame(matrix(ncol = 1, nrow = length(temp)))
end_b <- length(temp)
end_b <- end_b - 36
# Line types
#iterate from line 36 to the en of CPIs
for (i in 151:(end_b+1)){
    temporary <- temp[1:(i-1)]
    temporary <- ts(temporary, start = c(2000,1), frequency = 12)
    end_year <- end(temporary)[1]
    end_month <- end(temporary)[2]
    #fit arima model on the first i-1 observations
    fit <- arima(temporary, order = c(1,1,0))
    fore <- forecast(fit, h = 36)
    #save the mean of the fit
    to_save <- c(temporary, fore$mean, rep(NA, (end_b - i + 1)))
    mean_of_fit_b <- data.frame(mean_of_fit_b, to_save)
    #save the error
    to_save_2 <- fore$mean - as.numeric(tail(temp, 36))
    out_of_sample_b <- data.frame(out_of_sample_b, as.numeric(to_save_2))
}
Error_b <- out_of_sample_b[,-1]
Error_b_ag <- rowMeans(Error_b, na.rm = TRUE)
Error_b_sq <- rowMeans(Error_b^2, na.rm = TRUE)
rm(end_b, end_month, end_year, fore, i, temporary, to_save, to_save_2, Error_mean_by_time_b)



MSFE_pred_by_time <- 1 - (Error_sq/Error_b_sq)


pdf(paste(getwd(), "/Graphs/double minus/predictive_r_double_minus.pdf", sep=""), width = 13, height = 5)

barplot(MSFE_pred_by_time,names.arg = 1:36,main = "Predictive R_Squared by period" )

dev.off()


## plot spaghetti graph
temp <- ECM_Data$Petroleum.products
petro <- (temp / lag(temp, 12) - 1) * 100
petro <- petro[13:length(petro)]
petro <- ts(petro, start = c(2001,1), frequency = 12)
petro <- aggregate(petro, nfrequency = 4, FUN = mean)
#remove first column of mean_of_fit
mean_of_fit <- mean_of_fit[,-1]
mean_of_fit <- head(mean_of_fit, (nrow(mean_of_fit)-1))
mean_of_fit_b <- mean_of_fit_b[,-1]

pdf(paste(getwd(), "/Graphs/double minus/spag.pdf", sep=""))
dev.off()


plot(petro , type = "l", col = "red", xlab = "Year", ylab = "Inflation", main = "Spaghetti graph CPIs YoY without rent and without petroleum products")
legend("topleft",           # Position of the legend
       legend = c("ARIMA(3,0,0)", "ARIMA(1,0,0)"),  # Legend labels
       col = c("Blue", "Green"),       # Colors
       lty = 1)

# "to" needs to be the leght of the series
for (i in seq(from = 1, to = 100, by = 5)){

        print <- mean_of_fit[,i]
        print <- (print / lag(print ,12) - 1) * 100
        print <- print[13:length(print)]
        print <- ts(print, start = c(2001,1), frequency = 12)
        print <- tail(print,( 36 + 100 + 6 - i ))
        month <- start(print)[2]

        if(month %in% c(1,4,7,10)){
            print <- aggregate(print, nfrequency = 4, FUN = mean)
            lines(print, col="blue")
        }


        print <- mean_of_fit_b[,i]
        print <- (print / lag(print ,12) - 1) * 100
        print <- print[13:length(print)]
        print <- ts(print, start = c(2001,1), frequency = 12)
        print <- tail(print, (36 + 100 + 6 - i))
        month <- start(print)[2]
        if(month %in% c(1,4,7,10)){
            print <- aggregate(print, nfrequency = 4, FUN = mean)
            lines(print, col="green")
        }
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
# save ecm
#####

ecm_error <- Error
#Las column of mean_of_fit
ecm_forecast <- pourvoir2$Petroleum.products[1:(length(pourvoir2$Petroleum.products)-1)]
ecm_out <- mean_of_fit

#save
save(ecm_error, ecm_forecast, ecm_out, file = "ecm_forecast.RData")