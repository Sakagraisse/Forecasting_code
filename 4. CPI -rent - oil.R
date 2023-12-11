#clean space
rm(list = ls())

# Get the current working directory
current_directory <- getwd()
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


#load data : CPIs.RData
#load data : CPIs.RData
load("CPIs.RData")

######### weight data
dataw <- read_excel("wieght_data.xlsx", sheet = 1, col_names = TRUE)
#CPIs <- CPIs_trunk
#rm(CPIs_trunk)
CPIs$Inf_OIL <- log(CPIs$`Petroleum.products`/lag(CPIs$`Petroleum.products`,12))
CPIs$Inf_Rent <- log(CPIs$`Housing.rental.1`/lag(CPIs$`Housing.rental.1`,15))
CPIs$Inf_Total <- log(CPIs$Total/lag(CPIs$Total,12))
dataw <- dataw[,c(1:6)]
#create a repetition of each line 12 times
monthly_sequence <- rep(1:12, each = nrow(dataw))

# Repeat each row in your_data 12 times
monthly_data <- dataw[rep(seq_len(nrow(dataw)), each = 12), ]
# have the same lenght for CPIs and weight
CPIs <- CPIs[-c(1:204),]
monthly_data <- monthly_data[c(1:285),]
#create new column for weight of total minus oil minus rent
monthly_data$Totalw_o_r <- monthly_data$Total - monthly_data$Oil - monthly_data$Rent
#create time series for each column
weight_rent <- ts(monthly_data$Rent, start = c(2000,1), frequency = 12)
weight_oil <- ts(monthly_data$Oil, start = c(2000,1), frequency = 12)
weight_w_o <- ts(monthly_data$Totalw_o, start = c(2000,1), frequency = 12)
weight_w_r <- ts(monthly_data$Totalw_r, start = c(2000,1), frequency = 12)
weight_w_o_r <- ts(monthly_data$Totalw_o_r, start = c(2000,1), frequency = 12)

## create the wieghted CPIS for oil and rent
#create loop to multiply each value of cpi with the weight
for (i in 1:length(CPIs$Inf_Rent)){
    CPIs$Inflation.withoutRI_log[i] <- (CPIs$Inf_Total[i] - CPIs$Inf_OIL[i]*weight_oil[i] - CPIs$Inf_Rent[i]*weight_rent[i])/weight_w_o_r[i]*100
}


#plot without rent index and withoutRI
plot(CPIs$Year, CPIs$`Inflation.withoutRI_log`, type = "l", col = "red", xlab = "Year", ylab = "Index", main = "CPIs without rent and without petroleum products")
#plot(CPIs$Year, CPIs$Inf_Total, type = "l", col = "red", xlab = "Year", ylab = "Index", main = "CPIs without rent and without petroleum products")

#auto arima fit CPIs$Inflation.withoutRI
fit <- auto.arima(CPIs$Inflation.withoutRI_log, seasonal = FALSE, approximation = FALSE, trace=TRUE)
#reduce data to remove NAs introduced by the lag
CPIs <- CPIs[13:nrow(CPIs),]


######
# 2 Fitting ARIMA model
######

#check stationnarity manually
#manually check the best model by finding p,d,q in an ARIMA(p,d,q) model
#check for d with dickey fuller test
adf.test(CPIs$Inflation.withoutRI_log, alternative = "stationary", k = trunc((length(CPIs$Inflation.withoutRI_log)-1)^(1/3)))
## Not Stationnary
# check with KPSS
kpss.test(CPIs$Inflation.withoutRI_log, null = "Trend", lshort = TRUE)
## is stationnary

#check for p using PACF
pacf(CPIs$Inflation.withoutRI_log, lag.max = 20, plot = TRUE)
## around 3
#check for q using ACF
acf(CPIs$Inflation.withoutRI_log, lag.max = 20, plot = TRUE)
## around 2

#fit the correpsonding ARIMA(2,0,0) model
fit2 <- arima(CPIs$Inflation.withoutRI_log, order = c(3,0,0), method = "ML")



# export graph
png(file = paste(getwd(), "/Graphs/double minus/test.png", sep=""))
plot(tosee,type = "l", col = "red", xlab = "Year", ylab = "Index", main = "CPIs without rent and without petroleum products")
dev.off()

#check the residuals
checkresiduals(fit2)

######
# In sample tests
######

#calculate the in sample residuals
in_sample_residuals <- fit2$residuals
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
White <- white_test(fit2)

in_sample_tests <- data.frame(Ljung$p.value, White$p_value, Jarques$p.value,Pierce$p.value)
rm(Ljung, Pierce, Jarques, White)

######
# Out of sample tests comparative
######

### out of sample forecast for our model

Inflation.withoutRI_log <- ts(CPIs$Inflation.withoutRI_log,start = c(2000,1), frequency = 12)
out_of_sample <- data.frame(matrix(ncol = 1, nrow = 36))
mean_of_fit <- data.frame(matrix(ncol = 1, nrow = 36))
end <- nrow(CPIs)
end <- end - 36

#png(file = paste(getwd(), "/Graphs/double minus/spag.png", sep=""))
plot(Inflation.withoutRI_log)

#iterate from line 36 to the en of CPIs
for (i in 37:end){
    temporary <- Inflation.withoutRI_log[1:i-1]
    temporary <- ts(temporary, start = c(2000,1), frequency = 12)
    end_year <- end(temporary)[1]
    end_month <- end(temporary)[2]
    #fit arima model on the first i-1 observations
    fit <- arima(temporary, order = c(3,0,0))

    #forecast the i-th observation
    fore <- forecast(fit, h = 36)
    print <- ts(fore$mean, start = c(end_year, end_month + 1 ), frequency = 12)
    if (i %in% seq(from = 1, to=end, by=10)){
        lines(print, col="red")
    }

    #calculate the out of sample forecast
    to_save <- (as.numeric(fore$mean) - Inflation.withoutRI_log[i:i+36])^2
    out_of_sample <- data.frame(out_of_sample, to_save)
    mean_of_fit <- data.frame(mean_of_fit, fore$mean)

}
#dev.off()
#remove first column
Squared <- out_of_sample[,-1]
MSFE_by_time <- colMeans(Squared[1,] , na.rm = TRUE)
for (i in 2:36){
    MSFE_by_time <- rbind(MSFE_by_time, colMeans(Squared[1:i,] , na.rm = TRUE))
}
MSFE_Total <- rowMeans(MSFE_by_time, na.rm = TRUE)
#rm(end, end_month, end_year, fore, i, mean_of_fit, out_of_sample, print, temporary, to_save)


### out of sample forecast for benchmark model

Inflation.withoutRI_log <- ts(CPIs$Inflation.withoutRI_log,start = c(1984,1), frequency = 12)
out_of_sample_b <- data.frame(matrix(ncol = 1, nrow = 36))
mean_of_fit_b <- data.frame(matrix(ncol = 1, nrow = 36))
end_b <- nrow(CPIs)
end_b <- end_b - 36
#plot(Inflation.withoutRI_log)

#iterate from line 36 to the en of CPIs
for (i in 37:end_b){
    temporary <- Inflation.withoutRI_log[1:i-1]
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
    to_save <- (as.numeric(fore$mean) - Inflation.withoutRI_log[i:i+36])^2
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

######
# Out of sample tests Diebold
######


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







