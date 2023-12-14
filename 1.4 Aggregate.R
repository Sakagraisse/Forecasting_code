
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
load("arima_forecast.RData")
load("arimaX_forecast.RData")
load("ecm_forecast.RData")

#load data : CPIs.RData
load("CPIs_double_minus.RData")

weight_OIL <- c(CPIs$W_oil,rep(CPIs$W_oil[length(CPIs$W_oil)], 36))
weight_Rent <- c(CPIs$W_housing,rep(CPIs$W_housing[length(CPIs$W_housing)], 36))
weight_Rent_OIL <- c(CPIs$Totalw_o_r,rep(CPIs$Totalw_o_r[length(CPIs$Totalw_o_r)], 36))

#compute each contribution
contri_oil <- weight_OIL * ecm_forecast/100
contri_oil <-ts (contri_oil, start = c(2000,1), frequency = 12)
contri_oil <- aggregate(contri_oil,4,mean)

new_weight<- ts(weight_Rent, start = c(2000,1), frequency = 12)
new_weight <- aggregate(new_weight,4, mean)

contri_rent <- c(rep(NA, 35,),arimaX_forecast)
contri_rent <- contri_rent * new_weight/100

contri_rent_oil <- weight_Rent_OIL * arima_forecast/100
contri_rent_oil <-ts (contri_rent_oil, start = c(2000,1), frequency = 12)
contri_rent_oil <- aggregate(contri_rent_oil,4,mean)

aggregate <- as.numeric(contri_oil + contri_rent + contri_rent_oil)
length(aggregate)
aggregateYoY <- (aggregate / lag(aggregate, 4) - 1) * 100
length(aggregateYoY)
aggregateYoY <- ts(aggregateYoY, start = c(2000,1), frequency = 4)

plot(aggregateYoY, type = "l", col = "red", xlab = "Year", ylab = "Inflation", main = "CPIs YoY")

contri_rent <- ts(contri_rent, start = c(2000,1), frequency = 4)
contri_rent_oil <- ts(contri_rent_oil, start = c(2000,1), frequency = 4)




### out of sample test
#aggregate the 3 out of sample forecast
# add 35 row on top of  arimaX_out
to_add <- data.frame(matrix(NA, nrow = 35, ncol = ncol(arimaX_out)))
colnames(to_add) <- colnames(arimaX_out)
arimaX_out <- rbind(to_add, arimaX_out)
total_out_q <- data.frame(matrix(NA, nrow = (nrow(arimaX_out))))

for(i in seq(1, ncol(arima_out), 1)){
    #select column i of arima_out
    
    temp1 <- as.numeric(arima_out[,i])
    temp1 <- temp1 * CPIs$Totalw_o_r /100
    temp2 <- as.numeric(ecm_out[,i])
    temp2 <- temp2 * CPIs$W_oil /100
    temp1 <- ts(temp1, start = c(2000,1), frequency = 12)
    temp2 <- ts(temp2, start = c(2000,1), frequency = 12)
    temp1 <- aggregate(temp1,4,mean)
    temp2 <- aggregate(temp2,4,mean)
    test <- temp1 + temp2
    test <- as.numeric(test)
    total_out_q <- data.frame(total_out_q, test)
}
total_out_q <- total_out_q[,-1]

for(i in seq(1, ncol(arimaX_out), 1)){

    temp <- as.numeric(arimaX_out[,i])
    temp_w <- CPIs$W_housing
    temp_w <- ts(temp_w, start = c(2000,1), frequency = 12)
    temp_w <- aggregate(temp_w,4,mean)
    temp <- temp * temp_w /100
    total_out_q[,i] <- total_out_q[,i] + temp

}

test <- ecm_out[,1]
test <- ts(test, start = c(2000,1), frequency = 12)
test[151]
test <- arimaX_out[,20]
test <- ts(test, start = c(2000,1), frequency = 4)
