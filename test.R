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


######
# DATA import and tranformation
######

#load data : CPIs.RData
load("CPIs.RData")

######### weight data
dataw <- read_excel("wieght_data.xlsx", sheet = 1, col_names = TRUE)
CPIs$Inf_OIL <- log(CPIs$`Petroleum.products`/lag(CPIs$`Petroleum.products`,12))
CPIs$Inf_Rent <- log(CPIs$`Housing.rental.1`/lag(CPIs$`Housing.rental.1`,12))
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


fit <- auto.arima(CPIs$Total, seasonal = FALSE, stepwise = FALSE, approximation = FALSE, trace = TRUE)

fore <- forecast(fit, h = 36)

plot(fore)

info <- c(CPIs$Total,fore$mean)
zeah <- log(info/lag(info,12))
zeah <- ts(zeah, start = c(2000,1), frequency = 12)

plot(zeah, type = "l", col = "red", xlab = "Year", ylab = "Index", main = "CPIs without rent and without petroleum products")
toto <- zeah[1:285]
toto <- ts(toto, start = c(2000,1), frequency = 12)
lines(toto, type = "l", col = "blue")




######
# si necessaire
######


#aggregate the error 4 rows by 4 rows
Error$group <- (seq_len(nrow(Error)) - 1) %/% 3
Error <- Error %>%
  group_by(group) %>%
  summarise(across(everything(), mean, na.rm = TRUE))
Error <- Error[,-1]
