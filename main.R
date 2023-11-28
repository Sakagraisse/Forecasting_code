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

data <- read_excel("CPI_2020.xlsx", sheet = 1, col_names = TRUE, skip = 1)
#check which type is data
class(data)

#Remove the first 5 columns
data <- data[,12:504]

#Remove the columns 2
data <- data[,-2]
#Remove columns 3
data <- data[,-3]
#Remove first line
data <- data[-1,]
# Make the first line the name of the columns
colnames(data) <- data[1,]
#check type of data
class(data)
#exctracte column 2 and name it weight
weight <- data[,1:2]
#remove the first line
weight <- weight[-c(1:3),]
#remove first line and secdond column from data
data <- data[,-2]
#change the first index of the table to "Year"
data[1,1] <- "Year"
#check the type of data$30317
class(data$`30317`)
#Reshape the data so the years which is the first line are in one column
data <- t(data)
#make the first line the name of the columns
colnames(data) <- data[1,]
#remove the first line
data <- data[-1,]
#make colums vectors again and make it numbers not characters
data <- as.data.frame(data)
#format each column of data to be numeric
data <- data.frame(lapply(data, as.numeric))
#format the first column of date to display a date from the excel way of counting using openxlsx package
data$Year <- as.Date(as.numeric(data$Year), origin = "1899-12-30")


#####Rent data quarterly-----
#Mortgage rate quarterly

#import excel file with mortgage rate
data2 <- read_excel("mortgage_rate_c.xlsx", sheet = 1, col_names = TRUE)

#rename first column "mortgage rate"
colnames(data2)[1] <- "mortgage.rate"
#rename second column "date"
colnames(data2)[2] <- "date"

#order date
data2 <- data2[order(data2$date),]

#create dates
data2$dates <- seq(as.Date("2008-09-01"), by = "3 months", length.out = nrow(data2))
#delete date column
data2 <- data2[,-2]
#order date
data2 <- data2[order(data2$dates),]
#keep only Rent and Year variables from data3
data3 <- data[,c(1,156)]
#Convert monthly Rent data3 to quarterly
data3 <- data3 %>%
  mutate(date = as.Date(Year),  # Ensure the 'Year' column is in Date format
         year = year(date),
         quarter = quarter(date)) %>%
  group_by(year, quarter) %>%
  # Remove rows with NA in 'Rent' before summarizing
  filter(!is.na(Rent)) %>%
  summarize(last_quarter_rent = last(Rent))  # Get the last Rent value of each quarter

data3 <- data3 %>%
  mutate(Year = paste(year,
                      case_when(
                        quarter == 1 ~ "03.01",
                        quarter == 2 ~ "06.01",
                        quarter == 3 ~ "09.01",
                        quarter == 4 ~ "12.01"
                      ),
                      sep="."
  )
  )
#Keep only the variables Year and quarterly_rent
data3 <- data3[,c(3,4)]
#drop the first 102
data3 <- data3[-c(1:102),]
#rename quareterly_rent to Rent
colnames(data3)[1] <- "Rent"

#Merge data3 and data 2
data3$mortgage.rate <- data2$mortgage.rate

#create the "good monthly" times series using show_lin
# Exemple de séries trimestrielles
rent_quarterly <- ts(data3$Rent, start=c(2008,3), frequency=4)
mortgage_rate_quarterly <- ts(data3$mortgage.rate, start=c(2008,3), frequency=4)

#create a dataset with housing.and.energy from data but only the last 309 lines

data4 <- data$Housing.and.energy
#only keep the last 309 lines of data4
data4 <- data4[-c(1:309)]

# Exemple de série indicatrice mensuelle
indicator_monthly <- ts(data4, start=c(2008,6), frequency=12)

rent_monthly <- td(rent_quarterly ~ 1, to = "monthly", method = "denton-cholette")
mortgage_rate_monthly <- td(mortgage_rate_quarterly ~ 1, to = "monthly", method = "denton-cholette")

plot(rent_monthly)
plot(mortgage_rate_monthly)


rent_monthly_numeric <- as.numeric(rent_monthly[[1]])
mortgage_rate_monthly_numeric <- as.numeric(mortgage_rate_monthly[[1]])
rent_monthly_inflation <- log(rent_monthly_numeric/lag(rent_monthly_numeric, 12))


###### Rent forecast -----
#ARIMA model for rent  with mortgage rate as exogenous variable
# Dclate Rent as a quaretly time series
Rent <- ts(rent_monthly_inflation,start=c(2008,6),frequency=12)
mortgage.rate <- ts(mortgage_rate_monthly_numeric,start=c(2008,6),frequency=12)



#adf.test(Rent)
#rent.diff <- diff(Rent)
#adf.test(rent.diff)

#adf.test(mortgage.rate)
#mortgage.rate.diff <- diff(mortgage.rate)
#adf.test(mortgage.rate.diff)

fit <- auto.arima(Rent, xreg = mortgage.rate, seasonal=FALSE, approximation=FALSE, trace=TRUE)
summary(fit)
checkresiduals(fit)

## Forecast of the mortgage rate
# Fit an ARMA model to the historical mortgage rate data
#first difference for mortgage rate

fit2 <- auto.arima(mortgage.rate, seasonal=FALSE, stepwise=TRUE, approximation=FALSE)
checkresiduals (fit2)
# Use the fitted ARMA model to forecast future mortgage rates
future_mortgage_rate_forecast <- forecast(fit2, h=12)
# The forecast object contains point forecasts, lower and upper confidence intervals
print(future_mortgage_rate_forecast)
# Plot the forecast to visualize
plot(future_mortgage_rate_forecast)
# Extract the point forecasts of the future mortgage rates
future_mortgage_rate_values <- future_mortgage_rate_forecast$mean
# Forecasted mortgage rate values as the exogenous variable in the rent forecast
rent_forecast <- forecast(fit, xreg=future_mortgage_rate_values, h=12)
# Plot the forecast of rent
plot(rent_forecast)
# Print the forecasted rent values
print(rent_forecast)
#transform point forecast values to inflation rate


#Forecast of Total----------
#build a new column = data$Total - data$rent - oil
data5 <- data[,c(1,2,156,188)]
#keep after line 306
data5 <- data5[-c(1:305),]

Total <- ts(data5$Total,start=c(2008,6),frequency=12)
Heating.oil <- ts(data5$Heating.oil,start=c(2008,6),frequency=12)

#create time series equatl to total - rent - oil
Total_wor <- Total - 0.19325*Rent - 0.00603*Heating.oil
#drop NA
Total_wor <- na.omit(Total_wor)
Total_wor_numeric <- as.numeric(Total_wor)
Total_wor = log(Total_wor_numeric/lag(Total_wor_numeric,12))
Total_wor <- na.omit(Total_wor)


fit3 <- auto.arima(Total_wor, seasonal=FALSE, approximation=FALSE, trace=TRUE)
summary(fit3)
checkresiduals(fit3)
total_forecast_monthly <- forecast(fit3,  h=36)
# Plot the forecast of rent
plot(total_forecast_monthly$residuals)
# Print the forecasted rent values
print(total_forecast_monthly)
 plot(total_forecast_monthly$residuals)



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
ECM_Data$B10 <- (ECM_Data$OIL_CHF / ECM_Data$OIL_CHF[which(ECM_Data$Date == "2010-12-01")] ) * 100

######
# 2 Heating oil price
######

#add heating oil price to ECM_Data from  data$`Heating.oil`
heating_oil <- data.frame(Date = data$Year, oil = data$`Heating.oil`)
#merge heating_oil and ECM_Data
ECM_Data <- merge(ECM_Data, heating_oil, by.x = "Date", by.y = "Date", all.x = TRUE)
#remove heating_oil
rm(heating_oil)

## plot B17 and oil
plot(ECM_Data$B10, type = "l", col = "red")
lines(ECM_Data$oil, type = "l", col = "blue")

#plot oil
plot(ECM_Data$oil, type = "l", col = "blue")

######
# 4 perfom checks before ECM
######
#check for stationarity of the data
adf.test(ECM_Data$oil)
#non stationnarity satisfied

adf.test(ECM_Data$B10)
#non stationnarity satisfied

#check for cointegration between the two time series using urca package
test <- ca.jo(ECM_Data[,c(5,6)], type = "trace", ecdet = "const", K = 2, spec = "transitory")
#display the results
summary(test)


######
# 5 perfom the ecm
######

lm1 <- lm(ECM_Data$oil~ECM_Data$B10) #Create the linear regression
summary(lm1)
plot(lm1$residuals)

#create a lag ofe one for OIL and B10
ECM_Data$B10_lag1 <- lag(ECM_Data$B10,1)
ECM_Data$oil_lag1 <- lag(ECM_Data$oil,1)
#create a delta of OIL and B10
ECM_Data$B10_delta <- (ECM_Data$B10 - ECM_Data$B10_lag1)
ECM_Data$oil_delta <- (ECM_Data$oil - ECM_Data$oil_lag1)
#create long term correction
ECM_Data$long_term_correction <- ECM_Data$oil_lag1 - lm1$coefficients[1] - lm1$coefficients[2] * ECM_Data$B10_lag1

lm2 <- lm(ECM_Data$oil_delta~ECM_Data$B10_delta + ECM_Data$long_term_correction ) #Create the linear regression
plot(lm2$fitted.values)

#remove column 2 to 4 and store it in a new dataframe keep column 1
data_forecast <- ECM_Data[,c(1,5:11)]
ll <- length(data_forecast$B10)
new_rows <- data.frame(matrix(NA, nrow = 36, ncol = ncol(data_forecast)))
colnames(new_rows) <- colnames(data_forecast)
data_forecast <- rbind(data_forecast, new_rows)
data_forecast$B10[ll+1:36] <- rep(tail(ECM_Data$B10, 1), 36)
data_forecast$B10_lag1[ll+1:36] <- rep(tail(ECM_Data$B10, 1), 36)
data_forecast$B10_delta[ll+1:36] <- rep(0, 36)
#continue the date column
data_forecast$Date[ll+1:36] <- seq(as.Date("2023-10-01"), by = "1 months", length.out = 36)

# Function to forecast future values
forecast_ECM <- function(data_to_use,starting_row, steps_ahead) {
  temp <- data_to_use
  # Initialize the forecast dataframe with the last row of ECM_Data
  l_base <- starting_row
  # Iterate for the number of steps you want to forecast
  for(i in 2:steps_ahead-1) {
    # Create oil  lag 1
    temp$oil_lag1[l_base + i] <- temp$oil[l_base + i - 1]
    # Calculate long term correction
    temp$long_term_correction[l_base + i] <- temp$oil_lag1[l_base + i] - lm1$coefficients[1] - lm1$coefficients[2] * temp$B10_lag1[l_base + i]

    # Calculate OIL delta
    temp$oil_delta[l_base + i] <- lm2$coefficients[1] + lm2$coefficients[3] * temp$long_term_correction[l_base + i]

    # Update the value of oil
    temp$oil[l_base + i] <- temp$oil_lag1[l_base + i] + temp$oil_delta[l_base + i]


  }
  return(temp)
}

# Example usage: Forecasting 5 steps ahead
yay <- forecast_ECM(data_forecast,nrow(ECM_Data),36)


#plot oil delta from row 400
plot(yay$oil_delta[400:nrow(yay)], type = "l", col = "red")
lines(ECM_Data$oil_delta[400:nrow(ECM_Data)], type = "l", col = "blue")