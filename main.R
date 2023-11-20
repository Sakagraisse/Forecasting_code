#clean space
rm(list = ls())
# ##import data in excell format from the first table only
install.packages("readxl")
install.packages("reshape2")

library(readxl)

rm(list = ls())
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
#drop the first line
weight <- weight[-1,]
# transpose weight
weight <- t(weight)
#make the first line the name of the columns
colnames(weight) <- weight[1,]
#remove the first line
weight <- weight[-1,]
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

#Keep only Rent and Year variables from data from 309:489
data3 <- data[309:489,]
#keep only Rent and Year variables from data
data3 <- data3[,c(1,156)]
#convert monthly rent data to quarterly by taking the value of the last month of the quarter, 4 observatios per year
data3 <- data3[seq(1, nrow(data3), 3), ]
#Merge data3 and data 2
data3$mortagage.rate <- data2$mortgage.rate
#Quarterly Rent data
data4 <- data[,c(1,156)]
#convert monthly Rent data to quarterly from data4 by taking the value of the last month of each the quarter, starting from march
data4 <- data4[seq(3, nrow(data4), 3), ]

###### Rent forecast -----
install.packages("forecast")
install.packages("tseries")
library(forecast)
library(tseries)

#check for stationarity of the data
adf.test(data4$Rent)

#ARIMA model for rent  with mortgage rate as exogenous variable
fit <- auto.arima(data3$Rent, xreg = data3$mortagage.rate, seasonal=FALSE, approximation=FALSE, trace=TRUE)
summary(fit)
checkresiduals(fit)

## Forecast of the mortgage rate
#check for stationarity of the data
adf.test(data3$mortagage.rate)

# Fit an ARMA model to the historical mortgage rate data
fit2 <- auto.arima(data3$mortagage.rate, seasonal=FALSE, stepwise=TRUE, approximation=FALSE)
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

#Performance evaluation
library(ggplot2)
#out-of-sample evaluation
# Plot the forecasted rent values
plot(rent_forecast)

# Set the number of periods to forecast
n <- 12
# Split the data into training and test sets
train_end <- length(data4$Rent) - n
train_set <- data4$Rent[1:train_end]
test_set <- data4$Rent[(train_end + 1):length(data4$Rent)]

# Fit the ARIMA model on the training set
fit <- auto.arima(train_set)

# Forecast n periods ahead
rent_forecast <- forecast(fit, h = n)

# The forecast object contains point forecasts, upper and lower confidence intervals
# Extract the point forecasts
forecast_values <- rent_forecast$mean

# Compare the forecast to the actual values in the test set
comparison <- data.frame(Forecast = forecast_values, Actual = test_set)

# Calculate accuracy measures
accuracy_measures <- accuracy(forecast_values, test_set)

# Print the comparison and accuracy measures
print(comparison)
print(accuracy_measures)

# This assumes that your test_set is already a time series object with proper time attributes
forecast_series <- ts(forecast_values, start=start(test_set), frequency=frequency(test_set))

# Create a combined data frame for plotting
actual_df <- data.frame(Time = time(test_set), Rent = as.numeric(test_set), Type = "Actual")
forecast_df <- data.frame(Time = time(forecast_series), Rent = as.numeric(forecast_series), Type = "Forecasted")
all_data <- rbind(actual_df, forecast_df)

# Plot using ggplot2
ggplot(all_data, aes(x = Time, y = Rent, color = Type)) +
  geom_line() +
  labs(title = "Actual vs Forecasted Rent", x = "Time", y = "Rent") +
  scale_color_manual(values = c("Actual" = "blue", "Forecasted" = "red")) +
  theme_minimal()



##Try ecm using ecm package ##
install.packages("dplyr")
install.packages("lubridate")
install.packages("urca")
install.packages("ecm")
library(ecm)
library(urca)
library(dplyr)
library(lubridate)
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

######
# Convert to quarterly
######
#1. convert B10
#create empty df
ECM_Data_Q <- data.frame()
#convert to qurterly
ECM_Data$Date_Q <- quarter(ECM_Data$Date)
ECM_Data$Date_Y <- year(ECM_Data$Date)
ECM_Data_Q <- ECM_Data %>%
  group_by(Date_Q,Date_Y) %>%
  summarise(Q_OIL_CHF = mean(B10, na.rm = TRUE))
#order by date
ECM_Data_Q  <- ECM_Data_Q [order(ECM_Data_Q$Date_Y),]

#2. convert oil
#create empty df
ECM_Data_Q2 <- data.frame()
#convert to qurterly
ECM_Data$Date_Q <- quarter(ECM_Data$Date)
ECM_Data$Date_Y <- year(ECM_Data$Date)
ECM_Data_Q2 <- ECM_Data %>%
  group_by(Date_Q,Date_Y) %>%
  summarise(Q_OIL_CPI = mean(oil, na.rm = TRUE))
#order by date
ECM_Data_Q2  <- ECM_Data_Q2 [order(ECM_Data_Q2$Date_Y),]

#  happend the two data frames by columns
ECM_Data_Q <- cbind(ECM_Data_Q, ECM_Data_Q2)

#remove ECM_Data_Q2, quarterl_averages2
rm(ECM_Data_Q2, quarterly_averages)

######
# 3 calculate the growth rate of ECM_Data_Q$Q_OIL_CHF and ECM_Data_Q$Q_OIL_CPI
######
#calculate the growth rate of ECM_Data_Q$Q_OIL_CHF and ECM_Data_Q$Q_OIL_CPI
ECM_Data_Q$Q_OIL_CHF_R <- (ECM_Data_Q$Q_OIL_CHF - lag(ECM_Data_Q$Q_OIL_CHF,1))/lag(ECM_Data_Q$Q_OIL_CHF,1)
ECM_Data_Q$Q_OIL_CPI_R <- (ECM_Data_Q$Q_OIL_CPI - lag(ECM_Data_Q$Q_OIL_CPI,1))/lag(ECM_Data_Q$Q_OIL_CPI,1)
#remove the first line of ECM_Data_Q
ECM_Data_Q <- ECM_Data_Q[-1,]

#plot the growth rate of ECM_Data_Q$Q_OIL_CHF and ECM_Data_Q$Q_OIL_CPI
plot(ECM_Data_Q$Q_OIL_CHF_R, type = "l", col = "red")
lines(ECM_Data_Q$Q_OIL_CPI_R, type = "l", col = "blue")

######
# 4 perfom checks before ECM
######
#check for stationarity of the data
adf.test(ECM_Data_Q$Q_OIL_CHF_R)
adf.test(ECM_Data_Q$Q_OIL_CPI_R)
#check for cointegration between the two time series using urca package
test <- ca.jo(ECM_Data_Q[,c(7,8)], type = "trace", ecdet = "const", K = 2, spec = "transitory")

######
# 4 perfom the ecm
######

# generate RW
# Set the seed for reproducibility
set.seed(123)

# Number of periods
n_periods <- length(ECM_Data_Q$Q_OIL_CHF_R)

# Generate random error terms (assuming they follow a normal distribution)
# The standard deviation can be adjusted to reflect the volatility
error_terms <- rnorm(n_periods, mean = 0, sd = 1)

# Initialize the series, starting with an initial price (e.g., 100)
second <- rep(0, n_periods)

# Generate the random walk (without a drift)
for(i in 2:n_periods){
    second[i] <- second[i-1] + error_terms[i]
}

# Plot the series
plot(second, type = "l", main = "Simulated - Random Walk", xlab = "Time", ylab = "Price")

#fit the model ECM using CM_Data_Q$Q_OIL_CHF_R and a random walk and pass a data.frame
# Convertir xeq et xtr en data.frames
xeq <- data.frame(Q_OIL_CHF_R = ECM_Data_Q$Q_OIL_CHF_R)
xtr <- data.frame(second)

# Ajuster le modèle ECM
fit <- ecm(ECM_Data_Q$Q_OIL_CPI_R, xeq, xtr, includeIntercept = TRUE)

#plot the model
plot(fit)
#summary of the model
summary(fit)

######
# 5 forecast the ecm and do a spaghetti plot
######
#forecast the ecm
forecast(fit, h = 4)
#plot the forecast
plot(forecast(fit, h = 4))
#spaghetti plot
plot(forecast(fit, h = 4), include = 100)
