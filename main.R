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
#Mortgage rate quarterly

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
data2 <- data2[order(data2$date),]

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
#take the first difference of the data
#data4$Rent_diff <- c(NA, diff(data4$Rent, differences = 1))
#check for stationarity of the data
#adf.test(data4$Rent_diff)


# Difference the data to achieve stationarity
#data2$rent_diff <- c(NA, diff(data2$rent, differences = 1))
# Create the differenced rent time series excluding the first NA observation
#rent_diff <- diff(data2$rent, differences = 1)
# Run your Augmented Dickey-Fuller test again on the differenced rent
#adf.test(rent_diff)
# Difference the mortgage rate series and remove the first NA value
#mortgage_rate_diff <- diff(data2$mortgage.rate, differences = 1)
#mortgage_diff <- diff(data2$mortgage.rate, differences = 1)

#ARIMA model for rent  with mortgage rate as exogenous variable
fit <- auto.arima(data3$Rent, xreg = data3$mortagage.rate, seasonal=FALSE, approximation=FALSE, trace=TRUE)
summary(fit)
checkresiduals(fit)


## Forecast of the mortgage rate
# Create a time series object of the mortgage rate data
#mortgage_rates_ts <- ts(data2$mortgage.rate, start=c(2008, 9), frequency=12)
adf.test(data3$mortagage.rate)
#mortgage_diff <- diff(mortgage_rates_ts, differences = 1)
#adf.test(mortgage_diff)

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

