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
library(dplyr)
library(lubridate)
## import the data ##
#import MCOILWTICO.csv
oil_price <- read.csv("MCOILWTICO.csv", header = TRUE, sep = ",")
#convert oil_price$Date to R format from YYYY.MM.DD to monthly format
oil_price$Date <- as.Date(oil_price$DATE, format = "%Y-%m-%d")
#import usdchf.csv
exchange_rate <- read.csv("usdchf.csv", header = TRUE, sep = ",")





#convert to qurterly
#create dates
oil_price$Date_Q <- quarter(oil_price$Date)
oil_price$Date_Y <- year(oil_price$Date)
quarterly_averages <- oil_price %>%
  group_by(Date_Q,Date_Y) %>%
  summarise(average_value = mean(MCOILWTICO, na.rm = TRUE))

quarterly_averages  <- quarterly_averages [order(quarterly_averages $Date_Y),]
quarterly_averages$Date_q <- seq(as.Date("1986-01-01"), by = "3 months", length.out = nrow(quarterly_averages))





#import us-dollar-swiss-franc-exchange-rate-historical-chart.csv
#exchange_rate <- read.csv("us-dollar-swiss-franc-exchange-rate-historical-chart.csv", header = TRUE, sep = ",")
#convert exchange_rate$Date to R format from DD.MM.YYYY to monthly format
#exchange_rate$Date <- as.Date(exchange_rate$date, format = "%d.%m.%Y")
#import MCOILWTICO.csv
oil_price <- read.csv("MCOILWTICO.csv", header = TRUE, sep = ",")
#convert oil_price$Date to R format from YYYY.MM.DD to monthly format
oil_price$Date <- as.Date(oil_price$DATE, format = "%Y.%m.%d")
# calculate the inflation rate of oil_price
# remove the first line of oil_price
oil_price_plus <- oil_price[-1,]
# remove the last line of oil_price
oil_price_1 <- oil_price[-nrow(oil_price),]
# calculate the inflation rate of oil_price
oil_price_plus$rate <- (oil_price_plus$MCOILWTICO - oil_price_1$MCOILWTICO)/oil_price_1$MCOILWTICO
#retreive heating oil in a df
heating_oil <- data.frame(Date = data$Year, oil = data$`Heating.oil`)
#remove date below 1986-02-01
heating_oil <- heating_oil[which(heating_oil$Date >= "1986-02-01"),]
#remove date above 2021-08-01
oil_price_plus <- oil_price_plus[which(heating_oil$Date <= "2021-08-01"),]


## Some checks ##
# check the lag lenght needed
#put togheter the two time series
dataset <- cbind(heating_oil$oil,oil_price_plus['rate'])
var_select_result <- VARselect(dataset, lag.max = 10, type = "const")
var_select_result$criteria
# check for cointegration between the two time series using urca package
test <- ca.jo(dataset, type = "trace", ecdet = "const", K = 2, spec = "transitory")
#fit the model ECM
xeq <- xtr <- oil_price_plus['rate']
fit <- ecm(heating_oil$oil,xeq,xtr,includeIntercept = TRUE)
#plot the model
plot(fit)
#summary of the model
summary(fit)
