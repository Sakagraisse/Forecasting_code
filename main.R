# Read Data ---------------------------------------------------------------

#clean space
rm(list = ls())
# ##import data in excell format from the first table only
install.packages("readxl")
install.packages("reshape2")

library(readxl)

rm(list = ls())
data <- read_excel("~/Desktop/Econ Forcasting/CPI_Forecast/CPI_2020.xlsx", sheet = 1, col_names = TRUE, skip = 1)
#check which type is data
class(data)

#Remove the first 5 columns
data <- data[,12:479]

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

## data cleaning end ##


#Plots ----------
# plot the column "Heating oil" against the column "Year"
plot(data$Year, data$`Heating.oil`, type = "l", col = "red", xlab = "Year", ylab = "Heating oil", main = "Heating oil against the year")
# plot the column "Fuel" against the column "Year"
plot(data$Year, data$Fuel, type = "l", col = "blue", xlab = "Year", ylab = "Fuel", main = "Fuel against the year")
# plot the column “Total.1" against the column "Year"
plot(data$Year, data$Total.1, type = "l", col = "green", xlab = "Year", ylab = "Total.1", main = "Total.1 against the year")

#VECM---------
install.packages("vars")
install.packages("urca", dependencies = TRUE)
install.packages("vars", dependencies = TRUE)
# Load the required libraries
library(urca)
library(vars)

# Assuming you have a data frame 'data' with the 'Heating.oil' column
# Create a time series object from the 'Heating.oil' column
heating_oil_series <- ts(data$Heating.oil)

# Perform the Johansen test for cointegration for the 'Heating.oil' variable
johansen_test_result <- ca.jo(heating_oil_series, type = "eigen", ecdet = "none", K = 2)

# Summary of the Johansen test
summary(johansen_test_result)

# Determine the number of cointegration relationships (use eigenvalues)
cointegration_rank <- summary(johansen_test_result)$V[1, 1]



## Ploting some of the datas end ##

##Fit ARIMA process to the data##
# install the package forcast and tseries
install.packages("forecast")
install.packages("tseries")
# load the package
library(forecast)
library(tseries)

# fit the ARIMA process to the column "Heating oil"-----
fit1 <- auto.arima(data$`Heating.oil`)

# manual fit
test <- adf.test(data$`Heating.oil`)
diff_data <- diff(data$`Heating.oil`, differences = 1)
#plot diff data
plot(diff_data)
test <- adf.test(diff_data)

acf(diff_data)
pacf(diff_data)

fit <- arima(your_time_series_data, order=c(p,d,q))

checkresiduals(fit)

forecast_results <- forecast(fit, h=number_of_periods_to_forecast)
plot(forecast_results)

#use package ecm for error correction model for data$`Heating.oil`
install.packages("ecm")
library(ecm)
#fit the model
fit <- ecm(data$`Heating.oil`)
#plot the model
plot(fit)
# Read Data ---------------------------------------------------------------

#clean space
rm(list = ls())
# ##import data in excell format from the first table only
install.packages("readxl")
install.packages("reshape2")

library(readxl)

rm(list = ls())
data <- read_excel("~/Desktop/Econ Forcasting/CPI_Forecast/CPI_2020.xlsx", sheet = 1, col_names = TRUE, skip = 1)
#check which type is data
class(data)

#Remove the first 5 columns
data <- data[,12:479]

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

## data cleaning end ##


#Plots ----------
# plot the column "Heating oil" against the column "Year"
plot(data$Year, data$`Heating.oil`, type = "l", col = "red", xlab = "Year", ylab = "Heating oil", main = "Heating oil against the year")
# plot the column "Fuel" against the column "Year"
plot(data$Year, data$Fuel, type = "l", col = "blue", xlab = "Year", ylab = "Fuel", main = "Fuel against the year")
# plot the column “Total.1" against the column "Year"
plot(data$Year, data$Total.1, type = "l", col = "green", xlab = "Year", ylab = "Total.1", main = "Total.1 against the year")

#VECM---------
install.packages("vars")
install.packages("urca", dependencies = TRUE)
install.packages("vars", dependencies = TRUE)
# Load the required libraries
library(urca)
library(vars)

# Assuming you have a data frame 'data' with the 'Heating.oil' column
# Create a time series object from the 'Heating.oil' column
heating_oil_series <- ts(data$Heating.oil)

# Perform the Johansen test for cointegration for the 'Heating.oil' variable
johansen_test_result <- ca.jo(heating_oil_series, type = "eigen", ecdet = "none", K = 2)

# Summary of the Johansen test
summary(johansen_test_result)

# Determine the number of cointegration relationships (use eigenvalues)
cointegration_rank <- summary(johansen_test_result)$V[1, 1]



## Ploting some of the datas end ##

##Fit ARIMA process to the data##
# install the package forcast and tseries
install.packages("forecast")
install.packages("tseries")
# load the package
library(forecast)
library(tseries)

# fit the ARIMA process to the column "Heating oil"-----
fit1 <- auto.arima(data$`Heating.oil`)

# manual fit
test <- adf.test(data$`Heating.oil`)
diff_data <- diff(data$`Heating.oil`, differences = 1)
#plot diff data
plot(diff_data)
test <- adf.test(diff_data)

acf(diff_data)
pacf(diff_data)

fit <- arima(your_time_series_data, order=c(p,d,q))

checkresiduals(fit)

forecast_results <- forecast(fit, h=number_of_periods_to_forecast)
plot(forecast_results)

#use package ecm for error correction model for data$`Heating.oil`
install.packages("ecm")
library(ecm)
#fit the model
fit <- ecm(data$`Heating.oil`)
#plot the model
plot(fit)

#convert oil to quarterly data
oil <- ts(data$`Heating.oil`, start = c(1983,1), frequency = 4)


#### mortgage rate construction data monthly------

#import excel file with mortgage rate
data <- read_excel("~/Desktop/Econ Forcasting/CPI_Forecast/CPI_2020.xlsx", sheet = 1, col_names = TRUE, skip = 1)
data2 <- read_excel("/Users/abigailvasquez/Desktop/Econ Forcasting/CPI_Forecast/mortgage_rate_c.xlsx", sheet = 1, col_names = TRUE, skip = 1)

#rename first column "mortgage rate"
colnames(data2)[1] <- "mortgage.rate"
#rename second column "date"
colnames(data2)[2] <- "date"

#order date
data2 <- data2[order(data2$date),]

#create monthly data for mortgage rate in duplicating the data 3 times to have monthly value instead of quarterly
data2 <- data2[rep(row.names(data2), each = 3), ]

#create dates
data2$dates <- seq(as.Date("2008-09-10"), by = "month", length.out = nrow(data2))

#delete date column
data2 <- data2[,-2]

#keep data2 until line 156
data2 <- data2[1:156,]


###### construction mortgage + rent for ARIMA with mortgage rate lag 4 for exogenous variable
#create data for "rent" from data starting at lign 309 to 462 (to merge with mortgage rate)
data4 <- data[309:464,]
data2$rent <- data4$Rent

###### essai ARIMA
#arima model for rent
install.packages("forecast")
library(forecast)

fit3 <- auto.arima(data2$rent, approximation=FALSE, trace=TRUE, ic="aic")

#forecast for rent
forecast_results <- forecast(fit3, h=10)
plot(forecast_results)

#arima  model for rent  with mortgage rate  as exogenous variable
fit4 <- auto.arima(data2$rent, xreg = data2$mortgage.rate, approximation=FALSE, trace=TRUE, ic="aic")
forecast_results2 <- forecast(fit3, h=10)
plot(forecast_results2)
checkresiduals (fit4)

#arima  model for rent  with mortgage rate  as exogenous variable with lag 4 for mortgage rata
fit5 <- auto.arima(data2$rent, xreg = data2$mortgage.rate, approximation=FALSE, trace=TRUE, ic="aic")
forecast_results3 <- forecast(fit3, h=20)
plot(forecast_results3)
checkresiduals (fit5)
