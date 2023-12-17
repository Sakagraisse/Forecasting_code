# Define a list of packages you need
packages <- c("readxl", "reshape2", "dplyr", "lubridate", "urca", "ecm", "forecast",
              "tseries", "zoo", "tempdisagg", "openxlsx", "whitestrap", "lmtest", "xtable", "car", "sandwich")

# Loop through the list and install any missing packages
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load the packages
library(car)
library(xtable)
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
library(tempdisagg)
library(openxlsx)
library(sandwich)


######
# clean space
######

rm(list = ls())
# Get the current working directory
current_directory <- getwd()

######
# Create main dataframe of interest
######

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


CPIs<- data[c("Year", "Total", "Housing.rental.1","Index.without.housing.rental","Petroleum.products","Index.without.petroleum.products")]
save(CPIs, file = "CPIs.RData")

save(weight, file = "weight.RData")





######
# Create dataframe of interest for the CPI minus oi minus rent
######

#load data : CPIs.RData
load("CPIs.RData")

######### weight data
dataw <- read_excel("wieght_data.xlsx", sheet = 1, col_names = TRUE)
CPIs$OIL <-CPIs$`Petroleum.products`
CPIs$Rent <- CPIs$`Housing.rental.1`
dataw <- dataw[,c(1:6)]

# Repeat each row in your_data 12 times
monthly_data <- dataw[rep(seq_len(nrow(dataw)), each = 12), ]
monthly_data$Totalw_o_r <- monthly_data$Total - monthly_data$Oil - monthly_data$Rent
# generate monthly date  and add it to the data
monthly_data$Date <- seq(as.Date("2000/1/1"), by = "month", length.out = nrow(monthly_data))

# have the same lenght for CPIs and weight
CPIs <- CPIs[205:489,]
monthly_data <- monthly_data[1:285,]
monthly_data <- monthly_data[,-1]
#renqme the columns
names(monthly_data) <- c( "W_total", "W_housing", "W_oil", "W_totalw_o", "W_totalw_r", "Totalw_o_r", "Year")
#remove first column
monthly_data <- monthly_data[,-1]
#merge the two data sets
CPIs <- merge(monthly_data, CPIs, by = "Year")

CPIs$our <- (CPIs$Total - CPIs$OIL*CPIs$W_oil/100 - CPIs$Rent*CPIs$W_housing/100)/CPIs$Totalw_o_r*100

#save the data
save(CPIs, file = "CPIs_double_minus.RData")