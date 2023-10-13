##import data in excell format from the first table only
install.packages("readxl")
install.packages("reshape2")
library(readxl)
data <- read_excel("CPI_2020.xlsx", sheet = 1, col_names = TRUE, skip = 1)

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
#exctracte column 2 and name it weight
weight <- data[,2]
#drop the first line
weight <- weight[-1,]
#remove first line and secdond column from data
data <- data[,-2]
#change the first index of the table to "Year"
data[1,1] <- "Year"
#Reshape the data so the years which is the first line are in one column
data <- t(data)
#make the first line the name of the columns
colnames(data) <- data[1,]
#remove the first line
data <- data[-1,]
#format the data to numeric
data <- as.numeric(data)
#format the first column of date to display a date from the excel way of counting
data[,1] <- as.Date(as.numeric(data[,1]), origin = "1899-12-30")


#ARIMA MODEL
install.packages("forecast")

