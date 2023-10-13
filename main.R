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
#change format of table to long table
library(reshape2)
data <- melt(data, id.vars = "Country Name", variable.name = "Year", value.name = "CPI")

#ARIMA MODEL
install.packages("forecast")

