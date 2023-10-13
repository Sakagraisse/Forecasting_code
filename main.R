#import data in excell format from the first table only
install.packages("readxl")
library(readxl)
data <- read_excel("CPI_2020.xlsx", sheet = 1, col_names = TRUE, skip = 1)

#Remove the first 5 columns
data <- data[,9:479]
#Remove the
#Extract the second column
data2 <- data[,2]
kjgkktfkuzflzf