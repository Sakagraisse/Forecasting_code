
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
# clean space
######

rm(list = ls())
# Get the current working directory
current_directory <- getwd()

######
# DATA importation
######

#load data : CPIs.RData
load("arima_forecast.RData")
load("arimaX_forecast.RData")
load("ecm_forecast.RData")

#load data : CPIs.RData
load("CPIs_double_minus.RData")

######
# Load series of interest
######

weight_OIL <- c(CPIs$W_oil,rep(CPIs$W_oil[length(CPIs$W_oil)], 36))
weight_Rent <- c(CPIs$W_housing,rep(CPIs$W_housing[length(CPIs$W_housing)], 36))
weight_Rent_OIL <- c(CPIs$Totalw_o_r,rep(CPIs$Totalw_o_r[length(CPIs$Totalw_o_r)], 36))

#compute each contribution
# contribution of oil
contri_oil <- weight_OIL * ecm_forecast/100
contri_oil <-ts (contri_oil, start = c(2000,1), frequency = 12)

#convert to quarterly
contri_oil <- aggregate(contri_oil,4,mean)

#convert monthly weight to quarterly weight for Rent
new_weight<- ts(weight_Rent, start = c(2000,1), frequency = 12)
new_weight <- aggregate(new_weight,4, mean)

#contribution of rent
#Add 35 NA on top of arimaX_forecast to match length of new_weight and oth contribution
contri_rent <- c(rep(NA, 35,),arimaX_forecast)
contri_rent <- contri_rent * new_weight/100
contri_rent <-ts (contri_rent, start = c(2000,1), frequency = 4)

#contribution of CPI minus rent and oil
contri_rent_oil <- weight_Rent_OIL * arima_forecast/100
contri_rent_oil <-ts (contri_rent_oil, start = c(2000,1), frequency = 12)
contri_rent_oil <- aggregate(contri_rent_oil,4,mean)

# aggregate the 3 contributions
aggregate <- as.numeric(contri_oil + contri_rent + contri_rent_oil)
# create the YoY series
aggregateYoY <- (aggregate / lag(aggregate, 4) - 1) * 100
aggregateYoY <- ts(aggregateYoY, start = c(2000,1), frequency = 4)

#plot the aggregate YoY
to_plot <- tail(aggregateYoY, 68)
plot(to_plot, type = "l", col = "blue", xlab = "Year", ylab = "Inflation", main = "CPIs YoY")
to_plot_2 <- tail(aggregateYoY, 13)
lines(to_plot_2, col = "red")


rm(to_plot, to_plot_2)


######
# Out of sample tests comparative
######

#aggregate the 3 out of sample forecast
# add 35 row on top of  arimaX_out
to_add <- data.frame(matrix(NA, nrow = 35, ncol = ncol(arimaX_out)))
colnames(to_add) <- colnames(arimaX_out)
arimaX_out <- rbind(to_add, arimaX_out)
total_out_q <- data.frame(matrix(NA, nrow = (nrow(arimaX_out))))

for(i in seq(1, ncol(arima_out), 1)){
    #select column i of arima_out
    
    temp1 <- as.numeric(arima_out[,i])
    temp1 <- temp1 * CPIs$Totalw_o_r /100
    temp2 <- as.numeric(ecm_out[,i])
    temp2 <- temp2 * CPIs$W_oil /100
    temp1 <- ts(temp1, start = c(2000,1), frequency = 12)
    temp2 <- ts(temp2, start = c(2000,1), frequency = 12)
    temp1 <- aggregate(temp1,4,mean)
    temp2 <- aggregate(temp2,4,mean)
    test <- temp1 + temp2
    test <- as.numeric(test)
    total_out_q <- data.frame(total_out_q, test)
}
total_out_q <- total_out_q[,-1]
#keep one column over 3
total_out_q <- total_out_q[,seq(1, ncol(total_out_q), 3)]
total_out_q <- total_out_q[,(ncol(total_out_q) - ncol(arimaX_out) +1):ncol(total_out_q)]
weight_rent <- CPIs$W_housing
weight_rent <- ts(weight_rent, start = c(2000,1), frequency = 12)
weight_rent <- aggregate(weight_rent,4,mean)
total_out_q <- total_out_q + arimaX_out*as.numeric(weight_rent)/100

#repeat aggregate length(total_out_q) times
to_sub  <- matrix(aggregate[1:95], nrow = length(aggregate[1:95]), ncol = ncol(total_out_q), byrow = FALSE)
diff <- total_out_q - to_sub
error <- matrix(NA, nrow = 12)
for(i in 1:(ncol(diff))){
    temp <- diff[(i+55-1):(i+66-1),i]
    error <- data.frame(error, temp)
}
Error <- error[,-1]

#remove last line
Error_ag <- rowMeans(Error, na.rm = TRUE)
Error_sq <- rowMeans(Error^2, na.rm = TRUE)

rm()

### out of sample forecast for benchmark model

out_of_sample_b <- data.frame(matrix(ncol = 1, nrow = 12))
mean_of_fit_b <- data.frame(matrix(ncol = 1, nrow = nrow(total_out_q)))
end_b <- nrow(total_out_q)
end_b <- end_b - 12

#forecast from line 55 to the end of date benchmark model
for (i in 55:(end_b+1)){
    #store the truncated serie for fitting
    temporary <- aggregate[1:(i-1)]
    temporary <- ts(temporary, start = c(2000,1), frequency = 4)
    #fit arima model on the first i-1 observations
    fit <- arima(temporary, order = c(1,1,0))
    fore <- forecast(fit, h = 12)
    #save the mean of the fit
    to_save <- c(temporary, fore$mean, rep(NA, (end_b - i + 1)))
    mean_of_fit_b <- data.frame(mean_of_fit_b, to_save)
    #save the error
    to_save_2 <- as.numeric(fore$mean)  - as.numeric(aggregate[(i):(i+11)])
    out_of_sample_b <- data.frame(out_of_sample_b, as.numeric(to_save_2))
}

## remove place holder columns
mean_of_fit_b <- mean_of_fit_b[,-1]
Error_b <- out_of_sample_b[,-1]

#Prepare series for predictive rsquared and Diebold Mariano test
Error_b_ag <- rowMeans(Error_b, na.rm = TRUE)
Error_b_sq <- rowMeans(Error_b^2, na.rm = TRUE)


rm(end_b, end_month, end_year, fore, i, temporary, to_save, to_save_2)

######
# Predictive R squared
######

#calculate the predictive R squared
MSFE_pred_by_time <- 1 - (Error_sq/Error_b_sq)


pdf(paste(getwd(), "/Graphs/double minus/predictive_r_double_minus.pdf", sep=""), width = 13, height = 5)

barplot(MSFE_pred_by_time,names.arg = 1:12,main = "Predictive R_Squared by period" )

dev.off()


######
# Plot spaghetti graph
######
temp <- aggregate[1:95]
cpi_without_approx <- (temp / lag(temp, 4) - 1) * 100
cpi_without_approx <- cpi_without_approx[5:length(cpi_without_approx)]
cpi_without_approx <- ts(cpi_without_approx, start = c(2001,1), frequency = 4)
cpi_without_approx <- tail(cpi_without_approx, 56)
pdf(paste(getwd(), "/Graphs/double minus/spag.pdf", sep=""))
dev.off()


plot(cpi_without_approx , type = "l", col = "red", xlab = "Year", ylab = "Inflation", main = "Spaghetti graph CPIs YoY without rent and without petroleum products")
legend("topleft",           # Position of the legend
       legend = c("ARIMA(3,0,0)", "ARIMA(1,0,0)"),  # Legend labels
       col = c("Blue", "Green"),       # Colors
       lty = 1)

for (i in seq(from = 1, to = 30, by = 6)){
        #keep the i'th column of mean_of_fit
        print <- mean_of_fit[,i]
        #calculate the YoY
        print <- (print / lag(print ,4) - 1) * 100

        print[1:(19+i-2)] <- NA
        #create a time series
        print <- ts(print, start = c(2008,4), frequency = 4)
        lines(print, col="blue")

        #same for benchmark model
        #uncomment to plot it
        print <- mean_of_fit_b[,i]
        #calculate the YoY
        print <- (print / lag(print ,4) - 1) * 100
        print[1:(19+i -2)] <- NA
        #create a time series
        print <- ts(print, start = c(2008,4), frequency = 4)
        #lines(print, col="green")

}


#####
# Out of sample tests Diebold
######
Diebold_DM<- c()
Diebold_p<- c()
for(i in 1:12){
    Diebold_DM[i] <- dm.test(Error_ag, Error_b_ag, alternative = "two.sided", h = i, power = 2,varestimator = "bartlett")$statistic
    Diebold_p[i] <- dm.test(Error_ag, Error_b_ag, alternative = "two.sided", h = i, power = 2,varestimator = "bartlett")$p.value
}

barplot(Diebold_DM,names.arg = 1:12,main = "Diebold Mariano test by period" )
barplot(Diebold_p,names.arg = 1:12,main = "Diebold Mariano test by period" )