### mortgage

#import excel file with mortgage rate anc create quarterly data
Mortgage <- read_excel("mortgage_rate_c.xlsx", sheet = 1, col_names = TRUE)
#rename first column "mortgage rate"
colnames(Mortgage)[1] <- "mortgage_rate"
#rename second column "date"
colnames(Mortgage)[2] <- "date"
Mortgage <- Mortgage[order(Mortgage$date, decreasing = FALSE),]
length(Mortgage$mortgage_rate)

# Répéter les taux trimestriels pour chaque mois dans l'année
taux_mensuels <- rep(Mortgage$mortgage_rate, each = 3)
mortgage_rate_monthly <- ts(taux_mensuels, start = c(2008,9), frequency = 12)


### Rent
rental <- CPIs
#remove data before 2008-07-01
CPIs_trunk <- rental[rental$Year >= as.Date("2008-09-01"),]
#keep only rent
#rental <- as.numeric(CPIs_trunk$Housing.rental.1)
#keep only rent
rental <- CPIs_trunk$Housing.rental.1
#convert to ts
rental <- ts(rental, frequency = 12, start = c(2008,9))
#convert to quarterly data
quarterly_series <- aggregate(rental, nfrequency = 4, FUN = mean)

rental <- as.numeric(rental)
#create data frame with rent and mortgage monthly
length(mortgage_rate_monthly)
length(rental)
length(CPIs_trunk$Year)
mortgage_rate_monthly <- mortgage_rate_monthly[c(1:181)]
Rent_fore <- data.frame(CPIs_trunk$Year, rental, mortgage_rate_monthly)

#save the data
save(Rent_fore, file = "Rent_fore.RData")

## create quarterly data for rental by averaging the monthly data
length(Mortgage$mortgage_rate)
length(quarterly_series)

mortgage_rate_q <- Mortgage$mortgage_rate
mortgage_rate_q <- mortgage_rate_q[c(1:60)]
quarterly_series <- as.numeric(quarterly_series)

Rent_fore_q <- data.frame(mortgage_rate_q, quarterly_series)
#create quarterly dates colum
Rent_fore_q$Date <- seq(as.Date("2008-09-01"), as.Date("2023-06-01"), by = "3 month")
#save the data
save(Rent_fore_q, file = "Rent_fore_q.RData")