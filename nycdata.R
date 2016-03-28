
# Load libraries
library(share)
library(dplyr)
library(tidyr)


# Load data
data(consConc)
data(monitorsNE)

monNY <- filter(monitorsNE, Latitude > 40.5 & Latitude < 41 & Longitude > -74 & Longitude < -73.5)
monNY <- filter(monitorsNE, monitor == "36081.0124")

# verified from CSN website
nycdat <- consConc[["36081.0124"]]




###################
# Find time period

# 2003 - 2006
years <- substr(nycdat[, 1], 1, 4) %>% as.numeric()
keeps <- years <= 2005 & years >= 2003
nycdat <- nycdat[keeps, ]



#########
# Bad dates
# Remove bad dates
dates <- c("2006-01-05", "2006-01-20", "2005-02-09") %>% as.Date()
# Remove days around 4th of July, New Years
month <- substr(nycdat[, 1], 6, 7) %>% as.numeric()
day <- substr(nycdat[, 1], 9, 10) %>% as.numeric()
keeps <- !(nycdat$Date %in% dates | (month == 1 & day == 1) |
  (month == 12 & day == 31) | (month == 7 & day == 4) |
  (month == 7 & day == 5) | (month == 7 & day == 3) |
  (month == 11 & day == 11))
nycdat <- nycdat[keeps, ]







write.csv(nycdat, file = "nycdat.csv", row.names = F)

# Create uncertainties
P <- ncol(nycdat) - 1
unc <- matrix(rep(1, nrow(nycdat) * P), ncol = P)
unc <- data.frame(nycdat$Date, unc)
colnames(unc) <- colnames(nycdat)
write.csv(unc, file = "nycunc.csv", row.names = F)




