
# Load libraries
library(handles)
library(share)
library(dplyr)
library(ggplot2)
library(tidyr)


# Load data
data(consConc)
data(monitorsNE)

monNY <- filter(monitorsNE, Latitude > 40.5 & Latitude < 41 & Longitude > -74 & Longitude < -73.5)
monNY <- filter(monitorsNE, monitor == "36081.0124")

# verified from CSN website
nycdat <- consConc[["36081.0124"]]

write.csv(nycdat, file = "nycdat.csv", row.names = F)
