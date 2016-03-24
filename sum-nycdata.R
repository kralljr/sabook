
# Load libraries
library(handles)
library(share)
library(dplyr)
library(ggplot2)
library(tidyr)



# Load data
nycdat <- read.csv("nycdat.csv", stringsAsFactors = F)
nycdat[, 1] <- as.Date(nycdat[, 1])


# Check differences
dates <- nycdat$Date
summary(dates)
diff1 <- diff(dates)
sum1 <- summary(as.numeric(diff1))
sum1["Median"]
length(dates)


sulf <- select(nycdat, Date, ammonium_ion, sulfate)
colnames(sulf)[2 : 3] <- c("Ammonium ion", "Sulfate")
sulf <- gather(sulf, cons, value, -Date)

size1 <- 18
sulfplot <- ggplot(sulf, aes(x = Date, y = value)) + geom_line() + 
  ylab(expression(paste("Concentration (", mu, "g/m"^3, ")"))) +
  xlab("") + theme_bw() + 
  theme(text = element_text(size = size1)) +
  facet_wrap(~cons, ncol = 1, scales = "free")
pdf("sulfplot.pdf")
sulfplot
dev.off()



