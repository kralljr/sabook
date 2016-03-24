
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


resoil <- select(nycdat, Date, nickel, vanadium, lead)
resoil <- gather(resoil, cons, value, -Date)
resoil$cons <- factor(resoil$cons, levels = c("nickel", "vanadium", "lead"), labels = c("Nickel", "Vanadium", "Lead"))
#resoil <- filter(resoil, Date < as.Date("2006-01-01"))


size1 <- 18
resoilplot <- ggplot(resoil, aes(x = Date, y = value)) + 
  geom_line() + 
  ylab(expression(paste("Concentration (", mu, "g/m"^3, ")"))) +
  xlab("") + theme_bw() + 
  theme(text = element_text(size = size1)) +
  facet_wrap(~cons, ncol = 1, scales = "free")
pdf("resoilplot.pdf", height = 7, width = 8)
resoilplot
dev.off()


# Correlations
resoilC <- spread(resoil, cons, value)
cor(resoilC[, -1])
