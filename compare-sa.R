wd1 <- "~/Dropbox/SAbook"

# load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(handles)
library(xtable)
library(lubridate)

# Load data
nycdat <- read.csv("nycdat.csv", stringsAsFactors = F)
nycdat[, 1] <- as.Date(nycdat[, 1])
P <- ncol(nycdat) - 2
T <- nrow(nycdat)
L <- 6

# Get PMF Concentrations
skips <- 4 * 2 + T + 1
conc <- read.table(file.path(wd1, "nycrun", "nycrun_contributions.txt"), skip = skips, nrows = T, sep = "\t", header = F)[, -1]
colnames(conc) <- c("Date", "Sec Sulf", "Salt", "Burning", "Resoil", "Sec nitrate", "Mobile")
conc$Date <- as.Date(conc$Date, format = "%m/%d/%y")

# Run APCA
apca1 <- apca(nycdat[, -2], nycdat[, 2], nsources = L)

# Check correlation between PMF and APCA
cor(conc[, -1], apca1$conc) %>% round(., 2)

# Reorganize APCA
aconc <- apca1$conc
Date <- conc$Date
aconc <- data.frame(Date, aconc)
skeep <- c("Sec Sulf", "Salt", "Mobile")
colnames(aconc)[(1 + c(2, 3, 6))] <- skeep 

# Reshape
conc <- mutate(conc, Type = "PMF")
aconc <- mutate(aconc, Type = "APCA")
conc <- gather(conc, source, value, -Date, -Type)
aconc <- gather(aconc, source, value, -Date, -Type)
conccomp <- full_join(conc, aconc)


# Plot
conccomp2 <- dplyr::filter(conccomp, source %in% skeep[c(1, 3)], Date < as.Date("2005-01-01"))
conccomp2$source <- factor(conccomp2$source, levels = skeep[c(1, 3)], labels = c("Secondary sulfate", "Mobile-related"))
cols <- c("grey50", 1)
concplot <- ggplot(conccomp2, aes(x = Date, y = value, color = Type)) + 
  geom_point() + geom_line(aes(linetype = Type)) +
  ylab(expression(paste("PM"[2.5]," concentration (", mu, "g/m"^3, ")"))) +
  xlab("") +
#  scale_color_brewer(palette = "Dark2", name = "") +
  scale_color_manual(values = cols, name = "type") +
  scale_linetype_discrete(name = "type") +
  theme_bw() +
  theme(text = element_text(size = 18), legend.position = "top",
        legend.title = element_blank()) + 
  facet_wrap(~ source, ncol = 1, scale = "free")

pdf(file.path(wd1, "compare-plot.pdf"), height = 5, width = 8)
concplot
dev.off()


gconc <- group_by(conccomp2, Type, source)
sum1 <- summarise_each(gconc, funs(mean, sd), value) 
sum1 <- mutate(sum1, mean = paste0(round(mean, 2)," (",  sd = round(sd, 2), ")"))
sum1 <- select(sum1, -sd)
sum1 <- spread(sum1, Type, mean)
xtable(sum1)


# Check mobile weekend/weekday
# Is not sun/sat
weekday <- ifelse(wday(conccomp2$Date) %in% c(1, 7), "weekend", "weekday")
conccomp2 <- data.frame(weekday, conccomp2)
gconc <- group_by(conccomp2, Type, source, weekday )
sum1 <- summarise_each(gconc, funs(mean, sd), value) 
sum1 <- mutate(sum1, mean = paste0(round(mean, 2)," (",  sd = round(sd, 2), ")"))
sum1 <- select(sum1, -sd)
sum1 <- spread(sum1, Type, mean)
sum1

conccomp3 <- dplyr::filter(conccomp2, source == "Mobile-related")
ggplot(conccomp3, aes( x = Date, y = value, color = weekday)) + 
  geom_point() +
  facet_wrap(Type ~ source)
