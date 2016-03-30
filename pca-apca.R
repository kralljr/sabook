wd1 <- "~/Dropbox/SAbook"

# Load numfact functions
source("numfact.R")

# Load libraries
library(handles)
library(share)
library(dplyr)
library(ggplot2)
library(tidyr)



# Load data
nycdat <- read.csv("nycdat.csv", stringsAsFactors = F)
nycdat[, 1] <- as.Date(nycdat[, 1])


# Scree plot
pr1 <- prcomp(nycdat[, -c(1, 2)], scale = T)
sdev <- data.frame(seq(1, length(pr1$sdev)), pr1$sdev)
colnames(sdev) <- c("number", "sdev")
scree1 <- ggplot(sdev, aes(x = number, y = sdev)) + 
  geom_point() + geom_line() +
  geom_hline(yintercept = 1, linetype = 2, colour = "grey") + 
  ylab("PC standard deviations") + 
  xlab("Number of PCs") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank())  
pdf(file.path(wd1,"screeplot.pdf"), height = 3, width = 6)
scree1
dev.off()



# Number of sources
nf <- numfact(nycdat[, -c(1, 2)], seed1 = 895833)


# APCA
apca1 <- apca(nycdat[, -2], nycdat[, 2], nsources = nf)
ncons <- ncol(nycdat) - 2


####
# PROFILES
profs <- apca1$vmax$loadings[1 : ncons, ]
# Make more positive
profs <- sweep(profs, 2, sign(colSums(profs)), "*")

# Fix columns
profs <- data.frame(rownames(profs), profs)
colnames(profs)[1] <- "cons"
profs <- gather(profs, source, value, -cons)

###
# CONCENTRATIONS
conc <- data.frame(nycdat[, 1], apca1$conc)
colnames(conc)[1] <- "Date"
# fix
conc <- gather(conc, source, value, -Date)


###
# Plots of APCA results
profsplot <- ggplot(profs, aes(x = cons, y = value, fill = value)) + 
    geom_bar(stat="identity") +
    xlab("") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust= 0.4)) + 
    facet_wrap(~ source, ncol =3)
pdf(file.path(wd1,"apca-profplot.pdf"))
profsplot
dev.off()


concplot <- ggplot(conc, aes(x = Date, y = value)) + geom_line() + 
 geom_point() + facet_wrap(~ source, ncol = 3) 
pdf(file.path(wd1, "apca-concplot.pdf"))
concplot
dev.off()
