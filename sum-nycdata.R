wd1 <- "~/Dropbox/SAbook"

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


# Check correlations
cor1 <- cor(nycdat[, -c(1, 2)]) %>% round(., 1)
apply(cor1, 1, function(x) {
  cn1 <- colnames(cor1)[which(x > 0.6)]
  paste(cn1, collapse = ";")
})


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
pdf(file.path(wd1, "resoilplot.pdf"), height = 7, width = 8)
resoilplot
dev.off()


# Correlations
resoilC <- spread(resoil, cons, value)
cor(resoilC[, -1])



# Proportion plot

# Function to fix names
namefun <- function(names1) {
    # Fix underline
    names1 <- as.character(names1)
  names1 <- strsplit(names1, "\\_") %>% sapply(., function(x) paste(x, collapse = " "))

    names1[which(names1 == "OC")] <- "organic carbon"
    # capitalize
    #?
    names1

}




cons <- nycdat[, -c(1, 2)]
cons <- sweep(cons, 1, nycdat[, 2], "/")
prop2 <- apply(cons, 2, mean)

prop <- apply(cons, 2, mean) / mean(nycdat[, 2])
prop <- data.frame(names(prop), round(prop, 3), round(prop2, 3))
colnames(prop) <- c("Name", "prop", "prop2")
prop$Name <- namefun(prop$Name)

gprop <- ggplot(prop, aes(x = Name, y = prop2, fill = Name)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab(expression("Average proportion of total PM"[2.5])) + 
  scale_fill_grey() + 
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1, vjust = 0.5), 
        legend.position = "none") 

pdf(file.path(wd1, "nyc-prop.pdf"), height = 4, width = 4)
gprop
dev.off()
