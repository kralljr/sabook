wd1 <- "~/Dropbox/SAbook"

# load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

# Load data
nycdat <- read.csv("nycdat.csv", stringsAsFactors = F)
nycdat[, 1] <- as.Date(nycdat[, 1])
P <- ncol(nycdat) - 2
T <- nrow(nycdat)
L <- 6




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








# Profile results
#skips <- (4 + P) * 2 + 5
#skips <- 5
skips <- 10 + P
prof <- read.table(file.path(wd1, "nycrun", "nycrun_profiles.txt"), skip = skips, nrows = P, sep = "\t", header = F)[, -1]

colSums(prof[, -1])

# Check pm2.5 vs. sum
rs <- rowSums(nycdat[, -c(1, 2)])
wh1 <- which(nycdat[, 2] < rs)
data.frame(nycdat[wh1, 2], rs[wh1])

# Rescale based on total
#prof2 <- prof[, -1]
#prof2 <- sweep(prof2, 2, colSums(prof2), "/")
#colSums(prof2)
#prof2 <- data.frame(prof[, 1], prof2)
prof2 <- prof
colnames(prof2) <- c("Name", paste("Source", seq(1, L)))
prof2 <- gather(prof2, source, value, -Name)
prof2$Name <- namefun(prof2$Name)


# 1: secondary sulfate
# 2: soil?
# 3: gasoline?  burning?
# 4: residual oil/combustion
# 5: secondary nitrate
# 6: diesel/mobile

# Plot profiles
scale1 <- rep(brewer.pal(8, "Dark2"), length = P )
gprof <- ggplot(prof2, aes(x = Name, y = value, fill = Name)) + 
#  scale_fill_manual(values = scale1) + 
  theme_bw() + 
  scale_fill_grey() + 
  geom_bar(stat="identity") +
  xlab("") + ylab("Proportion of total constituent") +  
  theme(axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        axis.text.x = element_text(angle = 90, 
          vjust = 0.5, hjust = 1, size = 12),
        legend.position = "none") +
  facet_wrap(~ source)

pdf(file.path(wd1, "pmf-prof.pdf"), height = 7, width = 9)
gprof
dev.off()





# Concentrations
skips <- 4 * 2 + T + 1
conc <- read.table(file.path(wd1, "nycrun", "nycrun_contributions.txt"), skip = skips, nrows = T, sep = "\t", header = F)[, -1]

colnames(conc) <- c("Date", paste("Source", seq(1, L)))
conc <- gather(conc, source, value, -Date)
conc$Date <- as.Date(conc$Date, format = "%m/%d/%y")

# Concentration plots
conc2 <- filter(conc, Date < as.Date("2005-01-01"))
gconc <- ggplot(conc2, aes(x = Date, y = value)) + geom_line() +
  geom_point() +
  theme_bw() + 
  scale_fill_grey() + 
  xlab("") + 
  ylab(expression(paste("PM"[2.5]," concentration (", mu, "g/m"^3, ")"))) +
    theme(text = element_text(size = 18)) + 
    facet_wrap(~ source, scale = "free", ncol = 1)

    
pdf(file.path(wd1, "pmf-conc.pdf"), height = 10)
gconc
dev.off()
