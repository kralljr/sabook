# Working directory
wd1 <- "~/Dropbox/SAbook"
fp1 <- file.path(wd1, "nycrun")

# file
file1 <- file.path(fp1, "nycrun_BaseErrorEstimationSummary.txt")

# Load data
nycdat <- read.csv("nycdat.csv", stringsAsFactors = F)
nycdat[, 1] <- as.Date(nycdat[, 1])


# load libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Fix column names
cn1 <- read.table(file1, sep = "\t", skip = 13, nrow = 1)[, 1 : 7]
cn1 <- as.matrix(cn1)[1, ] %>% as.character()
cn1 <- sapply(cn1, function(x) {
  x <- strsplit(x, " ")
  paste(x[[1]], collapse = "")}) 


# Get data for each factor
skip1 <- 14
P <- 25
for(i in 1 : 6) {
  dat1 <- read.table(file1, sep = "\t", skip = skip1, nrows = P)[, 1 : 7]
  colnames(dat1) <- cn1
  dat1 <- mutate(dat1, Source = paste("Source", i)) 
  if(i > 1) {
    datall <- full_join(dat1, datall)
  } else{
    datall <- dat1
  }
  skip1 <- skip1 + P + 3
}


datall <- filter(datall, Species != "PM25_SPEC")
lowlim <- function(x, lim = 10^(-4)) {
  x[which(x < lim)] <- -Inf
  x
}
datall2 <- mutate(datall, BS25th = lowlim(BS25th), BS5th = lowlim(BS5th))


datall2 <- datall
datall2[, 3:4] <- apply(datall[, 3: 4], 2, function(x) {
  x[x < 10^(-4)] <- 10^(-4)
  x
  })

ggplot(datall2, aes(x = Species, lower = BS25th, middle = BS50th, upper = BS75th, 
  ymin = BS5th, ymax = BS95th)) + geom_boxplot(stat = "identity") +
  theme_bw() + xlab("") +
  scale_y_log10(limits = c(10^(-4), 10^1)) + #coord_trans(y = "log10") +
  ylab("Constituent concentration") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 14)) +
  facet_wrap(~ Source)


# Each row is a source
cn2 <- colnames(nycdat[, -1])
skip1 <- 15
for(i in 1 : 25) {
  dat1 <- read.table(file.path(fp1, "nycrun_profile_boot.txt"), sep = "\t", skip = skip1, nrows = 6, blank.lines.skip = F)

  dat1 <- mutate(dat1, Species = cn2[i], Source = paste("Source", seq(1, 6))) 
  if(i > 1) {
    datall <- full_join(dat1, datall)
  } else{
    datall <- dat1
  }
  skip1 <- skip1 + 6 + 1
}

datall2 <- select(datall, V16 : V115, Species, Source)
datall2 <- gather(datall2, boot, value, -Species, -Source)

base <- select(datall, V1, Species, Source)

datall2$Species <- factor(datall2$Species, levels = cn2)
#datall2 <- filter(datall2, Species != "PM25_SPEC")
gfun <- function(sourcen) {
  sn1 <- paste("Source", sourcen)
  datall2 <- filter(datall2, Source == sn1)
  base2 <- filter(base, Source == sn1)
  g1 <- ggplot(datall2, aes(x = Species, y = value)) + geom_boxplot() +
    geom_point(data = base2, aes(y = V1), colour = "red", shape =8 ) +
   theme_bw() + xlab("") +
    scale_y_log10(limits = c(10^(-4), 10^1)) + #coord_trans(y = "log10") +
  ylab("Constituent concentration") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 14))  
  #facet_wrap(~ Source)
  g1
}
gfun(1)




# Compare with profile results
P <- 25
L <- 6
skips <- 4
prof <- read.table(file.path(fp1, "nycrun_profiles.txt"), skip = skips, nrows = P, sep = "\t", header = F)[, -1]
colnames(prof) <- c("Species", paste("Source", seq(1, L)))
prof <- arrange(prof, Species)

s1 <- vector()
for(i in 1 : 114) {
  vname <- paste0("V", i)
  prof2 <- datall[, c(vname, "Species", "Source")]
  colnames(prof2)[1] <- "base"
  prof2 <- spread(prof2, Source, base)
  s1[i] <- sum((prof[, -1]- prof2[, -1])^2)

}
