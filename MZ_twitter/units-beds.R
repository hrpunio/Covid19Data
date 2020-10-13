## Problem dane są w 2 plikach csv
## należy je połączyć i wydrukować jako wykres liniowy
library("dplyr")
library("ggplot2")
library("scales")
library("ggpubr")

spanV <- 0.25
labTxt <- "loess smoothing with span = 0.25"
note <- "data:   source: twitter.com/MZ_GOV_PL (image scrapping)"

d <- read.csv("MZ_units_beds.csv", sep = ';',  header=T, na.string="NA");

last.obs <- last (d$date)
first.obs <- first (d$date)
fstDay <- as.Date(first.obs)

maxUU <- max(d$units)
p1 <- ggplot(d, aes(x= as.Date(date, format="%Y-%m-%d"), y=units )) + 
 geom_smooth(method="loess", se=F, span=spanV) +
 geom_point(size=.4, alpha=.5) +
 xlab(label="") +
 ylab(label="units") +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 scale_x_date( labels = date_format("%m/%d"), breaks = "2 weeks") +
 annotate("text", x = fstDay, y = maxUU, label = labTxt, hjust = 0, size=3) +
 ggtitle(sprintf("COVID19i: Ventillators in use (last: %s)", last.obs), subtitle=note)

maxBB <- max(d$beds)
p2 <- ggplot(d, aes(x= as.Date(date, format="%Y-%m-%d"), y=beds )) + 
 geom_smooth(method="loess", se=F, span=spanV) +
 geom_point(size=.4, alpha=.5) +
 xlab(label="") +
 ylab(label="Beds") +
 scale_x_date( labels = date_format("%m/%d"), breaks = "2 weeks") +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 annotate("text", x = fstDay, y = maxBB, label = labTxt, hjust = 0, size=3) +
 ggtitle(sprintf("COVID19: Beds occupied (last: %s)", last.obs), subtitle=note)

maxPP <- max(d$persons)
p3 <- ggplot(d, aes(x= as.Date(date, format="%Y-%m-%d"), y=persons )) + 
 geom_smooth(method="loess", se=F, span=spanV) +
 geom_point(size=.4, alpha=.5) +
 xlab(label="") +
 ylab(label="Persons") +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 scale_x_date( labels = date_format("%m/%d"), breaks = "2 weeks") +
 annotate("text", x = fstDay, y = maxPP, label = labTxt, hjust = 0, size=3) +
 ggtitle(sprintf("COVID19: Currently in quarantine (last: %s)", last.obs), subtitle=note)

pp <- ggarrange(p1,p2,p3, ncol=1, nrow=3)
ggsave(pp, file='beds_and_ventillators.png', height=11)
