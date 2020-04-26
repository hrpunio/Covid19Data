library("dplyr")
library("ggplot2")
library("scales")
library("ggpubr")
##
spanV <- 0.25

surl <- "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide"
today <- Sys.Date()
tt<- format(today, "%d/%m/%Y")

w <- read.csv("wms2020.csv", sep = ';',  header=T, na.string="NA", 
   colClasses = c('factor', 'factor', 'factor', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'));

str(w)

d <- read.csv("ecdc2020.csv", sep = ';',  header=T, na.string="NA", 
   colClasses = c('factor', 'factor', 'factor', 'character', 'character', 'numeric', 'numeric'));

d$newc <- as.numeric(d$newc)
d$newd <- as.numeric(d$newd)

d <- d %>% filter(as.Date(date, format="%Y-%m-%d") > "2020-04-03") %>% as.data.frame

str(d)


compCC <- function(cc) {
c1 <- c(cc)
ofile <- sprintf ("compC_%s.png", cc)

d1 <- d %>% filter (id %in% c1) %>% as.data.frame
w1 <- w %>% filter (iso %in% c1) %>% as.data.frame

lobs.e <- last(d1$date)
lobs.ey <- last(d1$dateY)

lobs.w <- last(w1$date)
lobs.wy <- last(w1$dateY)

lobs0 <- sprintf("(e/w=%s/%s)", lobs.e, lobs.w)
lobs1 <- sprintf("(e/w=%s/%s)", lobs.ey, lobs.wy)

pc1n <- ggplot(d1, aes(x= as.Date(date), y=newc )) +
 geom_line(aes(), group=1, size=1, color="mediumpurple1", alpha=.4) +
 geom_line(data=w1, aes(x = as.Date(date), y=newC ), group=1, size=1, color="lightsalmon1", alpha=.4) +
 xlab(label="yy/mm") +
 ylab(label="cases") +
 scale_x_date( labels = date_format("%m/%d"), breaks = "1 week") +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 ggtitle(sprintf("New cases %s (last: %s)", cc, lobs0), subtitle=sprintf("ECDC vs Wordometers (red)"))

pc1t <- ggplot(d1, aes(x= as.Date(date), y=totalc )) +
 geom_line(aes(), group=1, size=1, color="mediumpurple1", alpha=.4) +
 geom_line(data=w1, aes(x = as.Date(date), y=totalC ), group=1, size=1, color="lightsalmon1", alpha=.4) +
 xlab(label="yy/mm") +
 ylab(label="cases") +
 scale_x_date( labels = date_format("%m/%d"), breaks = "1 week") +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 ggtitle(sprintf("Total cases %s (last: %s)", cc, lobs0), subtitle=sprintf("Source: ECDC vs Wordometers (red)"))

pc1n1 <- ggplot(d1, aes(x= as.Date(dateY), y=newc )) +
 geom_line(aes(), group=1, size=1, color="mediumpurple1", alpha=.4) +
 geom_line(data=w1, aes(x = as.Date(dateY), y=newC ), group=1, size=1, color="lightsalmon1", alpha=.4) +
 xlab(label="yy/mm") +
 ylab(label="cases") +
 scale_x_date( labels = date_format("%m/%d"), breaks = "1 week") +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 ggtitle(sprintf("New cases %s (last: %s)", cc, lobs1), subtitle=sprintf("ECDC vs Wordometers (red)"))

pc1t1 <- ggplot(d1, aes(x= as.Date(dateY), y=totalc )) +
 geom_line(aes(), group=1, size=1, color="mediumpurple1", alpha=.4) +
 geom_line(data=w1, aes(x = as.Date(dateY), y=totalC ), group=1, size=1, color="lightsalmon1", alpha=.4) +
 xlab(label="yy/mm") +
 ylab(label="cases") +
 scale_x_date( labels = date_format("%m/%d"), breaks = "1 week") +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 ggtitle(sprintf("Total cases %s (last: %s)", cc, lobs1), subtitle=sprintf("Source: ECDC vs Wordometers (red)"))

pp <- ggarrange(pc1n, pc1t,  pc1n1, pc1t1, ncol = 2, nrow = 2)
ggsave(plot=pp, ofile, width=12)

}
compDD <- function(cc) {
c1 <- c(cc)
ofile <- sprintf ("compD_%s.png", cc)

d1 <- d %>% filter (id %in% c1) %>% as.data.frame
w1 <- w %>% filter (iso %in% c1) %>% as.data.frame

d1 <- d %>% filter (id %in% c1) %>% as.data.frame
w1 <- w %>% filter (iso %in% c1) %>% as.data.frame

lobs.e <- last(d1$date)
lobs.ey <- last(d1$dateY)
lobs.w <- last(w1$date)
lobs.wy <- last(w1$dateY)

lobs0 <- sprintf("(e/w=%s/%s)", lobs.e, lobs.w)
lobs1 <- sprintf("(e/w=%s/%s)", lobs.ey, lobs.wy)

pc1n <- ggplot(d1, aes(x= as.Date(date), y=newd )) +
 geom_line(aes(), group=1, size=1, color="mediumpurple1", alpha=.4) +
 geom_line(data=w1, aes(x = as.Date(date), y=newD ), group=1, size=1, color="lightsalmon1", alpha=.4) +
 xlab(label="yy/mm") +
 ylab(label="cases") +
 scale_x_date( labels = date_format("%m/%d"), breaks = "1 week") +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 ggtitle(sprintf("New deaths %s (last: %s)", cc, lobs0), subtitle=sprintf("ECDC vs Wordometers (red)"))

pc1t <- ggplot(d1, aes(x= as.Date(date), y=totald )) +
 geom_line(aes(), group=1, size=1, color="mediumpurple1", alpha=.4) +
 geom_line(data=w1, aes(x = as.Date(date), y=totalD ), group=1, size=1, color="lightsalmon1", alpha=.4) +
 xlab(label="yy/mm") +
 ylab(label="cases") +
 scale_x_date( labels = date_format("%m/%d"), breaks = "1 week") +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 ggtitle(sprintf("Total deaths %s (last: %s)", cc, lobs0), subtitle=sprintf("Source: ECDC vs Wordometers (red)"))

pc1n1 <- ggplot(d1, aes(x= as.Date(dateY), y=newd )) +
 geom_line(aes(), group=1, size=1, color="mediumpurple1", alpha=.4) +
 geom_line(data=w1, aes(x = as.Date(dateY), y=newD ), group=1, size=1, color="lightsalmon1", alpha=.4) +
 xlab(label="yy/mm") +
 ylab(label="cases") +
 scale_x_date( labels = date_format("%m/%d"), breaks = "1 week") +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 ggtitle(sprintf("New deaths %s (last: %s)", cc, lobs1), subtitle=sprintf("ECDC vs Wordometers (red)"))

pc1t1 <- ggplot(d1, aes(x= as.Date(dateY), y=totald )) +
 geom_line(aes(), group=1, size=1, color="mediumpurple1", alpha=.4) +
 geom_line(data=w1, aes(x = as.Date(dateY), y=totalD ), group=1, size=1, color="lightsalmon1", alpha=.4) +
 xlab(label="yy/mm") +
 ylab(label="cases") +
 scale_x_date( labels = date_format("%m/%d"), breaks = "1 week") +
 theme(plot.subtitle=element_text(size=8, hjust=0, color="black")) +
 ggtitle(sprintf("Total deaths %s (last: %s)", cc, lobs1), subtitle=sprintf("Source: ECDC vs Wordometers (red)"))

pp <- ggarrange(pc1n, pc1t,  pc1n1, pc1t1, ncol = 2, nrow = 2)
ggsave(plot=pp, ofile, width=12)

}

################
compCC('US')
compCC('IT')
compCC('DE')
compCC('ES')
compCC('FR')
compCC('GB')
compCC('BE')
compCC('SE')
compCC('DK')
compCC('IE')
compCC('IL')
compCC('GR')
compCC('AT')
compCC('PL')
compCC('CZ')
compCC('SK')
compCC('HU')
compCC('RO')
compCC('BG')
compCC('CH')
compCC('PT')
compCC('NL')
compCC('JP')
compCC('BR')
###############
compDD('US')
compDD('IT')
compDD('DE')
compDD('ES')
compDD('FR')
compDD('GB')
compDD('BE')
compDD('SE')
compDD('DK')
compDD('IE')
compDD('IL')
compDD('GR')
compDD('AT')
compDD('PL')
compDD('CZ')
compDD('SK')
compDD('HU')
compDD('RO')
compDD('BG')
compDD('CH')
compDD('PT')
compDD('NL')
compDD('JP')
compDD('BR')
