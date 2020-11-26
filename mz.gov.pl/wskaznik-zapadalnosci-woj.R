#!/usr/bin/env Rscript
## Narodowy Instytut K-W (NI-KW)
## (mzn_sxx wersja dla NI-KW)
##
library("dplyr")
library("tidyr")
library("ggplot2")
library("scales")
library("ggpubr")
library("ggthemes")
##
pwidth <- 12
spanV <- 0.5
m1unit <- 100 ## pop in ths so 100 not 100000
##
surl <- "© NI-KW (source: http://www.wsse.gda.pl/)"
##mzurl <- "https://www.gov.pl/web/koronawirus/wykaz-zarazen-koronawirusem-sars-cov-2"
mzurl <- "© NI-KW (source: https://www.gov.pl/web/koronawirus/wykaz-zarazen-koronawirusem-sars-cov-2)"

mainColor <- "deeppink"
loessColor <- "steelblue"
mainBreaks <- "3 weeks"

# date;time;woj;newc;newd;totalc;totald
d <- read.csv("MZN.csv", sep = ';',  header=T, na.string="NA" )
p <- read.csv("wojewodztwa.csv", sep = ';',  header=T, na.string="NA" )
d <- left_join(d, p, by = "woj")

d$tc1m <- d$totalc / d$pop * m1unit

labTxtN <- "cases/day (loess smoothing with span = 0.5)"
labTxtT <- "cases/total (loess smoothing with span = 0.5)"
last.obs <- last(d$date)
first.obs <- first(d$date)

today <- last(as.Date(d$date))
first.day <- today - 14
tt <- format(today, "%Y-%m-%d")
fd <- format(first.day, "%Y-%m-%d")

day00 <- as.Date(last.obs)
day14 <- as.Date(last.obs) - 14
day28 <- as.Date(last.obs) - 28

e00 <- d %>% filter (as.Date(date) == day00 ) %>% group_by(woj) %>% as.data.frame
e14 <- d %>% filter (as.Date(date) == day14 ) %>% group_by(woj) %>% as.data.frame
e14 <- d %>% filter (as.Date(date) == day14 ) %>% group_by(woj) %>% as.data.frame
e28 <- d %>% filter (as.Date(date) == day28 ) %>% group_by(woj) %>% as.data.frame
e28

c28 <- e14$totalc - e28$totalc
c14 <- e00$totalc - e14$totalc
## per 100/tys
c28m1 <- e14$tc1m - e28$tc1m
c14m1 <- e00$tc1m - e14$tc1m

########################################################
## wskaźnik zapadalności = cases in last 14 days/100 ths
########################################################
## przyrost względem poprzednich dwóch tygodni
##d14v28 <- (c14m1 - c28m1) / c28m1 * 100
d14v28 <- c14m1 / c28m1 * 100
##d14v28

e00$c14m1 <- c14m1
e00$d14v28 <- d14v28

#### 
sprintf("%10.10s = %6.2f | %6.2f | %6.2f - %6.2f | %i | %i | %i (%.2f)",
 e00$woj, d14v28, c14m1, e00$tc1m, e28$tc1m, e00$totalc, e14$totalc, e28$totalc, e00$pop )

####
barColor <- "darkseagreen4"
labelColor <- "darkslategray"

w1 <- ggplot(data=e00, aes(x=reorder(as.factor(woj), c14m1), y=c14m1 )) +
  geom_bar(stat="identity", position=position_dodge(width=.4), width=.8,  fill=barColor, alpha=.5) +
  geom_text(aes(label=sprintf("%.1f", c14m1), y= c14m1 ), hjust=1.25, size=3, color=labelColor ) +
  theme(legend.position="top") +
  ylab(label="cases") +
  xlab(label="") +
  theme_nikw() +
  labs(caption=sprintf("%s", mzurl) ) +
  ggtitle(sprintf("PL: COVID19 14-day rate* (%s--%s)", fd, tt),
  subtitle= sprintf("14-day notification rate of reported COVID-19 cases per 100000") ) +
  coord_flip()

w2 <- ggplot(data=e00, aes(x=reorder(as.factor(woj), d14v28), y=d14v28 )) +
  geom_bar(stat="identity", position=position_dodge(width=.4), width=.8,  fill=barColor, alpha=.5) +
  geom_text(aes(label=sprintf("%.1f", d14v28), y= d14v28 ), hjust=1.25, size=3, color=labelColor ) +
  theme(legend.position="top") +
  ylab(label="%") +
  xlab(label="") +
  theme_nikw() +
  labs(caption=sprintf("%s", mzurl) ) +
  ggtitle(sprintf("PL: COVID19 14-day rate change*"), 
  subtitle= sprintf("*1--14 days rate/15--28 days rate * 100") ) +
  coord_flip()

w12 <- ggarrange( w1, w2, ncol=2, nrow=1)

ggsave(plot=w12, file="covid_PL_14x100_nikw.png", width=12)
