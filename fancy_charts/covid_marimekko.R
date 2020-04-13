## https://learnr.wordpress.com/2009/03/29/ggplot2-variable-width-column-chart/
## https://www.r-graph-gallery.com/81-barplot-with-variable-width.html
## https://www.yan-holtz.com/
library(ggplot2)
library(dplyr)

dat <- "2020/04/09"

##df <- data.frame(x = c("Alpha", "Beta", "Gamma",
##     "Delta"), width = c(25, 250, 75, 100), height = c(100,
##     75, 50, 25))

d <- read.csv("indcs.csv", sep = ';',  header=T, na.string="NA");
## liczba ludności w milionach (Width of Marineko):
d$popm <- d$pop / million

## Oblicz współczynniki na 1mln
d$casesr <- d$cases/d$popm
### Height of Marineko:
d$deathsr <- d$deaths/d$popm

## Tylko kraje wykazujące zmarłych
d <- d %>% filter(deaths > 0) %>% as.data.frame
## Tylko kraje z kompletem wskaźników
##d <- d[complete.cases(d), ]
nrow(d)
######################## Deaths ##############################################
## Tylko kraje z min 2/1mln i populacji > 1mln
d9 <- d %>% filter(deathsr > 2 & popm > 3 & deaths > 49) %>% droplevels() %>%
 arrange (deathsr) %>% as.data.frame

d8 <- d %>% filter(casesr > 10 & popm > 3 & deaths > 49) %>% droplevels() %>%
 arrange (casesr) %>% as.data.frame

d9$w <- cumsum(d9$popm)
d9$wm <- d9$w - d9$popm
d9$wt <- with(d9, wm + (w - wm)/2)

d8$w <- cumsum(d8$popm)
d8$wm <- d8$w - d8$popm
d8$wt <- with(d8, wm + (w - wm)/2)


d9$iso3h <- d9$iso3
d9$iso3l <- d9$iso3
## Kraje o niskich  wartościach bez etykiet
d9$iso3h[ (d9$popm < 15 ) ] <- ""
d9$iso3l[ (d9$popm >= 15 ) ] <- ""

p9  <- ggplot(d9, aes(ymin = 0)) +
  ylab(label="mratio (deaths/1mln)") +
  xlab(label="population (mln)") +
  ggtitle(sprintf("COVID19 mortality (%s | mratio > 2 | population > 3mln )", dat), 
      subtitle="source: https://www.ecdc.europa.eu/en/covid-19-pandemic (twitter.com/tprzechlewski)") +
  geom_rect(aes(xmin = wm, xmax = w, ymax = deathsr, fill = iso3)) +
  geom_text(aes(x = wt, y = 0, label = iso3h), vjust=+0.5, hjust=+1.25, size=2.0, angle = 90) +
  geom_text(aes(x = wt, y = 0, label = iso3l), vjust=+0.5, hjust=-0.20, size=1.5, angle = 90) +
  theme(legend.position = "none") 

##p  <- ggplot(d9, aes(ymin = 0)) +
##  ylab(label="Deaths/1mln") +
##  xlab(label="Population (mln)") +
##  ggtitle("COVID19 mortality", subtitle="source: https://www.ecdc.europa.eu/en/covid-19-pandemic (twitter.com/tprzechlewski)")
##p1 <- p + geom_rect(aes(xmin = wm, xmax = w, ymax = deathsr, fill = iso3))
##
####p2 <- p1 + geom_text(aes(x = wt, y = deathsr * 0.5, label = iso3), size=1.5, angle = 90)
##p2 <- p1 + geom_text(aes(x = wt, y = 0, label = iso3), vjust=+0.5, hjust=+1.25, size=2.0, angle = 90) +
##  theme(legend.position = "none") 
##
##p3 <- p2 + theme_bw() 
##     ###opts(legend.position = "none") +
##     ##labs(x = NULL, y = NULL)
###########################################

d8$iso3h <- d8$iso3
d8$iso3l <- d8$iso3
## Kraje o niskich  wartościach bez etykiet
d8$iso3h[ (d8$popm < 15 ) ] <- ""
d8$iso3l[ (d8$popm >= 15 ) ] <- ""

p8  <- ggplot(d8, aes(ymin = 0)) +
  ylab(label="cratio (cases/1mln)") +
  xlab(label="population (mln)") +
  ggtitle(sprintf("COVID19 cases (%s | cratio > 10 | population > 3mln)", dat), 
       subtitle="source: https://www.ecdc.europa.eu/en/covid-19-pandemic (twitter.com/tprzechlewski)") +
  geom_rect(aes(xmin = wm, xmax = w, ymax = casesr, fill = iso3)) +
  geom_text(aes(x = wt, y = 0, label = iso3h), vjust=+0.5, hjust=+1.25, size=2.0, angle = 90) +
  geom_text(aes(x = wt, y = 0, label = iso3l), vjust=+0.5, hjust=-0.20, size=1.5, angle = 90) +
  theme(legend.position = "none") 

ggsave(file="marimeko_01.png", plot=p9, width=15)
ggsave(file="marimeko_02.png", plot=p8, width=15)
