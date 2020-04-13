##
library("dplyr")
library("ggplot2")
library("ggpubr")
##
options(scipen=1000000)
## https://www.r-bloggers.com/the-notin-operator/
`%notin%` <- Negate(`%in%`)
##
today <- Sys.Date()
tt<- format(today, "%d/%m/%Y")
million <- 1000000
##
## Selected European Countries + Israel
ee <- c(
'BEL', 'GRC', 'LTU', 'PRT', 'BGR', 'ESP', 'LUX', 'ROU', 'CZE', 'FRA', 'HUN',
'SVN', 'DNK', 'HRV', 'MLT', 'SVK', 'DEU', 'ITA', 'NLD', 'FIN', 'EST', 'CYP',
'AUT', 'SWE', 'IRL', 'LVA', 'POL', 'ISL', 'NOR', 'LIE', 'CHE', 'MNE', 'MKD',
'ALB', 'SRB', 'TUR', 'BIH', 'BLR', 'MDA', 'UKR', 'ISR', 'RUS', 'GBR' );
ee.ee <- c('POL')

d <- read.csv("indcs.csv", sep = ';',  header=T, na.string="NA");
## Liczba krajów
N1 <- nrow(d)
## liczba ludności w milionach
d$popm <- d$pop / million

## Oblicz współczynniki na 1mln 
d$casesr <- d$cases/d$popm 
d$deathsr <- d$deaths/d$popm 

## Tylko kraje wykazujące zmarłych
d <- d %>% filter(deaths > 0) %>% as.data.frame
## Liczba krajów wykazujących zmarłych
N1d <- nrow(d)

## Tylko kraje z kompletem wskaźników
d <- d[complete.cases(d), ]

#########
nrow(d)
######################## Deaths ##############################################

d9 <- d %>% filter(deathsr > 2 ) %>% droplevels() %>% 
 mutate (iso3 = reorder(iso3, deathsr)) %>% as.data.frame

N1d2 <- nrow(d9)
M1d2 <- median(d9$deathsr, na.rm=T)

str(d9)

d9$iso3
###
factor_cols <- vapply(d9, is.factor, logical(1))

# Apply the factor() function to those columns, and assign then back into d
d9[factor_cols] <- lapply(d9[factor_cols], factor)
str(d9)
###

## https://stackoverflow.com/questions/11093248/geom-vline-with-character-xintercept
rys99 <- ggplot(d9, aes(x =iso3, y = deathsr )) +
  geom_point(size=1, colour = 'steelblue', alpha=.5) +
  xlab(label="Country") +
  ylab(label="Deaths/1mln") +
  ggtitle(sprintf("COVID19 mortality in deaths/mln (as of %s)", tt), 
   subtitle=sprintf("Countries with ratio > 0: %i | Countries with ratio > 2.0: N=%i (median %.1f)", 
       N1d, N1d2, M1d2)) +
  theme(axis.text = element_text(size = 4)) +
  ##theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks=c(0,20,40,60,80,100,120,140,160,180,200,220,240,260,280,300,320,340,360)) +
  geom_hline(yintercept=M1d2, linetype="solid", color = "steelblue") +
  ## nihu-hu nie działa
  geom_vline(aes(xintercept = which(levels(iso3) == 'POL')), size=1, color="#8A0303", alpha=.25) +
  geom_vline(aes(xintercept = which(levels(iso3) == 'DEU')), size=1, color="#8A0303", alpha=.25) +
  geom_vline(aes(xintercept = which(levels(iso3) == 'SWE')), size=1, color="#8A0303", alpha=.25) +
  coord_flip()

ggsave(plot=rys99, file="covidd_dp99.png")

################
sources <- sprintf ("As of %s\n(Sources: https://www.ecdc.europa.eu/en/covid-19-pandemic https://ourworldindata.org/)", tt)

## Etykiety tylko dla wybranych krajów

## Add empty factor level 
# https://rpubs.com/Mentors_Ubiqum/Add_Levels
d$iso3 <- factor(d$iso3, levels = c(levels(d$iso3), ""))

d$iso3xgdp <- d$iso3
d$iso3xlex <- d$iso3
d$iso3xcm <- d$iso3

## Kraje o niskich  wartościach bez etykiet
d$iso3xgdp[ (d$gdp2016 < 45000) & (d$deathsr < 50 ) ] <- ""
d$iso3xlex[ ( (d$lex2019 < 80) | (d$deathsr < 50 ) ) ] <- ""
d$iso3xcm[ ((d$cm2017 > 1.2) | (d$deathsr < 50 ) ) ] <- ""

nrow(d)

rys1 <- ggplot(d, aes(x=gdp2016, y=deathsr)) + 
  geom_point() +
  geom_text(data=d, aes(label=sprintf("%s", iso3xgdp), x=gdp2016, y= deathsr), vjust=-0.9, size=2 ) +
  xlab("GDP (USD, Constant prices)") + 
  ylab("deaths/1mln") + 
  geom_smooth(method="loess", se=F, size=2) +
  ggtitle("GDP2016CP vs COVID19 mortality", subtitle=sources)

rys2 <- ggplot(d, aes(x=lex2019, y=deathsr)) + 
  geom_point() +
  geom_text(data=d, aes(label=sprintf("%s", iso3xlex), x=lex2019, y= deathsr), vjust=-0.9, size=2 ) +
  xlab("Life expentancy") + 
  ylab("deaths/1mln") + 
  geom_smooth(method="loess", se=F, size=2) +
  ggtitle("Life expentancy vs COVID19 mortality", subtitle=sources)

rys3 <- ggplot(d, aes(x=cm2017, y=deathsr)) + 
  geom_point() +
  geom_text(data=d, aes(label=sprintf("%s", iso3xcm), x=cm2017, y= deathsr), vjust=-0.9, size=2 ) +
  xlab("Child mortality %") +
  ylab("deaths/1mln") + 
  geom_smooth(method="loess", se=F, size=2) +
  ggtitle("Child mortality vs COVID19 mortality", subtitle=sources)

rys0 <- ggplot(d, aes(x=gdp2016, y=cm2017)) + 
  geom_point() +
  xlab("GDP (USD, Constant prices)") + 
  ylab("Child mortality") +
  geom_smooth(method="loess", se=F, size=2) +
  ggtitle("GDP2016CP vs Child mortality", subtitle=sources)

ggsave(plot=rys1, file="covidd_gdp_w.png", width=10)
ggsave(plot=rys2, file="covidd_le_w.png", width=10)
ggsave(plot=rys3, file="covidd_cm_w.png", width=10)
ggsave(plot=rys0, file="covidd_gdp_cm_w.png", width=10)

de <- d %>% filter (iso3 %in% ee) %>% as.data.frame
str(de)

## Wykresy 1--3 dla krajów europy ############
## Wyróżniamy Polskę
de$iso3xgdp <- de$gdp2016
de$iso3xlex <- de$lex2019
de$iso3xcm <- de$cm2017
## Bez etykiet wszystko za wyjątkiem PL
de$iso3xgdp[ de$iso3 %notin% ee.ee ] <- NA
de$iso3xlex[ de$iso3 %notin% ee.ee ] <- NA
de$iso3xcm[ de$iso3 %notin% ee.ee ] <- NA

sources <- sprintf ("As of %s | Red dot = PL\n(Sources: https://www.ecdc.europa.eu/en/covid-19-pandemic https://ourworldindata.org/)", tt)

rys1e <- ggplot(de, aes(x=gdp2016, y=deathsr)) + 
  geom_point() +
  geom_text(data=de, aes(label=sprintf("%s", iso3), x=gdp2016, y= deathsr), vjust=-0.9, size=2 ) +
  geom_point(data=de, aes(x=iso3xgdp, y= deathsr), size=2, color="red" ) +
  xlab("GDP (USD, Constant prices)") + 
  ylab("deaths/1mln") +
  geom_smooth(method="loess", se=F, size=2) +
  ggtitle("GDP2016CP vs COVID19 mortality (Europe)", subtitle=sources)

rys2e <- ggplot(de, aes(x=lex2019, y=deathsr)) + 
  geom_point() +
  geom_text(data=de, aes(label=sprintf("%s", iso3), x=lex2019, y= deathsr), vjust=-0.9, size=2 ) +
  geom_point(data=de, aes(x=iso3xlex, y= deathsr), size=2, color="red" ) +	       
  xlab("Life expentancy") + 
  ylab("deaths/1mln") +
  geom_smooth(method="loess", se=F, size=2) +
  ggtitle("Life expentancy vs COVID19 mortality (Europe)", subtitle=sources)

rys3e <- ggplot(de, aes(x=cm2017, y=deathsr)) + 
  geom_point() +
  geom_text(data=de, aes(label=sprintf("%s", iso3), x=cm2017, y= deathsr), vjust=-0.9, size=2 ) +
  geom_point(data=de, aes(x=iso3xcm, y= deathsr), size=2, color="red" ) +
  xlab("Child mortality %") + 
  ylab("deaths/1mln") +
  geom_smooth(method="loess", se=F, size=2) +
  ggtitle("Child mortality vs COVID19 mortality (Europe)", subtitle=sources)

rys0e <- ggplot(de, aes(x=gdp2016, y=cm2017)) + 
  geom_point() +
  geom_text(data=de, aes(label=sprintf("%s", iso3), x=gdp2016, y= cm2017), vjust=-0.9, size=2 ) +
  xlab("GDP (USD, Constant prices") + 
  ylab("Child mortality %") +
  geom_smooth(method="loess", se=F, size=2) +
  ggtitle("GDP2016CP vs Child mortality (Europe)", subtitle=sources)

ggsave(plot=rys1e, file="covidd_gdp_e.png", width=10)
ggsave(plot=rys2e, file="covidd_le_e.png", width=10)
ggsave(plot=rys3e, file="covidd_cm_e.png", width=10)
ggsave(plot=rys0e, file="covidd_gdp_cm_e.png", width=10)

############################ Cases #######################################################
sources <- sprintf ("As of %s\n(Sources: https://www.ecdc.europa.eu/en/covid-19-pandemic https://ourworldindata.org/)", tt)

d$iso3xgdp <- d$iso3
d$iso3xlex <- d$iso3
d$iso3xcm <- d$iso3

## Niskie wartości bez etykiet
d$iso3xgdp[ (d$gdp2016 < 45000) & (d$casesr < 50 ) ] <- ""
d$iso3xlex[ ( (d$lex2019 < 80) | (d$casesr < 50 ) ) ] <- ""
d$iso3xcm[ ((d$cm2017 > 1.2) | (d$casesr < 50 ) ) ] <- ""

rys1c <- ggplot(d, aes(x=gdp2016, y=casesr)) + 
  geom_point() +
  geom_text(data=d, aes(label=sprintf("%s", iso3xgdp), x=gdp2016, y= casesr), vjust=-0.9, size=2 ) +
  xlab("GDP (USD, Constant prices)") + 
  ylab("cases/1mln") + 
  geom_smooth(method="loess", se=F, size=2) +
  ggtitle("GDP2016CP vs COVID19 cases", subtitle=sources)

rys2c <- ggplot(d, aes(x=lex2019, y=casesr)) + 
  geom_point() +
  geom_text(data=d, aes(label=sprintf("%s", iso3xlex), x=lex2019, y= casesr), vjust=-0.9, size=2 ) +
  xlab("Life expentancy") + 
  ylab("cases/1mln") + 
  geom_smooth(method="loess", se=F, size=2) +
  ggtitle("Life expentancy vs COVID19 cases", subtitle=sources)

rys3c <- ggplot(d, aes(x=cm2017, y=casesr)) + 
  geom_point() +
  geom_text(data=d, aes(label=sprintf("%s", iso3xcm), x=cm2017, y= casesr), vjust=-0.9, size=2 ) +
  xlab("Child mortality %") +
  ylab("cases/1mln") + 
  geom_smooth(method="loess", se=F, size=2) +
  ggtitle("Child mortality vs COVID19 mortality", subtitle=sources)

### ### ##
sources <- sprintf ("As of %s | Red dot = PL\n(Sources: https://www.ecdc.europa.eu/en/covid-19-pandemic https://ourworldindata.org/)", tt)
ggsave(plot=rys1c, file="covidc_gdp_w.png", width=10)
ggsave(plot=rys2c, file="covidc_le_w.png", width=10)
ggsave(plot=rys3c, file="covidc_cm_w.png", width=10)

de <- d %>% filter (iso3 %in% ee) %>% as.data.frame

de$iso3xgdp <- de$gdp2016
de$iso3xlex <- de$lex2019
de$iso3xcm <- de$cm2017
de$iso3xgdp[ de$iso3 %notin% ee.ee ] <- NA
de$iso3xlex[ de$iso3 %notin% ee.ee ] <- NA
de$iso3xcm[ de$iso3 %notin% ee.ee ] <- NA

rys1ec <- ggplot(de, aes(x=gdp2016, y=casesr)) + 
  geom_point() +
  geom_text(data=de, aes(label=sprintf("%s", iso3), x=gdp2016, y= casesr), vjust=-0.9, size=2 ) +
  geom_point(data=de, aes(x=iso3xgdp, y= casesr), size=2, color="red" ) +
  xlab("GDP (USD, Constant prices") + 
  ylab("cases/1mln") +
  geom_smooth(method="loess", se=F, size=2) +
  ggtitle("GDP2016CP vs COVID19 cases (Europe)", subtitle=sources)

rys2ec <- ggplot(de, aes(x=lex2019, y=casesr)) + 
  geom_point() +
  geom_text(data=de, aes(label=sprintf("%s", iso3), x=lex2019, y= casesr), vjust=-0.9, size=2 ) +
  geom_point(data=de, aes(x=iso3xlex, y= casesr), size=2, color="red" ) +	       
  xlab("Life expentancy") + 
  ylab("cases/1mln") +
  geom_smooth(method="loess", se=F, size=2) +
  ggtitle("Life expentancy vs COVID19 cases (Europe)", subtitle=sources)

rys3ec <- ggplot(de, aes(x=cm2017, y=casesr)) + 
  geom_point() +
  geom_text(data=de, aes(label=sprintf("%s", iso3), x=cm2017, y= casesr), vjust=-0.9, size=2 ) +
  geom_point(data=de, aes(x=iso3xcm, y= casesr), size=2, color="red" ) +
  xlab("Child mortality") + 
  ylab("cases/1mln") +
  geom_smooth(method="loess", se=F, size=2) +
  ggtitle("Child mortality vs COVID19 cases (Europe)", subtitle=sources)

ggsave(plot=rys1ec, file="covidc_gdp_e.png", width=10)
ggsave(plot=rys2ec, file="covidc_le_e.png", width=10)
ggsave(plot=rys3ec, file="covidc_cm_e.png", width=10)

