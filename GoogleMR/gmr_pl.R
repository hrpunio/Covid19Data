library("dplyr")
library("ggplot2")
library("ggpubr")
##
spanV <- 0.25

surl <- "https://www.google.com/covid19/mobility/"
today <- Sys.Date()
tt<- format(today, "%d/%m/%Y")

d <- read.csv("2020-03-29_PL_Mobility_Report_en.csv", sep = ';',  header=T, na.string="NA")
##   colClasses = c('factor', 'factor', 'factor', 'character', 'character', 'numeric', 'numeric'));

## date;country;retrec;grocp;parks;transit;work;residential
p1 <- ggplot(d, aes(x=reorder(country, retrec),, y=retrec)) + 
 geom_bar(stat="identity", fill = "steelblue") + 
 geom_text(data=d, aes(label=sprintf("%.0f", retrec), x=country, y= retrec), hjust=-0.5, size=3, color="white" ) +
 xlab(label="") + ylab(label="%%") +
 scale_y_continuous(breaks=c(-90,-80,-70,-60,-50,-40,-30,-20,-10, 0, 10, 20, 30, 40, 50)) +
 coord_flip()+
 ggtitle(sprintf ("Retail/recreation mobility trends"), subtitle=sprintf("%s", surl))

p2 <- ggplot(d, aes(x=reorder(country, grocp),, y=grocp)) + 
 geom_bar(stat="identity", fill = "steelblue") + 
 geom_text(data=d, aes(label=sprintf("%.0f", grocp), x=country, y= grocp), hjust=-0.5, size=3, color="white" ) +
 xlab(label="") + ylab(label="%%") +
 scale_y_continuous(breaks=c(-90,-80,-70,-60,-50,-40,-30,-20,-10, 0, 10, 20, 30, 40, 50)) +
 coord_flip()+
 ggtitle(sprintf ("Grocery/pharmacy mobility trends"), subtitle=sprintf("%s", surl))

p3 <- ggplot(d, aes(x=reorder(country, parks),, y=parks)) + 
 geom_bar(stat="identity", fill = "steelblue") + 
 geom_text(data=d, aes(label=sprintf("%.0f", parks), x=country, y= parks), hjust=-0.5, size=3, color="white" ) +
 xlab(label="") + ylab(label="%%") +
 scale_y_continuous(breaks=c(-90,-80,-70,-60,-50,-40,-30,-20,-10, 0, 10, 20, 30, 40, 50)) +
 coord_flip()+
 ggtitle(sprintf ("Parks mobility trends"), subtitle=sprintf("%s", surl))
####
p4 <- ggplot(d, aes(x=reorder(country, transit),, y=transit)) + 
 geom_bar(stat="identity", fill = "steelblue") + 
 geom_text(data=d, aes(label=sprintf("%.0f", transit), x=country, y= transit), hjust=-0.5, size=3, color="white" ) +
 xlab(label="") + ylab(label="%%") +
 scale_y_continuous(breaks=c(-90,-80,-70,-60,-50,-40,-30,-20,-10, 0, 10, 20, 30, 40, 50)) +
 coord_flip()+
 ggtitle(sprintf ("Transit stations mobility trends"), subtitle=sprintf("%s", surl))

p5 <- ggplot(d, aes(x=reorder(country, work),, y=work)) + 
 geom_bar(stat="identity", fill = "steelblue") + 
 geom_text(data=d, aes(label=sprintf("%.0f", work), x=country, y= work), hjust=-0.5, size=3, color="white" ) +
 xlab(label="") + ylab(label="%%") +
 scale_y_continuous(breaks=c(-90,-80,-70,-60,-50,-40,-30,-20,-10, 0, 10, 20, 30, 40, 50)) +
 coord_flip()+
 ggtitle(sprintf ("Workplaces mobility trends"), subtitle=sprintf("%s", surl))

p6 <- ggplot(d, aes(x=reorder(country, residential), y=residential)) + 
 geom_bar(stat="identity", fill = "steelblue") + 
 geom_text(data=d, aes(label=sprintf("%.0f", residential), x=country, y= residential), hjust=1.4, size=3, color="white" ) +
 xlab(label="") + ylab(label="%%") +
 scale_y_continuous(breaks=c(-90,-80,-70,-60,-50,-40,-30,-20,-10, 0, 10, 20, 30, 40, 50)) +
 coord_flip()+
 ggtitle(sprintf ("Residential mobility trends"), subtitle=sprintf("%s", surl))

p12 <- ggarrange(p1, p2, ncol=2, nrow=1)
p34 <- ggarrange(p3, p4, ncol=2, nrow=1)
p56 <- ggarrange(p5, p6, ncol=2, nrow=1)

ggsave(plot=p12, "gmr_rr_12pl.png", width=9)
ggsave(plot=p34, "gmr_rr_34pl.png", width=9)
ggsave(plot=p56, "gmr_rr_56pl.png", width=9)

