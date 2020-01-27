# Tanay Mukherjee
# 26th Jan, 2020

# install.packages(c("CarletonStats", "devtools", "epanetReader", "fmsb", "ggplot2", "ggthemes", 
#                    "latticeExtra", "MASS", "PerformanceAnalytics", "psych", 
#                    "plyr", "prettyR", "plotrix", "proto", "RCurl", "reshape", "reshape2"))


# Minimal line plot in base graphics

x <- 1967:1977
y <- c(0.5,1.8,4.6,5.3,5.3,5.7,5.4,5,5.5,6,5)
pdf(width=10, height=6)
plot(y ~ x, axes=F, xlab="", ylab="", pch=16, type="b")
axis(1, at=x, label=x, tick=F, family="serif")
axis(2, at=seq(1,6,1), label=sprintf("$%s", seq(300,400,20)), tick=F, las=2, family="serif")
abline(h=6,lty=2)
abline(h=5,lty=2)
text(max(x), min(y)*2.5,"Per capita\nbudget expanditures\nin constant dollars", adj=1, 
     family="serif")
text(max(x), max(y)/1.08, labels="5%", family="serif")
dev.off()

# Minimal line plot in lattice
library(lattice)
x <- 1967:1977
y <- c(0.5,1.8,4.6,5.3,5.3,5.7,5.4,5,5.5,6,5)
xyplot(y~x, xlab="", ylab="", pch=16, col=1, border = "transparent", type="o",
       abline=list(h = c(max(y),max(y)-1), lty = 2),
       scales=list(x=list(at=x,labels=x, fontfamily="serif", cex=1),
                   y=list(at=seq(1,6,1), fontfamily="serif", cex=1,
                          label=sprintf("$%s",seq(300,400,20)))),
       par.settings = list(axis.line = list(col = "transparent"), dot.line=list(lwd=0)),
       axis = function(side, line.col = "black", ...) {
         if(side %in% c("left","bottom")) {axis.default(side = side, line.col = "black", ...)}})
ltext(current.panel.limits()$xlim[2]/1.1, adj=1, fontfamily="serif", 
      current.panel.limits()$ylim[1]/1.3, cex=1,
      "Per capita\nbudget expandures\nin constant dollars")
ltext(current.panel.limits()$xlim[2]/1.1, adj=1, fontfamily="serif", 
      current.panel.limits()$ylim[1]/5.5, cex=1, "5%")

# Minimal line plot in ggplot2

library(ggplot2)
library(ggthemes)
x <- 1967:1977
y <- c(0.5,1.8,4.6,5.3,5.3,5.7,5.4,5,5.5,6,5)
d <- data.frame(x, y)
ggplot(d, aes(x,y)) + geom_line() + geom_point(size=3) + theme_tufte(base_size = 15) +
  theme(axis.title=element_blank()) + geom_hline(yintercept = c(5,6), lty=2) + 
  scale_y_continuous(breaks=seq(1, 6, 1), label=sprintf("$%s",seq(300,400,20))) + 
  scale_x_continuous(breaks=x,label=x) +
  annotate("text", x = c(1977,1977.2), y = c(1.5,5.5), adj=1,  family="serif",
           label = c("Per capita\nbudget expandures\nin constant dollars", "5%"))


# Minimal line plot - interactive plot with highcharter
library(highcharter)
library(digest)
library(dplyr)
x <- 1967:1977
y <- c(290,318,372,385,385,372,386,380,390,400,380)
d <- data.frame(x, y)
highchart() %>%
  hc_chart(type = "scatter") %>% 
  hc_subtitle(text = "Per capita budget expanditures in constant dollars") %>%
  hc_yAxis(labels = list(format = "${value}")) %>%
  hc_add_series(data = d) %>% 
  hc_add_theme(hc_theme_tufte())


# Range-frame (or quartile-frame) scatterplot

# Range frame plot in base graphics
x <- mtcars$wt
y <- mtcars$mpg
plot(x, y, main="", axes=FALSE, pch=16, cex=0.8, family="serif",
     xlab="Car weight (lb/1000)", ylab="Miles per gallon of fuel")
axis(1,at=summary(x),labels=round(summary(x),1), tick=F, family="serif")
axis(2,at=summary(y),labels=round(summary(y),1), tick=F, las=2, family="serif")

# Range frame plot in base graphics with fancyaxis
# library(devtools)
# source_url("https://raw.githubusercontent.com/sjmurdoch/fancyaxis/master/fancyaxis.R")
# x <- mtcars$wt
# y <- mtcars$mpg
# plot(x, y, main="", axes=FALSE, pch=16, cex=0.8,
#      xlab="Car weight (lb/1000)", ylab="Miles per gallon of fuel")
# fancyaxis(1, summary(x), digits=1)
# fancyaxis(2, summary(y), digits=1)


# Range frame plot in lattice
library(lattice)
x <- mtcars$wt
y <- mtcars$mpg
xyplot(y ~ x, mtcars, col=1, pch=16, fontfamily="serif",
       xlab="Car weight (lb/1000)", ylab="Miles per gallon of fuel",
       par.settings = list(axis.line = list(col="transparent"),
                           par.xlab.text=list(fontfamily="serif"),
                           par.ylab.text=list(fontfamily="serif")),
       scales = list(x=list(at=summary(mtcars$wt),labels=round(summary(mtcars$wt),1),
                            fontfamily="serif"),
                     y=list(at=summary(mtcars$mpg),labels=round(summary(mtcars$mpg),1),
                            fontfamily="serif")),
       axis = function(side, line.col = "black", ...) {
         if(side %in% c("left","bottom")) {axis.default(side = side, line.col = "black", ...)}})


# Range-frame plot in ggplot2
library(ggplot2)
library(ggthemes)
ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_rangeframe() + theme_tufte() +
  xlab("Car weight (lb/1000)") + ylab("Miles per gallon of fuel") + 
  theme(axis.title.x = element_text(vjust=-0.5), axis.title.y = element_text(vjust=1.5))


# Range-frame plot in ggplot2 with qfplot
library(devtools)
source_url('https://raw.githubusercontent.com/bearloga/Quartile-frame-Scatterplot/master/qfplot.R')
qfplot(x=mtcars$wt, y=mtcars$mpg, xlab="Car weight (lb/1000)", ylab="Miles per gallon of fuel")


# Dot-dash (or rug) scatterplot

# Dot-dash plot in base graphics with fancyaxis
# library(devtools)
# source_url("https://raw.githubusercontent.com/sjmurdoch/fancyaxis/master/fancyaxis.R")
# x <- mtcars$wt
# y <- mtcars$mpg
# plot(x, y, main="", axes=FALSE, pch=16, cex=0.8,
# xlab="Car weight (lb/1000)", ylab="Miles per gallon of fuel", 
# xlim=c(min(x)-0.2, max(x)+0.2),
# ylim=c(min(y)-1.5, max(y)+1.5))
# axis(1, tick=F)
# axis(2, tick=F, las=2)
# minimalrug(x, side=1, line=-0.8)
# minimalrug(y, side=2, line=-0.8)

# Dot-dash plot in lattice
library(lattice)
x <- mtcars$wt
y <- mtcars$mpg
xyplot(y ~ x, xlab="Car weight (lb/1000)", ylab="Miles per gallon of fuel",
       par.settings = list(axis.line = list(col="transparent")),
       panel = function(x, y,...) { 
         panel.xyplot(x, y, col=1, pch=16)
         panel.rug(x, y, col=1, x.units = rep("snpc", 2), y.units = rep("snpc", 2), ...)})


# Dot-dash plot in ggplot2
library(ggplot2)
library(ggthemes)
ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_rug() + theme_tufte(ticks=F) + 
  xlab("Car weight (lb/1000)") + ylab("Miles per gallon of fuel") + 
  theme(axis.title.x = element_text(vjust=-0.5), axis.title.y = element_text(vjust=1))


# Marginal histogram scatterplot

#Marginal histogram scatterplot in base graphics with fancyaxis

library(devtools)
source_url("https://raw.githubusercontent.com/sjmurdoch/fancyaxis/master/fancyaxis.R")
x <- faithful$waiting
y <- faithful$eruptions
plot(x, y, main="", axes=FALSE, pch=16, cex=0.8,
     xlab="Time till next eruption (min)", ylab="Duration (sec)", 
     xlim=c(min(x)/1.1, max(x)), ylim=c(min(y)/1.5, max(y)))
axis(1, tick=F)
axis(2, tick=F, las=2)
axisstripchart(faithful$waiting, 1)
axisstripchart(faithful$eruptions, 2)


# Marginal histogram scatterplot in ggplot2 with ggMarginal
library(ggplot2)
library(ggExtra)
library(ggthemes)
p <- ggplot(faithful, aes(waiting, eruptions)) + geom_point() + theme_tufte(ticks=F)
ggMarginal(p, type = "histogram", fill="transparent")

# However, ggMarginal can be also used to quickly create
# margin densityplots using the same function:

library(ggplot2)
library(ggExtra)
library(ggthemes)
p <- ggplot(faithful, aes(waiting, eruptions)) + geom_point() + theme_tufte(ticks=F) +
  theme(axis.title=element_blank(), axis.text=element_blank())
ggMarginal(p, type = "density")

# can also be used to create margin boxplots:
library(ggplot2)
library(ggExtra)
library(ggthemes)
p <- ggplot(faithful, aes(waiting, eruptions)) + geom_point() + theme_tufte(ticks=F) +
  theme(axis.title=element_blank(), axis.text=element_blank())
ggMarginal(p, type = "boxplot", size=10, fill="transparent")


# Minimal boxplot

# Minimal boxplot in base graphics
x <- quakes$mag
y <- quakes$stations
boxplot(y ~ x, main = "", axes = FALSE, xlab=" ", ylab=" ",
        pars = list(boxcol = "transparent", medlty = "blank", medpch=16, whisklty = c(1, 1),
                    medcex = 0.7,  outcex = 0, staplelty = "blank"))
axis(1, at=1:length(unique(x)), label=sort(unique(x)), tick=F, family="serif")
axis(2, las=2, tick=F, family="serif")
text(min(x)/3, max(y)/1.1, pos = 4, family="serif",
     "Number of stations \nreporting Richter Magnitude\nof Fiji earthquakes (n=1000)")


# Minimal boxplot in base graphics with chart.Boxplot

library(PerformanceAnalytics)
library(psych)
d <- msq[,80:84]
chart.Boxplot(d, main = "", xlab="average personality rating (based on n=3896)", ylab="", 
              element.color = "transparent", as.Tufte=TRUE)


#Minimal boxplot in lattice
x <- quakes$mag
y <- quakes$stations
bwplot(y ~ x, horizontal=F, xlab="", ylab="", do.out = FALSE, box.ratio = 0,
       scales=list(x=list(labels=sort(unique(x)), fontfamily="serif"),
                   y=list(fontfamily="serif")),
       par.settings = list(axis.line = list(col = "transparent"), box.umbrella=list(lty=1, col= 1),
                           box.dot=list(col= 1), box.rectangle = list(col= c("transparent"))))
ltext(current.panel.limits()$xlim[1]+250, adj=1,
      current.panel.limits()$ylim[2]+50, fontfamily="serif",
      "Number of stations \nreporting Richter Magnitude\nof Fiji earthquakes (n=1000)")


# Minimal boxplot in ggplot2
library(ggplot2)
library(ggthemes)
ggplot(quakes, aes(factor(mag),stations)) + theme_tufte() +
  geom_tufteboxplot(outlier.colour="transparent") + theme(axis.title=element_blank()) +
  annotate("text", x = 8, y = 120, adj=1,  family="serif",
           label = c("Number of stations \nreporting Richter Magnitude\nof Fiji earthquakes (n=1000)"))


# Minimal barchart

# library(psych)
d <- colMeans(msq[,c(2,7,34,36,42,43,46,55,68)], na.rm = T)*10
barplot(d, xaxt="n", yaxt="n", ylab="", border=F, width=c(.35), space=1.8)
axis(1, at=(1:length(d))-.26, labels=names(d), tick=F, family="serif")
axis(2, at=seq(1, 5, 1), las=2, tick=F, family="serif")
abline(h=seq(1, 5, 1), col="white", lwd=3)
abline(h=0, col="gray", lwd=2)
text(min(d)/2, max(d)/1.2, pos = 4, family="serif",
     "Average scores\non negative emotion traits\nfrom 3896 participants\n(Watson et al., 1988)")

# Minimal barchart in lattice

library(lattice)
library(psych)
d <- colMeans(msq[,c(2,7,34,36,42,43,46,55,68)],na.rm = T)*10
barchart(sort(d), xlab="", ylab="", col = "grey", origin=1,  
         border = "transparent", box.ratio=0.5, 
         panel = function(x,y,...) {
           panel.barchart(x,y,...)
           panel.abline(v=seq(1,6,1), col="white", lwd=3)},
         par.settings = list(axis.line = list(col = "transparent")))
ltext(current.panel.limits()$xlim[2]-50, adj=1,  
      current.panel.limits()$ylim[1]-100,
      "Average scores\non negative emotion traits\nfrom 3896 participants\n(Watson et al., 1988)")

# Minimal barchart in ggplot2

library(ggplot2)
library(ggthemes)
library(psych)
library(reshape2)
d <- melt(colMeans(msq[,c(2,7,34,36,42,43,46,55,68)],na.rm = T)*10)
d$trait <- rownames(d)
ggplot(d, aes(x=trait, y=value)) + theme_tufte(base_size=14, ticks=F) +
  geom_bar(width=0.25, fill="gray", stat = "identity") +  theme(axis.title=element_blank()) +
  scale_y_continuous(breaks=seq(1, 5, 1)) + 
  geom_hline(yintercept=seq(1, 5, 1), col="white", lwd=1) +
  annotate("text", x = 3.5, y = 5, adj=1,  family="serif",
           label = c("Average scores\non negative emotion traits
          from 3896 participants\n(Watson et al., 1988)"))

# Minimal barchart - interactive with highcharter
library(psych)
library(reshape)
library(highcharter)
values <- 1 + abs(rnorm(12))
d <- melt(colMeans(msq[,c(2,7,34,36,42,43,46,55,68)], na.rm = T)*10)
trait <- row.names(d) 
value <- as.vector(d[,1])
highchart() %>%
  hc_chart(type = "column") %>%
  hc_add_series(data = value) %>%
  hc_xAxis(categories = row.names(d)) %>%
  hc_add_theme(hc_theme_tufte2())



# Slopegraph

# Slopegraph in base graphics

library(devtools)
#install_github("leeper/slopegraph")#install Leeper's package from Github
library(slopegraph)
data(cancer)
slopegraph(cancer, col.lines = 'gray', col.lab = 1, col.num = 1,
           xlim = c(-.2,5),
           main = "Estimate of % survival rates",
           xlabels = c('5 Year','10 Year','15 Year','20 Year'))

# Slopegraph in ggplot2 with plot_slopegraph
# library(ggplot2)
# library(ggthemes)
# library(devtools)
# library(RCurl)
# library(plyr)
# source_url("https://raw.githubusercontent.com/jkeirstead/r-slopegraph/master/slopegraph.r")
# d <- read.csv(text = getURL("https://raw.githubusercontent.com/jkeirstead/r-slopegraph/master/cancer_survival_rates.csv"))
# df <- build_slopegraph(d, x="year", y="value", group="group", method="tufte", min.space=0.04)
# df <- transform(df, x=factor(x, levels=c(5,10,15,20),
#                              labels=c("5 years","10 years","15 years","20 years")), y=round(y))
# plot_slopegraph(df) + labs(title="Estimates of % survival rates") +
#   theme_tufte(base_size=16, ticks=F) + theme(axis.title=element_blank())


# Sparklines

# Sparklines in base graphics

library(RCurl)
dd <- read.csv(text = getURL("https://gist.githubusercontent.com/GeekOnAcid/da022affd36310c96cd4/raw/9c2ac2b033979fcf14a8d9b2e3e390a4bcc6f0e3/us_nr_of_crimes_1960_2014.csv"))
d <- dd[,c(2:11)]
pdf("sparklines_base.pdf", height=10, width=6)
par(mfrow=c(ncol(d),1), mar=c(1,0,0,8), oma=c(4,1,4,4))
for (i in 1:ncol(d)){
  plot(d[,i], lwd=0.5, axes=F, ylab="", xlab="", main="", type="l", new=F)
  axis(4, at=d[nrow(d),i], labels=round(d[nrow(d),i]), tick=F, las=1, line=-1.5, 
       family="serif", cex.axis=1.2)
  axis(4, at=d[nrow(d),i], labels=names(d[i]), tick=F, line=1.5, 
       family="serif", cex.axis=1.4, las=1)
  text(which.max(d[,i]), max(d[,i]), labels=round(max(d[,i]),0), 
       family="serif", cex=1.2, adj=c(0.5,3))
  text(which.min(d[,i]), min(d[,i]), labels=round(min(d[,i]),0), 
       family="serif", cex=1.2, adj=c(0.5,-2.5))
  ymin <- min(d[,i]); tmin <- which.min(d[,i]); ymax<-max(d[,i]); tmax<-which.max(d[,i]);
  points(x=c(tmin,tmax), y=c(ymin,ymax), pch=19, col=c("red","blue"), cex=1)
  rect(0, summary(d[,i])[2], nrow(d), summary(d[,i])[4], border=0, 
       col = rgb(190, 190, 190, alpha=90, maxColorValue=255))}
axis(1, at=1:nrow(dd), labels=dd$Year, pos=c(-5), tick=F, family="serif", cex.axis=1.4)
dev.off()


# Sparklines in base graphics with plotSparklineTable
library(epanetReader)
library(reshape)
library(RCurl)
dd <- read.csv(text = getURL("https://gist.githubusercontent.com/GeekOnAcid/da022affd36310c96cd4/raw/9c2ac2b033979fcf14a8d9b2e3e390a4bcc6f0e3/us_nr_of_crimes_1960_2014.csv"))
d <- melt(dd[,c(2:11)])
pdf("sparklines_base_epanetReader.pdf", height=6, width=10)
plotSparklineTable(d, row.var = 'variable', col.vars = 'value')


# Sparklines in lattice
library(lattice)
library(latticeExtra)
library(grid)
library(reshape)
library(RCurl)
dd <- read.csv(text = getURL("https://gist.githubusercontent.com/GeekOnAcid/da022affd36310c96cd4/raw/9c2ac2b033979fcf14a8d9b2e3e390a4bcc6f0e3/us_nr_of_crimes_1960_2014.csv"))
d <- melt(dd, id="Year")
names(d)[1] <- "time"
pdf("sparklines_lattice.pdf", height=10, width=8)
xyplot(value~time | variable, d, xlab="", ylab="", strip=F, lwd=0.7, col=1, type="l",
       layout=c(1,length(unique(d$variable))), between = list(y = 1),
       scales=list(y=list(at=NULL, relation="free"), x=list(fontfamily="serif")),
       par.settings = list(axis.line = list(col = "transparent"),
                           layout.widths=list(right.padding=20, left.padding=-5)),
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         pushViewport(viewport(xscale=current.viewport()$xscale-5,
                               yscale=current.viewport()$yscale, clip="off"))
         panel.text(x=tail(x,n=1), y=tail(y,n=1), labels=levels(d$variable)[panel.number()],
                    fontfamily="serif", pos=4)
         popViewport()
         panel.text(x=x[which.max(y)], y=max(y), labels=round(max(y),0), cex=0.8,
                    fontfamily="serif",adj=c(0.5,2.5))
         panel.text(x=x[which.min(y)], y=min(y), labels=round(min(y),0), cex=0.8,
                    fontfamily="serif",adj=c(0.5,-1.5))
         panel.text(x=tail(x,n=1), y=tail(y,n=1), labels=round(tail(y,n=1),0), cex=0.8,
                    fontfamily="serif", pos=4)
         panel.points(x[which.max(y)], max(y),  pch=16, cex=1)
         panel.points(x[which.min(y)], min(y),  pch=16, cex=1, col="red")
         panel.rect(min(x), quantile(y, 0.25), max(x), quantile(y, 0.75),
                    col = "grey", border = "transparent", alpha = 0.4)
       })
dev.off()

# Sparklines in ggplot2
library(ggplot2)
library(ggthemes)
library(dplyr)
library(reshape)
library(RCurl)
dd <- read.csv(text = getURL("https://gist.githubusercontent.com/GeekOnAcid/da022affd36310c96cd4/raw/9c2ac2b033979fcf14a8d9b2e3e390a4bcc6f0e3/us_nr_of_crimes_1960_2014.csv"))
d <- melt(dd, id="Year")
names(d) <- c("Year","Crime.Type","Crime.Rate")
d$Crime.Rate <- round(d$Crime.Rate,0)
mins <- group_by(d, Crime.Type) %>% slice(which.min(Crime.Rate))
maxs <- group_by(d, Crime.Type) %>% slice(which.max(Crime.Rate))
ends <- group_by(d, Crime.Type) %>% filter(Year == max(Year))
quarts <- d %>% group_by(Crime.Type) %>%
  summarize(quart1 = quantile(Crime.Rate, 0.25),
            quart2 = quantile(Crime.Rate, 0.75)) %>%
  right_join(d)
pdf("sparklines_ggplot.pdf", height=10, width=8)
ggplot(d, aes(x=Year, y=Crime.Rate)) + 
  facet_grid(Crime.Type ~ ., scales = "free_y") + 
  geom_ribbon(data = quarts, aes(ymin = quart1, max = quart2), fill = 'grey90') +
  geom_line(size=0.3) +
  geom_point(data = mins, col = 'red') +
  geom_point(data = maxs, col = 'blue') +
  geom_text(data = mins, aes(label = Crime.Rate), vjust = -1) +
  geom_text(data = maxs, aes(label = Crime.Rate), vjust = 2.5) +
  geom_text(data = ends, aes(label = Crime.Rate), hjust = 0, nudge_x = 1) +
  geom_text(data = ends, aes(label = Crime.Type), hjust = 0, nudge_x = 5) +
  expand_limits(x = max(d$Year) + (0.25 * (max(d$Year) - min(d$Year)))) +
  scale_x_continuous(breaks = seq(1960, 2010, 10)) +
  scale_y_continuous(expand = c(0.1, 0)) +
  theme_tufte(base_size = 15, base_family = "Helvetica") +
  theme(axis.title=element_blank(), axis.text.y = element_blank(), 
        axis.ticks = element_blank(), strip.text = element_blank())
dev.off()


# Stem-and-leaf display

#Stem-and-leaf display in console with base graphics
stem(faithful$eruptions)

# Stem-and-leaf display in console with CarletonStats
library(CarletonStats)
library(MASS)
stemPlot(birthwt$bwt, birthwt$smoke, varname="infant birth weight (in grams)",
         grpvarname="whether mother smoked during pregnancy (1) or not (0)")


# Stem-and-leaf display in base graphics with fmsb
library(fmsb)
gstem(faithful$eruptions)
