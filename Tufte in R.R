# Tanay Mukherjee
# 26th Jan, 2020

# install.packages(c("CarletonStats", "devtools", "epanetReader", "fmsb", "ggplot2", "ggthemes", 
#                    "latticeExtra", "MASS", "PerformanceAnalytics", "psych", "highcharter", "digest",
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

