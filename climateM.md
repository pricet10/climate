climateManali
================
Trevor
2025-02-07

## R Markdown

In this document we are going to do all the climate analyses. First up
is hobo data from Manali, 2023-2024

``` r
# functions
drawline<-function(data, col, lwd)
{x<-c(min(data[,2]), max(data[,2]))
y<-lm(data[,1]~data[,2])$coefficients[2]*x+ lm(data[,1]~data[,2])$coefficients[1]
 lines(y~x, col=col, lwd=2)}
```

``` r
library(scales)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(ggplot2)

setwd("~/Desktop/current does/currentpapersgrants/elevationPaper2024/climate/hobos20232024")
troch<-read.csv("Troch_camp_2023_2024.csv") #3400m
data<-as.POSIXlt(troch$Date.Time..IST., tz="Asia/Kolkata", format="%m/%d/%Y %H:%M:%OS")
troch<-data.frame(troch, data)
```

``` r
p1<-ggplot(troch, aes(x=data, y=Temperature....C, color=factor(year(data)))) +  geom_line(show.legend = FALSE)+scale_color_manual(values=c("black", "black"))
p2<-p1+theme(axis.title = element_text(size = 16)) + ylab(bquote("Hourly temperature"^o*C))+ xlab("Date")
p3<-p2+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill="white"), axis.line = element_line(colour = "black"))
p3
```

![](climateM_files/figure-gfm/unnamed-chunk-3-1.png)<!-- --> now we are
going to get May-July for each year

``` r
#making the plot
troch<-subset(troch, month(data) >=5 & month(data) <=7)
troch$days<-as.Date(format(troch$data,"%m-%d-2015"),format="%m-%d-%y") #dummy variable for x axis labels
ggplot(troch, aes(x=days, y=Temperature....C, color=factor(year(data)))) + stat_summary(fun=mean, geom = "line") + scale_x_date(labels = date_format("%b"))
```

![](climateM_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggplot(troch, aes(x=days, y=Temperature....C, color=factor(year(data))))  +stat_summary(fun = function(z) { quantile(z,0.75, na.rm=T) }, geom="line") + scale_x_date(labels = date_format("%b"))
```

![](climateM_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
troch<-subset(troch, month(data) >=5 & month(data) <7)
dailymeantemp<-aggregate.data.frame(cbind(troch[,c(3,4)]), by=list(year(troch$data), yday(troch$data)), function(x) mean(x, na.rm=T))
colnames(dailymeantemp)<-c("Year", "day", "Temperature", "Light")

y2023<-subset(dailymeantemp, Year==2023)
y2024<-subset(dailymeantemp, Year==2024)
y2024<-subset(y2024, day>133)
plot(y2023$Temperature~y2023$day, type="l", ylim=c(0,18), bty="l")
lines(y2024$Temperature~y2024$day, col='red')
drawline(y2023[,c(3,2)], "black", 2)
drawline(y2024[,c(3,2)], "red", 2)
```

![](climateM_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
