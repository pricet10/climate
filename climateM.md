climateManali
================
Trevor
2025-02-07

## R Markdown

In this document we are going to do all the climate analyses. First up
is hobo data from Manali, 2023-2024

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

![](climateM_files/figure-gfm/unnamed-chunk-2-1.png)<!-- --> now we are
going to get May-July for each year

``` r
#making the plot
troch<-subset(troch, month(data) >=5 & month(data) <=7)
troch$days<-as.Date(format(troch$data,"%m-%d-2015"),format="%m-%d-%y") #dummy variable for x axis labels
ggplot(troch, aes(x=days, y=Temperature....C, color=factor(year(data)))) + stat_summary(fun=mean, geom = "line") + scale_x_date(labels = date_format("%b"))
```

![](climateM_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggplot(troch, aes(x=days, y=Temperature....C, color=factor(year(data))))  +stat_summary(fun = function(z) { quantile(z,0.75, na.rm=T) }, geom="line") + scale_x_date(labels = date_format("%b"))
```

![](climateM_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
troch<-subset(troch, month(data) >=5 & month(data) <7)
head(troch)
```

    ##   number     Date.Time..IST. Temperature....C Light...lux                data
    ## 1      1 05/14/2023 06:47:52            13.60     7400.96 2023-05-14 06:47:52
    ## 2      2 05/14/2023 07:47:52            11.67    13987.84 2023-05-14 07:47:52
    ## 3      3 05/14/2023 08:47:52            10.94    15037.44 2023-05-14 08:47:52
    ## 4      4 05/14/2023 09:47:52            14.37    25098.24 2023-05-14 09:47:52
    ## 5      5 05/14/2023 10:47:52            15.36    25630.72 2023-05-14 10:47:52
    ## 6      6 05/14/2023 11:47:52            18.19    32051.20 2023-05-14 11:47:52
    ##         days
    ## 1 2020-05-14
    ## 2 2020-05-14
    ## 3 2020-05-14
    ## 4 2020-05-14
    ## 5 2020-05-14
    ## 6 2020-05-14
