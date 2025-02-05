climateManali
================
Trevor
2025-02-05

## R Markdown

In this document we are going to do all the climate analyses. First up
is hobo data from Manali, 2023-2024

``` r
setwd("~/Desktop/current does/currentpapersgrants/elevationPaper2024/climate/hobos20232024")
library(ggplot2)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
troch<-read.csv("Troch_camp_2023_2024.csv") #3400m
data<-as.POSIXlt(troch$Date.Time..IST., tz="Asia/Kolkata", format="%m/%d/%Y %H:%M:%OS")
troch<-data.frame(troch, data)
p1<-ggplot(troch, aes(x=data, y=Temperature....C, color=factor(year(data)))) +  geom_line()+scale_color_manual(values=c("black", "black"))
p2<-p1+theme(axis.title = element_text(size = 16)) + ylab(bquote("Hourly temperature"^o*C))+ xlab("Date")
p3<-p2+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill="white"), axis.line = element_line(colour = "black"))
p3
```

![](climateM_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->
