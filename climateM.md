climateManali
================
Trevor
2025-02-08

## R Markdown

In this document we are going to do all the climate analyses. First up
is hobo data from Manali, 2023-2024. Do note we are only using a subset
for the range 3000-3400m

``` r
# functionshead(tr)
drawline<-function(data, col, lwd)
{x<-c(min(data[,2]), max(data[,2]))
y<-lm(data[,1]~data[,2])$coefficients[2]*x+ lm(data[,1]~data[,2])$coefficients[1]
 lines(y~x, col=col, lwd=2)}

prettyplot<-function(p1) {p2<-p1+theme(axis.title = element_text(size = 16)) + ylab(bquote("Mean Temperature "^o*C))+ xlab("Date")
p3<-p2+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill="white"), axis.line = element_line(colour = "black"))
return(p3)}
```

Here we create a figure for the supplement of lapse rates in 2023 and
2024 note other data, such as JS6 datalogger not been examined

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
library(gridExtra)

#as.Date(x, origin) useful
troch<-read.csv("~/Desktop/current does/currentpapersgrants/elevationPaper2024/climate/hobos20232024/hobo2023_2024.csv")

data<-as.POSIXlt(troch$Date.Time..IST., tz="Asia/Kolkata", format="%m/%d/%Y %H:%M:%OS")
troch<-data.frame(troch, data, floor(as.numeric(julian(data)))-19357, hour(data), year(data), month(data))
colnames(troch)[c(7,8,9,10)]<-c("Days_Jan1_2023","hours","year","month")
```

``` r
# try2<-ggplot(troch, aes(x=data, y=Temperature, color=factor(year(data))))  +stat_summary(fun = function(z) { quantile(z,0.75, na.rm=T) }, geom="line") #note this is a tril, and combines all elevations at present. Included to show how month works, and function

troch<-subset(troch, month(data) >=5 & month(data) <7)
troch<-subset(troch, elevn_gnd=="3000_1m"|elevn_gnd=="3200_1m"|elevn_gnd=="3400_1m")
troch$elev<-as.numeric(gsub("_1m","", troch$elevn_gnd))
# troch<-subset(troch, hours > 19 | hours < 6)#ran with just night temp to confirm robust

dailymeantemp<-aggregate.data.frame(cbind(troch[,c(3,4,11)]), by=list(year(troch$data), yday(troch$data), troch$elevn_gnd), mean)
colnames(dailymeantemp)<-c("Year", "day", "Location","Temperature", "Light", "Elevation")

y2023<-subset(dailymeantemp, Year==2023)
y2024<-subset(dailymeantemp, Year==2024)
y2024<-subset(y2024, day>133)
p1<-ggplot(y2023, aes(x=day, y=Temperature, color=factor(Location))) + geom_line(show.legend = FALSE)+ geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) + scale_color_manual(values=c("red", "blue", "purple") )+ylim(c(0,20)) +ggtitle("Y2023")
y23<-prettyplot(p1)
p1<-ggplot(y2024, aes(x=day, y=Temperature, color=factor(Location))) + geom_line(show.legend = FALSE)+ geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) + scale_color_manual(values=c("red", "blue", "purple") )+ylim(c(0,20))+ggtitle("Y2024")
y24<-prettyplot(p1)
grid.arrange(y23, y24, ncol=2)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](climateM_files/figure-gfm/Making%20the%20groahs%20of%20lapse%20rate%20for%20supplement-1.png)<!-- -->

``` r
with(y2023, summary(lm(Temperature~day+Elevation)))
```

    ## 
    ## Call:
    ## lm(formula = Temperature ~ day + Elevation)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.4679 -2.1510  0.0558  2.2151  6.3947 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  2.860639   5.709662   0.501   0.6171    
    ## day          0.121712   0.018115   6.719 4.25e-10 ***
    ## Elevation   -0.003366   0.001529  -2.201   0.0294 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.981 on 140 degrees of freedom
    ## Multiple R-squared:  0.2648, Adjusted R-squared:  0.2543 
    ## F-statistic: 25.22 on 2 and 140 DF,  p-value: 4.43e-10

``` r
with(y2024, summary(lm(Temperature~day+Elevation)))
```

    ## 
    ## Call:
    ## lm(formula = Temperature ~ day + Elevation)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.5877 -1.0642  0.3053  1.3072  3.6746 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 22.6567283  3.3464699   6.770 3.02e-10 ***
    ## day          0.0197499  0.0104801   1.885   0.0615 .  
    ## Elevation   -0.0038696  0.0009076  -4.264 3.62e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.797 on 144 degrees of freedom
    ## Multiple R-squared:  0.1311, Adjusted R-squared:  0.119 
    ## F-statistic: 10.86 on 2 and 144 DF,  p-value: 4.03e-05
