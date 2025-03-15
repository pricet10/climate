climateManali
================
Trevor
2025-03-15

## R Markdown

In this document we are going to do all the climate analyses, and add
all the figures. First up is hobo data from Manali, 2023-2024. Do note
we are only using a subset for the range 3000-3400m

``` r
# functionshead(tr)
drawline<-function(data, col, lwd)
{x<-c(min(data[,2]), max(data[,2]))
y<-lm(data[,1]~data[,2])$coefficients[2]*x+ lm(data[,1]~data[,2])$coefficients[1]
 lines(y~x, col=col, lwd=2)}

prettyplot<-function(p1) {p2<-p1+theme(axis.title = element_text(size = 16)) + ylab(bquote("Mean Temperature "^o*C))+ xlab("Date")
p3<-p2+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill="white"), axis.line = element_line(colour = "black"), legend.position="none")
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

data<-as.POSIXlt(troch$Date.Time..IST., tz="Asia/Kolkata", format="%m/%d/%Y %H:%M")
troch<-data.frame(troch, as.Date(104, origin = "2014-01-01"), data, floor(as.numeric(julian(data)))-19357, hour(data), year(data), month(data))
colnames(troch)[c(7:10)]<-c("Days_Jan1_2023","hours","year","month")
```

``` r
# try2<-ggplot(troch, aes(x=data, y=Temperature, color=factor(year(data))))  +stat_summary(fun = function(z) { quantile(z,0.75, na.rm=T) }, geom="line") #note this is a tril, and combines all elevations at present. Included to show how month works, and function

troch<-subset(troch, month(data) >=5 & month(data) <7)
troch<-subset(troch, elevn_gnd=="3000_1m"|elevn_gnd=="3200_1m"|elevn_gnd=="3400_1m")
troch$elev<-as.numeric(gsub("_1m","", troch$elevn_gnd))
# troch<-subset(troch, hours > 19 | hours < 6)#ran with just night temp to confirm robust

dailymeantemp<-aggregate.data.frame(cbind(troch[,c(2,3,10)]), by=list(year(troch$data), yday(troch$data), troch$elevn_gnd), mean)
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

![](climateM_files/figure-gfm/Making%20the%20graphs%20of%20lapse%20rate%20for%20supplement-1.png)<!-- -->

``` r
with(y2023, summary(lm(Temperature~day+Elevation)))
```

    ## 
    ## Call:
    ## lm(formula = Temperature ~ day + Elevation)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.7636 -2.1423  0.1281  2.2066  6.4163 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -7.4714     3.0657  -2.437   0.0161 *  
    ## day           0.1381     0.0336   4.110 6.71e-05 ***
    ## Elevation    -0.5378     0.9574  -0.562   0.5752    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.028 on 140 degrees of freedom
    ## Multiple R-squared:  0.2411, Adjusted R-squared:  0.2303 
    ## F-statistic: 22.24 on 2 and 140 DF,  p-value: 4.096e-09

``` r
with(y2024, summary(lm(Temperature~day+Elevation)))
```

    ## 
    ## Call:
    ## lm(formula = Temperature ~ day + Elevation)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.4160 -0.9316  0.2909  1.1796  3.6284 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 12.80453    1.75339   7.303 1.76e-11 ***
    ## day          0.09209    0.01947   4.729 5.33e-06 ***
    ## Elevation   -2.48751    0.56524  -4.401 2.09e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.79 on 144 degrees of freedom
    ## Multiple R-squared:  0.1374, Adjusted R-squared:  0.1255 
    ## F-statistic: 11.47 on 2 and 144 DF,  p-value: 2.382e-05

``` r
#Merra2200m<-read.csv("~/Desktop/current does/currentpapersgrants/elevationPaper2024/climate/datafiles/Manali_Merra2200m.csv")
#str(Merra2200m)
Merra3534m<-read.csv("~/Desktop/current does/currentpapersgrants/elevationPaper2024/climate/datafiles/Kalingcha_Merra3534.csv")
#for Manali:
#rainspring<-subset(Merra2200m, (MO==4 & DY>14) | (MO==5 & DY<16))
#for Kalingcha:
  rainspring<-subset(Merra3534m, (DOY>104 & DOY<135))


rains<-data.frame(tapply(rainspring$PRECTOTCORR, rainspring$YEAR, sum), tapply(rainspring$PRECTOTCORR, rainspring$YEAR, max), tapply(rainspring$PRECTOTCORR, rainspring$YEAR, function(x) sum(x==0)) )
colnames(rains)<-(c("sum","max","no rain day"))

Temperature<-data.frame(tapply(rainspring$T2M, rainspring$YEAR, mean), tapply(rainspring$T2M, rainspring$YEAR, max), tapply(rainspring$T2M, rainspring$YEAR, min))

colnames(Temperature)<-(c("meanT","maxT","minT"))

Merra<-data.frame(rains, Temperature)
Merra$Year<-as.numeric(rownames(Merra))
with(Merra, plot(meanT~Year, type="l", ylab="Daily Mean Temperature", las=1, bty="l"))
```

![](climateM_files/figure-gfm/Making%20the%20graphs%20of%20Manali%20and%20Kalingcha,%20Merra%20data-1.png)<!-- -->

``` r
with(Merra, plot(sum~Year, type="l", ylab="Precipitation, mm", las=1, bty="l"))
```

![](climateM_files/figure-gfm/Making%20the%20graphs%20of%20Manali%20and%20Kalingcha,%20Merra%20data-2.png)<!-- -->

note: correlation of MerraManali and Kullu over 37 years since 1980 is
only r = 0.55, MerraKalingcha and Kullu r = 0.6; correlation with
DelhiSaf is ~r=0.5

``` r
kullu<-read.csv("~/Desktop/current does/currentpapersgrants/elevationPaper2024/climate/datafiles/42081_Table_2_Daily_NDCQ-2024-12-221.csv")
str(kullu)
```

    ## 'data.frame':    16578 obs. of  40 variables:
    ##  $ INDEX      : int  42081 42081 42081 42081 42081 42081 42081 42081 42081 42081 ...
    ##  $ YEAR       : int  1971 1971 1971 1971 1971 1971 1971 1971 1971 1971 ...
    ##  $ MN         : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ DT         : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ MAX        : num  18.7 15.5 17.4 18.8 18.4 18.3 18.9 20.1 18.2 19.9 ...
    ##  $ MIN        : num  -3.5 -3.3 -2.8 -2.3 -0.4 -1.4 -2.1 -2.1 -1.6 -0.9 ...
    ##  $ AW         : int  4 3 3 3 7 6 3 4 3 6 ...
    ##  $ RF         : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ EVP        : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ DRNRF.hrs. : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ DRNRF.mnts.: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ SSH        : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ TS         : logi  NA NA NA NA NA NA ...
    ##  $ N          : logi  NA NA NA NA NA NA ...
    ##  $ FFF        : logi  NA NA NA NA NA NA ...
    ##  $ D          : logi  NA NA NA NA NA NA ...
    ##  $ TOCSQ      : logi  NA NA NA NA NA NA ...
    ##  $ DU         : logi  NA NA NA NA NA NA ...
    ##  $ RA         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ DZ         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ SN         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ SL         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ HA         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ TH         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ DS         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ FG         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ GA         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ T          : chr  NA NA NA NA ...
    ##  $ G          : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ DUR        : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ T.1        : chr  NA NA NA NA ...
    ##  $ G.1        : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ DUR.1      : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ T.2        : chr  NA NA NA NA ...
    ##  $ G.2        : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ DUR.2      : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ T.3        : chr  NA NA NA NA ...
    ##  $ G.3        : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ DUR.3      : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ X          : logi  NA NA NA NA NA NA ...

``` r
kullu<- subset(kullu, (MN==4 & DT>14) | (MN==5 & DT<16))

TotalRain<-tapply(kullu$RF, kullu$YEAR, function(x) sum(x, na.rm=T))
MeanMaxTemp<- tapply(kullu$MAX, kullu$YEAR, function(x) mean(x, na.rm=T))
kulludaily<-data.frame(TotalRain, MeanMaxTemp)
kulludaily$Year<-as.numeric(rownames(kulludaily))
kulludaily$timeperiod<-cut(kulludaily$Year, c(1970,1993,2019, 2025))

imd_merra<-merge(kulludaily,Merra)
```

daily rain in June; should repeat for Manali

``` r
kullu<-read.csv("~/Desktop/current does/currentpapersgrants/elevationPaper2024/climate/datafiles/42081_Table_2_Daily_NDCQ-2024-12-221.csv")
kullu<- subset(kullu, MN==6)
kullu<-subset(kullu, YEAR==2001|YEAR==2003|YEAR==2004|YEAR==2023)
p1<-ggplot(kullu, aes(x=DT, y=RF, color=factor(YEAR))) + geom_line(show.legend = T) + scale_color_manual(values=c("red", "blue", "purple", "yellow") ) +ggtitle("Rain in June")
p3<-p1+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill="white"), axis.line = element_line(colour = "black"))
yday(as.Date('2023-06-01',format='%Y-%m-%d'))
```

    ## [1] 152

Below we make figure of Drent and Daan

``` r
x <- seq(-3, 3, length=1000)
y <- dnorm(x, mean=0, sd=1)
plot(x,y, type="l", ylim=c(0,0.5), xlim=c(-3,3), las=1, bty="l", col="grey", lwd=2, ylab="Relative fitness", xlab="Breeding date")
yl<-0.05+0.07*x
yh<-0.2+0.07*x
lines(x,yl, col="#440154FF")
lines(x,yh, col="red")
lines(x,yl*y*3, col="#440154FF", lty=2)
lines(x,yh*y*3, col="red", lty=2)
```

![](climateM_files/figure-gfm/Figure%20DrentDaan%20left-1.png)<!-- -->

``` r
#Drent and Daan right. the linear function is y=ax+b. guassian is standard normal. max is given from differentiation
computeMax<-function(a,b) (sqrt(4*b*b+a*a)-a)/(2*b)
library(ggforce)
avec<-seq(0.02, 0.2, 0.005)
max<-computeMax(avec, 0.07)
opt<-data.frame(avec, max)
p1<-ggplot(opt, aes(x=avec, y=max, color = avec)) + ggforce::geom_link2(lwd=2) + 
  scale_color_gradient(low = "red", high = "black")+ylim(c(0,1))
p3<-prettyplot(p1)
```

``` r
 computemeans<-function(x,y) sum(x*y)/sum(y)
cl<-read.csv("~/Desktop/current does/currentpapersgrants/elevationPaper2024/presentationUBC/humeiclutchkashmir.csv")
day_in_year<-yday(as.Date(cl$averagelay.date,format='%m/%d/%Y'))
year<-year(as.Date(cl$averagelay.date,format='%m/%d/%Y'))
cl<-data.frame(cl,day_in_year, year)

p1<-ggplot(cl, aes(x=day_in_year, y=clutch.size, color=factor(year))) +geom_jitter(cex=cl$numbers/5, width=0.5, height=0)
#avgday<-sapply(cl, function(x) computemeans(x[,4], x[,3]))
#avgclutch<-sapply(cl, function(x) computemeans(x[,1], x[,3]))
```
