condition
================
Trevor
2025-02-19

functions

``` r
library(ggplot2)
library(ggpubr)
library(viridis)
```

    ## Loading required package: viridisLite

``` r
#to make graph pretty
p2<-function(sp) sp+theme(axis.title = element_text(size = 10), axis.text = element_text(size = 10), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill="white"), axis.line = element_line(colour = "black"))
```

reading in the data and making means

``` r
measures<-read.csv("~/Desktop/current does/currentpapersgrants/elevationPaper2024/combineddatafiles/humeimeasures.csv")
meas<-measures[,c(1,3,10,15,17,20,21,22,24,26,27,28,30,31,c(58:61))]
meas<-subset(meas, sex=="c")
meas$sample[(meas$site=="ks" & meas$year<1988) | (meas$site=="ks" & meas$year==2011) | (meas$site=="kl" & meas$year==2008) | (meas$site=="mn" & meas$year>2000 & meas$year<2005) |(meas$site=="mn" & meas$year>2022)]<-1
#meas<-subset(meas, sample==1)


nestDay<-tapply(meas$fledge.days.jan, meas$nest, function(x) mean(x, na.rm=T))
nestElev<-tapply(meas$nest.elev, meas$nest, function(x) mean(x, na.rm=T))
nestYear<-tapply(meas$year, meas$nest, function(x) mean(x, na.rm=T))
nestTarsus<-tapply(meas$tarsus, meas$nest, function(x) mean(x, na.rm=T))
nestloc<-tapply(meas$site, meas$nest, function(x) x[1])
nestTs<-data.frame(nestDay,nestElev,nestloc, nestYear,nestTarsus)
table(nestTs$nestYear,nestloc)
```

    ##       nestloc
    ##        kl ks mn
    ##   1985  0 51  0
    ##   1986  0 54  0
    ##   1987  0 47  0
    ##   1989  0  1  0
    ##   1990  0  7  0
    ##   1994  0  0  2
    ##   1996  0  0  8
    ##   2001  0  0 29
    ##   2003  0  0 32
    ##   2004  0  0 26
    ##   2007  3  0  8
    ##   2008 26  5  6
    ##   2009  3  0  0
    ##   2011  0 14  0
    ##   2017  0  0  9
    ##   2023  0  0 11
    ##   2024  0  0 20

making the plots

    ## Warning: Removed 11 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 11 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](condition_files/figure-gfm/plots-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
