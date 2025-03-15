condition
================
Trevor
2025-03-15

functions

``` r
library(ggplot2)
library(ggpubr)
library(viridis)
```

    ## Loading required package: viridisLite

``` r
library(R2jags)
```

    ## Loading required package: rjags

    ## Loading required package: coda

    ## Linked to JAGS 4.3.2

    ## Loaded modules: basemod,bugs

    ## 
    ## Attaching package: 'R2jags'

    ## The following object is masked from 'package:coda':
    ## 
    ##     traceplot

``` r
library(jagsUI)
```

    ## 
    ## Attaching package: 'jagsUI'

    ## The following objects are masked from 'package:R2jags':
    ## 
    ##     autojags, jags, traceplot

    ## The following object is masked from 'package:coda':
    ## 
    ##     traceplot

``` r
library(MCMCvis)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.4     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(beepr)
library(tictoc)
#to make graph pretty
p2<-function(sp) sp+theme(axis.title = element_text(size = 10), axis.text = element_text(size = 10), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill="white"), axis.line = element_line(colour = "black"))
```

reading in the data and making means

``` r
measures<-read.csv("~/Desktop/current does/currentpapersgrants/elevationPaper2024/combineddatafiles/humeimeasures.csv")
meas<-measures[,c(1,3,10,15,17,20,21,22,24,26,27,28,30,31,c(58:61))]
#meas<-data.frame(measures$sex, measures$fledge.days.jan,measures$nest.elev,measures$year, measures$weight,measures$tarsus, measures$site)
#colnames(meas)<-gsub("measures.","", colnames(meas))
meas<-subset(meas, sex=="c")
meas$sample[(meas$site=="ks" & meas$year<1988) | (meas$site=="ks" & meas$year==2011) | (meas$site=="kl" & meas$year==2008) | (meas$site=="mn" & meas$year>2000 & meas$year<2005) |(meas$site=="mn" & meas$year>2022)| (meas$site=="mn" & meas$year==2017)]<-1
meas<-subset(meas, sample==1)

nestDay<-tapply(meas$fledge.days.jan, meas$nest, function(x) mean(x, na.rm=T))
nestElev<-tapply(meas$nest.elev, meas$nest, function(x) mean(x, na.rm=T))
nestYear<-tapply(meas$year, meas$nest, function(x) mean(x, na.rm=T))
nestTarsus<-tapply(meas$tarsus, meas$nest, function(x) mean(x, na.rm=T))
nestMass<-tapply(meas$weight, meas$nest, function(x) mean(x, na.rm=T))
nestloc<-tapply(meas$site, meas$nest, function(x) x[1])
nestchicks<-tapply(meas$numb.chicks, meas$nest,function(x) mean(x, na.rm=T))
nestTs<-data.frame(nestDay,nestElev,nestloc, nestYear,nestTarsus, nestMass, nestchicks)
table(nestTs$nestYear,nestloc)
```

    ##       nestloc
    ##        kl ks mn
    ##   1985  0 51  0
    ##   1986  0 54  0
    ##   1987  0 47  0
    ##   2001  0  0 29
    ##   2003  0  0 32
    ##   2004  0  0 26
    ##   2008 26  0  0
    ##   2011  0 14  0
    ##   2017  0  0  9
    ##   2023  0  0 11
    ##   2024  0  0 20

making the plots

``` r
nestTs<-subset(nestTs, nestYear!="2017"&nestYear!="2023")
z<-length(unique(nestTs[nestTs$nestloc=='mn',]$nestYear))
spmn<-ggplot(nestTs[nestTs$nestloc=='mn',], aes(x=nestDay, y=nestTarsus, color=as.factor(nestYear)))+geom_point(cex=1)+geom_smooth(method='lm', formula= y~x, se = F, lwd=1)+scale_color_manual(values=viridis(z)) + ylim(15,19) + xlim(150, 210)+ ylab("Mean tarsus length, mm.")+ xlab("Banding date")

#nestTs<-subset(nestTs, nestYear!="2011")
z<-length(unique(nestTs[nestTs$nestloc=='ks',]$nestYear))
spks<-ggplot(nestTs[nestTs$nestloc=='ks',], aes(x=nestDay, y=nestTarsus, color=as.factor(nestYear)))+geom_point(cex=1)+geom_smooth(method='lm', formula= y~x, se = F, lwd=1)+scale_color_manual(values=viridis(z)) + ylim(15,19)+ xlim(150, 210)+ ylab("Mean tarsus length, mm.")+ xlab("Banding date")

spwt<-ggplot(nestTs, aes(x=nestDay, y=nestMass, color=as.factor(nestYear)))+geom_point(cex=1)+geom_smooth(method='lm', formula= y~x, se = F, lwd=1)+scale_color_manual(values=viridis(z)) + ylim(15,19) + xlim(150, 210)+ ylab("Mass, g.")+ xlab("Banding date")

z<-length(unique(nestTs[nestTs$nestloc=='mn',]$nestYear))
spmn<-ggplot(nestTs[nestTs$nestloc=='mn',], aes(x=nestDay, y=nestMass, color=as.factor(nestYear)))+geom_point(cex=1)+geom_smooth(method='lm', formula= y~x, se = F, lwd=1)+scale_color_manual(values=viridis(z)) + xlim(150, 210)+ ylab("Mass, g.")+ xlab("Banding date")

nestTs<-subset(nestTs, nestYear!="2011")
z<-length(unique(nestTs[nestTs$nestloc=='ks',]$nestYear))
spks<-ggplot(nestTs[nestTs$nestloc=='ks',], aes(x=nestDay, y=nestMass, color=as.factor(nestYear)))+geom_point(cex=1)+geom_smooth(method='lm', formula= y~x, se = F, lwd=1)+geom_smooth(method='lm', formula= y~x, se = F, lwd=1)+scale_color_manual(values=viridis(z)) + xlim(150, 210)+ ylab("Mass,g.")+ xlab("Banding date")

z<-length(unique(nestTs[nestTs$nestloc=='mn',]$nestYear))
spmn<-ggplot(nestTs[nestTs$nestloc=='mn',], aes(x=nestElev, y=nestTarsus, color=as.factor(nestYear)))+geom_point(cex=1)+geom_smooth(method='lm', formula= y~x, se = F, lwd=1)+scale_color_manual(values=viridis(z))+ ylim(c(15,19))+ylab("Tarsus length, mm.")+ xlab("Elevation, m.")

ggarrange(p2(spks),p2(spmn), nrow =2)
```

    ## Warning: Removed 2 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 2 rows containing non-finite outside the scale range (`stat_smooth()`).

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 6 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 6 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](condition_files/figure-gfm/plots-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.

``` r
test<-subset(measures, year<1987 & sex=="c" & !is.na(measures$fledge.days.jan))
year<-as.numeric(as.factor(test$year))
tarsus<-test$tarsus
nest<-as.numeric(as.factor(test$nest))
day<-test$fledge.days.jan
#########################
#### Model ########
#########################
{
  sink("Shifts.R.jags")
  cat("
  model {
# Likelihood
for(i in 1:n){
  tarsus[i] ~ dnorm(mu[i], tau)
  mu[i] <- alpha[year[i]]+beta[year[i]]*day[i]+gamma[nest[i]]
}

for(j in 1:nyear){
alpha[j] ~ dnorm(mu.alpha, tau.alpha)
beta[j] ~ dnorm(mu.beta, tau.beta)


}

for (k in 1:nNest) {
      gamma[k] ~ dnorm(mu.gamma, tau.gamma) #prior
    }

# Priors
sigma ~ dunif(0,10)

mu.alpha ~ dnorm(0,0.001)
sigma.alpha ~ dunif(0,10)

mu.beta ~ dnorm(0,0.001)
sigma.beta ~ dunif(0,10)

mu.gamma ~ dnorm(0,0.001)
sigma.gamma ~ dunif(0,10)

# Derived quantities
tau <- pow(sigma,-2)
tau.alpha <- pow(sigma.alpha,-2)
tau.beta <- pow(sigma.beta,-2)
tau.gamma<-pow(sigma.gamma,-2)


}",fill = TRUE)
  sink()
} 

# Bundle data
win.data <- list(day=day,tarsus=tarsus,year=year,nest=nest, n=length(day),nyear=length(unique(test$year)), nNest=length(unique(nest)))



# Parameters monitored
params <- c('alpha','sigma','sigma.alpha', 'beta','sigma.beta', 'sigma.gamma')

# MCMC settings
ni <- 40000; nt <- 30; nb <- 10000; nc <- 3

tic();Sys.time();out<-jags(win.data, inits=NULL, params, "Shifts.R.jags", n.chains = nc,
           n.thin = nt, n.iter = ni, n.burnin = nb,parallel=T);toc();beep(2)
```

    ## [1] "2025-03-15 18:21:41 CDT"

    ## 
    ## Processing function input....... 
    ## 
    ## Done. 
    ##  
    ## Beginning parallel processing using 3 cores. Console output will be suppressed.
    ## 
    ## Parallel processing completed.
    ## 
    ## Calculating statistics....... 
    ## 
    ## Done.

    ## 2.543 sec elapsed
