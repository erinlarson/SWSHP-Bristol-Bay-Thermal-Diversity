# Primary author: Will Samuel
#Identify Coho Salmon year classes in Bristol Bay streams 
#Started 2/12/2024


library(tidyverse)
library(ggplot2)
library(cowplot)
library(mclust)
set.seed(1)


rm(list=ls(all=TRUE)) # clear the working environment

#See old data, not postprocessed in "Coho size.R" for more details



fishClean <- read.csv("./fish/newdata/fishClean.csv")
fishSiteYears <- read.csv("./fish/newdata/fishSiteYears.csv")
tempMetricsClean <- read.csv("./fish/newdata/tempMetricsClean.csv") 

View(fishClean) #Complete coho size data, only observed in August and September, and with the outlier fish removed.
View(fishSiteYears) #Fish sites and the sample size
View(tempMetricsClean) #Temperature metadata

fishTempSiteYears <- tempMetricsClean %>%
  left_join(fishSiteYears, by = c("Site", "Year"))

fishTemp <- fishTempSiteYears %>%
  left_join(fishClean, by = c("Site", "Year"))

View(fishTemp) #All of the compiled data
st(fishTemp)
#'data.frame':	3474 obs. of  19 variables:
#  $ StationName.x: chr  "Bear Creek" "Bear Creek" "Bear Creek" "Bear Creek" ...
#$ YearSampled.x: int  2011 2011 2011 2011 2011 2011 2011 2011 2011 2011 ...
#$ CDD          : num  514 514 514 514 514 ...     ##### Cumulative growing Degree Days
Not sure what this is #$ CDD_172      : num  445 445 445 445 445 ...
#$ MWAT         : num  6.95 6.95 6.95 6.95 6.95 ... ##### maximum weekly average temperature                             
Not sure what this is #$ mn_summer    : num  5.78 5.78 5.78 5.78 5.78 ...
Not sure what this is #$ count        : int  89 89 89 89 89 89 89 89 89 89 ...
#$ Year         : int  2011 2011 2011 2011 2011 2011 2011 2011 2011 2011 ...
#$ Site         : chr  "Bear Creek" "Bear Creek" "Bear Creek" "Bear Creek" ...
This is the number of fish observed at each site #$ Length_n     : int  283 283 283 283 283 283 283 283 283 283 ...
#$ SampleType   : chr  "Stick Seine" "Stick Seine" "Stick Seine" "Stick Seine" ...
#$ Lake         : chr  "Aleknagik" "Aleknagik" "Aleknagik" "Aleknagik" ...
#$ YearSampled.y: int  2011 2011 2011 2011 2011 2011 2011 2011 2011 2011 ...
#$ DateSampled  : chr  "2011-08-04T00:00:00Z" "2011-08-04T00:00:00Z" "2011-08-04T00:00:00Z" "2011-08-04T00:00:00Z" ...
#$ StationName.y: chr  "Bear Creek" "Bear Creek" "Bear Creek" "Bear Creek" ...
#$ Species      : chr  "Coho salmon" "Coho salmon" "Coho salmon" "Coho salmon" ...
#$ Length       : int  87 97 84 95 83 85 87 99 77 102 ...
#$ Mass         : num  7.92 NA 6.8 11.06 6.97 ...
#$ Month        : int  8 8 8 8 8 8 8 8 8 8 ...



if (any(is.na(fishClean))) {
  stop("Missing values found in the dataset. Please clean the data.")
}

# Check for infinite values
if (any(is.infinite(fishClean))) {
  stop("Infinite values found in the dataset. Please clean the data.")
}




















# Identify year classes using Sethi et al. 2017 code ----------------------





#Goal: run this code for each site year that we have data for (22 site years)
#First write the code and get it to work on a single cold vs warm site (Big Whitefish and Bear Creek)
#Then iterate through all site years using a for loop










#Old trials VV


install.packages("dtaa.table")
library(data.table)


Length_all <- data.table(fishClean$Length)





opt.threshold.sim <- function(mod,n.draw=100000,grid.lo=10,grid.hi=210)
{
  # Simulate data drawn from fitted Mclust mixture distribution object. Use 'try' and 'while' to avoid failure of 
  # sim{mclust} to successfully generate training data
  train <- try(sim(modelName=mod$modelName,parameters=mod$parameters,n=n.draw),TRUE) 
  while(is.numeric(train)==F) train <- try(sim(modelName=mod$modelName,parameters=mod$parameters,n=n.draw),TRUE)
  train.df <- data.frame(x.=train[,2],y.true=train[,1],y.fit=1)
  # 2-component mixture model (e.g. two age cohorts, e.g. young of year 0's and age 1+), finds a single threshold
  obj.funG2 <- function(len,X) 
  {
    X$y.fit[X$x. > len] <- 2
    objG2 <- sum(abs(X$y.true - X$y.fit)) # classification errors
  } # end obj.fun
  fit <- optimize(f=obj.funG2,X=train.df,interval=c(grid.lo,grid.hi)) # employs a grid search using lengths from grid.lo to grid.hi      
  thresh <- fit$minimum    
  error <- fit$objective/n.draw     # the estimated classification error rate with the best-fit age-discriminating length threshold
  return(list(threshold=thresh,error.rate = error))
} # end opt.threshold.sim

# Fork length data: juvenile Coho salmon, O. kisutch, from Bristol Bay 
str(fishClean)

hist(fishClean$Length,breaks=100)
# Define nonparametric bootstrap routine parameters
boots <- 5 # number of boostrap datasets
draws <- 100# number of draws within the opt.threshold.sim() function
grid. <- c(30,155) # search grid for opt.threshold.sim()
# Define Type I error level for bootstrap confidence intervals (alpha level)
alpha <- 0.05      
# Observed data
# Fit an mclust mixture model, forcing to a 2-component mixture, allow mclust to pick BIC best variance structure (default option)
mix.mod <- Mclust(data=Length_all,modelNames=NULL,G=2) #note package is mclust but function is Mclust      
# Find the optimal age-discriminating fork length threshold
mix.mod.threshold <- opt.threshold.sim(mod=mix.mod,grid.lo=grid.[1],grid.hi=grid.[2],n.draw=draws)
# Bootstrap routine
# Create storage list object
pars.boot <- list(means=matrix(nr=boots,nc=mix.mod$G),variances=matrix(nr=boots,nc=length(mix.mod$parameters$mean)),
                  thresholds=matrix(nr=boots,nc=mix.mod$G-1),error.rate=matrix(nr=boots,nc=1))

# Run bootstrap iterations
for ( i in 1:boots){
  # Resample dataset and fit the mixture model, forcing number of distributions and variance 
  # structure from best fit model; includes error trap
  mix.mod.boot <-NULL
  while(is.null(mix.mod.boot)[1] == T) {
    dat.boot <- sample(x=fishClean,size=length(fishClean),replace=T)
    try(mix.mod.boot <- Mclust(data=dat.boot,G=mix.mod$G,modelName=mix.mod$modelName,warn=F),TRUE)
  }
  # store fitted Mclust parameters
  pars.boot$means[i,] <- mix.mod.boot$parameters$mean[order(mix.mod.boot$par$mean)]
  if(mix.mod.boot$parameters$variance$modelName=="E"){
    pars.boot$variances[i,] <- rep(mix.mod.boot$parameters$variance$sigmasq,length(mix.mod$parameters$mean))
  } else 
  {pars.boot$variances[i,] <- mix.mod.boot$parameters$variance$sigmasq[order(mix.mod.boot$par$mean)]}
  # estimate and store optimal age-discriminating length threshold  and classification error rate using opt.threshold.sim()
  temp <- opt.threshold.sim(mod=mix.mod.boot,grid.lo=grid.[1],grid.hi=grid.[2],n.draw=draws)
  pars.boot$thresholds[i,] <- round(temp$threshold,0)
  pars.boot$error.rate[i,] <- temp$error.rate
} # end i loop for bootstrap...takes some time

# calculate nonparametric bootstrap CI.pct 2-sided confidence intervals
CI.pct <- (1-alpha) # percentage for nonparameteric bootstrap routine confidence intervals
q.lo <- alpha/2
q.hi <- 1-alpha/2
temp.fun <- function(Y){apply(X=Y,MARGIN=2,FUN=function(x){quantile(x,probs=c(q.lo,q.hi),na.rm=T)})}
boot.CIs <- lapply(pars.boot,FUN=temp.fun)
# Print estimate and CI's
mix.mod.threshold
boot.CIs











