library(mclust)
set.seed(1)

# Function for resampling approach to estimate the optimal age-discriminating forklength given a k-finite Gaussian mixture model fit.
# This code simualtes forklength data from the fitted mixture distribution which entails known group membership.  
# Subsequently, a grid search is conducted over a vector of candidate age-discriminating length thresholds
# choosing the misclassification error rate minimizing best threshold.
# Argumnets: a) mod = fitted mclust mixture model, b) n.draw = number of simulated data, consider utilizing a 
# large number of draws to stabilize threshold estimates,
# c) grid.lo = minimum length candidate threshold size, d) grid.hi= maximum length candidate threshold size.
# Values: $thresholds = estimated thresholds; $error.rate = estimate of misclassification error rate given estimated optimal thresholds

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

# Example fork length data: juvenile Coho salmon, O. kisutch, from Meadow Creek 2013 Biweek 3 Lotic environment (n=76)
mix.dat <- c(86,90,83,79,87,76,92,115,91,50,50,50,80,85,52,42,56,94,84,105,93,87,54,71,94,45,48,61,48,
             73,80,88,113,53,53,74,97,92,91,101,83,103,105,93,82,78,89,107,61,99,87,98,86,58,48,58,
             44,50,72,60,48,84,81,59,87,79,99,45,52,88,88,100,118,82,91,96)
hist(mix.dat,breaks=25)
# Define nonparametric bootstrap routine parameters
boots <- 500 # number of boostrap datasets
draws <- 100000# number of draws within the opt.threshold.sim() function
grid. <- c(30,155) # search grid for opt.threshold.sim()
# Define Type I error level for bootstrap confidence intervals (alpha level)
alpha <- 0.05      
# Observed data
# Fit an mclsut mixture model, forcing to a 2-component mixture, allow mclust to pick BIC best variance structure (default option)
mix.mod <- Mclust(data=mix.dat,modelNames=NULL,G=2) #note package is mclust but function is Mclust      
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
    dat.boot <- sample(x=mix.dat,size=length(mix.dat),replace=T)
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
mix.mod.threshold #$threshold = 63.71887, $error.rate = 0.01016
boot.CIs 
#$means
#2.5%  48.99866 86.12433
#97.5% 53.98508 92.48792

#$variances
#2.5%  10.68025  68.23359
#97.5% 42.93424 194.89317

#$thresholds
#2.5%    57
#97.5%   67

#$error.rate
#2.5%  0.00250425
#97.5% 0.02453275
