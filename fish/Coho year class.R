#Primary author of this script: Will Samuel
#Identify Coho Salmon year classes in Bristol Bay streams 
#Project PI: Erin Larson, ACCS
#Other collaborators/Co-PI's: Erik Schoen, Becky Shaftel, Sue Mauger, Daniel Schindler, Ben Meyer, Dustin Merrigan... 
#See the README file for more info...
#Started 2/12/2024


library(tidyverse) #For data manipulation
library(ggplot2) #For plotting
library(cowplot) #For plotting
library(mclust) #Used for the mixure model from Sethi et al.
library(rcompanion) #for pseudo R-squared
library(data.table) #Changes format to help the analysis run quicker
library(DescTools) #Used to calculate Mode()
set.seed(1)


rm(list=ls(all=TRUE)) # clear the working environment
#setwd("~/Development/SWSHP-Bristol-Bay-Thermal-Diversity")

#See old data, not postprocessed in "Coho size.R" for more details



# Combine and filter data -------------------------------------------------


fishClean <- fread("./fish/newdata/fishClean.csv")
fishSiteYears <- fread("./fish/newdata/fishSiteYears.csv")
tempMetricsClean <- fread("./fish/newdata/tempMetricsClean.csv") 

#View(fishClean) #Complete coho size data, only observed in August and September, and with the outlier fish removed.
#View(fishSiteYears) #Fish sites and the sample size
#View(tempMetricsClean) #Temperature metadata


#Filtering down just the Site Years we are interested in, with temperature data and > 20 fish observations
minSampleSize <- 30

fishTempSiteYears <- tempMetricsClean %>%
  left_join(fishSiteYears, by = c("Site", "Year"))




fishTemp <- fishTempSiteYears %>%
  left_join(fishClean, by = c("Site", "Year")) %>% 
  mutate(Site = str_replace_all(Site, " ", "_")) %>% 
  mutate(SiteYear = paste0(as.character(Site),"_", Year))
summary(fishTemp)


hist(fishTemp$Length_n, breaks = 50) #There would be little difference in limiting the site years to fish with 20 vs 30 fish, since almost all the years have a lot more than that. 
abline(v = 30, col = "red", lty = 1)
abline(v = 20, col = "red", lty = 2)



fishTemp <- fishTemp %>%
  filter(Length_n >= minSampleSize)  #We only lose 11 fish by filtering Length_n >= 30, and have the same effect when using >= 20
  

#Older code, delete later after making sure it's necessary
#fishTemp <- fishTemp %>%
#  mutate(Site = str_replace_all(Site, " ", "_")) %>% 
#  mutate(SiteYear = paste0(as.character(Site),"_", Year)) %>% 
#  filter(Length > 0, 
#         Length_n >= minSampleSize) %>% 
#  filter(!is.na(CDD))


#rm(fishClean)
#rm(fishSiteYears)
#rm(tempMetricsClean)

unique(fishTemp$SiteYear) #22 unique site-years
View(fishTemp) #Complete coho size data, only observed in August and September, and with the outlier fish removed.
str(fishTemp)
#Classes ‘data.table’ and 'data.frame':	3463 obs. of  20 variables:
#$ StationName.x: chr  "Bear Creek" "Bear Creek" "Bear Creek" "Bear Creek" ...
#$ YearSampled.x: int  2011 2011 2011 2011 2011 2011 2011 2011 2011 2011 ...
#$ CDD          : num  514 514 514 514 514 ...
Not sure what this is #$ CDD_172      : num  445 445 445 445 445 ...
#$ MWAT         : num  6.95 6.95 6.95 6.95 6.95 ... #max weekly average temp), helpful for understanding heat stress on fish 
Not sure what this is, mean summer temp? #$ mn_summer    : num  5.78 5.78 5.78 5.78 5.78 ...
Not sure what this is #$ count        : int  89 89 89 89 89 89 89 89 89 89 ...
#$ Year         : int  2011 2011 2011 2011 2011 2011 2011 2011 2011 2011 ...
#$ Site         : chr  "Bear_Creek" "Bear_Creek" "Bear_Creek" "Bear_Creek" ...
#This is the number of fish observations $ Length_n     : int  283 283 283 283 283 283 283 283 283 283 ...
#$ SampleType   : chr  "Stick Seine" "Stick Seine" "Stick Seine" "Stick Seine" ...
#$ Lake         : chr  "Aleknagik" "Aleknagik" "Aleknagik" "Aleknagik" ...
#$ YearSampled.y: int  2011 2011 2011 2011 2011 2011 2011 2011 2011 2011 ...
#$ DateSampled  : POSIXct, format: "2011-08-04" "2011-08-04" "2011-08-04" "2011-08-04" ...
#$ StationName.y: chr  "Bear Creek" "Bear Creek" "Bear Creek" "Bear Creek" ...
#$ Species      : chr  "Coho salmon" "Coho salmon" "Coho salmon" "Coho salmon" ...
#$ Length       : int  87 97 84 95 83 85 87 99 77 102 ...
#$ Mass         : num  7.92 NA 6.8 11.06 6.97 ...
#$ Month        : int  8 8 8 8 8 8 8 8 8 8 ...
#$ SiteYear     : chr  "Bear_Creek_2011" "Bear_Creek_2011" "Bear_Creek_2011" "Bear_Creek_2011" ...
#- attr(*, ".internal.selfref")=<externalptr> 








# Start by identify year classes using Sethi et al. 2017 code  with a subset of the data ----
#(these are the same sites used in "Coho size.R")

big.whitefish.creek <- fishTemp %>% #Warm site year
  filter(SiteYear == "Big_Whitefish_Creek_2019") 

head(big.whitefish.creek)

bear.creek <- fishTemp %>% #Cold site year
  filter(SiteYear == "Bear_Creek_2019") 

head(bear.creek)



mix.dat <- as.numeric(big.whitefish.creek$Length)
#mix.dat <- as.numeric(length.dat)
mix.dat

hist(mix.dat, breaks = 50)

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
mix.mod.threshold #$threshold = 97.60057, $error.rate = 0.04416 
#^^This shows a classification threshold of 97.6mm. So age-0 < 97.6 < Age-1

boot.CIs 
#$means
#2.5%  79.70290 106.2982
#97.5% 82.26498 110.6009

#$variances
#2.5%  57.40144 57.40144
#97.5% 91.43552 91.43552

#$thresholds
#2.5%    96
#97.5%   99

#$error.rate
#2.5%  0.0256995
#97.5% 0.0739405



####Repeat with a cold Site Year
mix.dat <- as.numeric(bear.creek$Length)
#mix.dat <- as.numeric(length.dat)
mix.dat

hist(length.dat, breaks = 50)

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
mix.mod.threshold #$threshold = 84.171093, $error.rate = 0.0136
#^^This shows a classification threshold of 84.171093mm. So age-0 < 84.2 < Age-1
#This supports our hypothesis that fish in colder creeks grow slower. 

boot.CIs 
#$means
#2.5%  67.74597 101.5857
#97.5% 71.32163 103.6552

#$variances
#2.5%  49.28704 49.28704
#97.5% 70.10785 70.10785

#$thresholds
#2.5%    83
#97.5%   85

#$error.rate
#2.5%  0.00645000
#97.5% 0.02204825





# Repeat the analysis, using a loop to do this for each site year --------



#First fit the function for the analysis:
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


# Then define nonparametric bootstrap routine parameters
boots <- 500 # number of boostrap datasets
draws <- 100000# number of draws within the opt.threshold.sim() function
grid. <- c(30,155) # search grid for opt.threshold.sim()
# Define Type I error level for bootstrap confidence intervals (alpha level)
alpha <- 0.05      
# Observed data



#Then prep the data to analyze by site year
SiteYears <- unique(fishTemp$SiteYear)

site_year_data <- list()

# Iterate over unique SiteYears
for (current_site_year in SiteYears) {
  # Create a new data frame for the current SiteYear
  current_data <- fishTemp %>% 
    filter(SiteYear == current_site_year)
  
  # Store the data frame in the list
  site_year_data[[current_site_year]] <- current_data
}



head(site_year_data)
site_year_data$Bear_Creek_2019

SiteYears
#Bear_Creek_2011
#Bear_Creek_2014
#Bear_Creek_2018
#Bear_Creek_2019
#Big_Whitefish_Creek_2011
#Big_Whitefish_Creek_2013
#Big_Whitefish_Creek_2014
#Big_Whitefish_Creek_2018
#Big_Whitefish_Creek_2019
#Hidden_Lake_Creek_2016
#Hidden_Lake_Creek_2017
#Hidden_Lake_Creek_2020  
#Lynx_Creek_2007
#Lynx_Creek_2017
#Lynx_Creek_2019
#Pfifer_Creek_2011
#Silver_Salmon_Creek_2011
#Silver_Salmon_Creek_2014
#Teal_Creek_2014
#Teal_Creek_2016
#Yako_Creek_2011
#Yako_Creek_2018     





#Then run the analysis, looping through each site year


# Create an empty data frame to store the results
results_table <- data.frame(SiteYear = character(), threshold = numeric(), error.rate = numeric(), stringsAsFactors = FALSE)


# Iterate over each data frame in the list
for (i in seq_along(site_year_data)) {
  current_data <- site_year_data[[i]]
  
  # Extract the mix.dat column from the current data frame
  mix.dat <- current_data$Length
  
  # Fit an mclsut mixture model
  mix.mod <- Mclust(data = mix.dat, modelNames = NULL, G = 2)
  
  # Find the optimal age-discriminating fork length threshold
  mix.mod.threshold <- opt.threshold.sim(mod = mix.mod, grid.lo = grid.[1], grid.hi = grid.[2], n.draw = draws)
  
  
  ##Taking this out for now to see if I can get it to run
  # Bootstrap routine
  #pars.boot <- list(means = matrix(nr = boots, nc = mix.mod$G),
  #                  variances = matrix(nr = boots, nc = length(mix.mod$parameters$mean)),
  #                  thresholds = matrix(nr = boots, nc = mix.mod$G - 1),
  #                  error.rate = matrix(nr = boots, nc = 1))
  
  #for (j in 1:boots) {
  #  dat.boot <- sample(x = mix.dat, size = length(mix.dat), replace = TRUE)
  #  mix.mod.boot <- NULL
  #  while (is.null(mix.mod.boot)[1] == TRUE) {
  #    try(mix.mod.boot <- Mclust(data = dat.boot, G = mix.mod$G, modelName = mix.mod$modelName, warn = FALSE), TRUE)
  #  }
  
  # pars.boot$means[j,] <- mix.mod.boot$parameters$mean[order(mix.mod.boot$par$mean)]
  
  #  if (mix.mod.boot$parameters$variance$modelName == "E") {
  #    pars.boot$variances[j,] <- rep(mix.mod.boot$parameters$variance$sigmasq, length(mix.mod$parameters$mean))
  # } else {
  #  pars.boot$variances[j,] <- mix.mod.boot$parameters$variance$sigmasq[order(mix.mod.boot$par$mean)]
  #}
  
  #temp <- opt.threshold.sim(mod = mix.mod.boot, grid.lo = grid.[1], grid.hi = grid.[2], n.draw = draws)
  #pars.boot$thresholds[j,] <- round(temp$threshold, 0)
  #pars.boot$error.rate[j,] <- temp$error.rate
  # }
  
  # Calculate nonparametric bootstrap CI.pct 2-sided confidence intervals
  #CI.pct <- (1 - alpha)
  #q.lo <- alpha / 2
  #q.hi <- 1 - alpha / 2
  #temp.fun <- function(Y) {apply(X = Y, MARGIN = 2, FUN = function(x) {quantile(x, probs = c(q.lo, q.hi), na.rm = TRUE)})}
  #boot.CIs <- lapply(pars.boot, FUN = temp.fun)
  
  # Append the results to the results_table
  results_table <- rbind(results_table, data.frame(SiteYear = unique(current_data$SiteYear),
                                                   threshold = mix.mod.threshold$threshold,
                                                   error_rate = mix.mod.threshold$error.rate,
                                                   stringsAsFactors = FALSE))
}

# Print the results table
print(results_table)





# Now incorporate the length threshold data into the original data --------

fishTemp_Length <- merge(x = fishTemp, y = results_table, by = "SiteYear", all = TRUE)
head(fishTemp_Length)


fishTemp_Length <- fishTemp_Length %>% 
  mutate(age = ifelse(Length > threshold, 1, 0))


write_csv(fishTemp_Length, "./fish/newdata/fishTemp_Length.csv")





ggplot(data = fishTemp_Length, aes(x = Length, color = CDD)) +
  geom_density(linewidth = 1.5) +
  scale_color_continuous(low = "blue", high = "red") +
  geom_vline(aes(xintercept = threshold), linewidth = 1.25, linetype = 2) +
  scale_y_continuous(name = "Density", breaks = NULL) +
  facet_grid(rows = vars(Year), cols = vars(Site)) +
  theme_bw(8)





#Problem site-years:
#Bear Creek 2014
#Hidden Lake Creek 2017

#Armstrong et al. 2010 data includes Bear Creek from 2007 (Sept 9), the min threshold for Age 1 is ~65 mm. Some overlap between Age 1 and Age 0 between 65 and 70 mm. 

bear_threshold <- fishTemp_Length %>% 
  filter(Site == "Bear_Creek") 

summary(bear_threshold$threshold)
unique(bear_threshold$Year)       #2011       2014    2018    2019
unique(bear_threshold$threshold) #66.84656 44.07640 77.24647 83.74401
unique(bear_threshold$CDD)      #514.1750 693.1488 716.1138 753.2176
#I will replace the 2014 measurement with the upper end of the range from Armstrong et al. 2010, trying to split the difference between that and the threshold seen in similar warm-ish years (2018 & 2019)


hidden_lake_threshold <- fishTemp_Length %>% 
  filter(Site == "Hidden_Lake_Creek") 

summary(hidden_lake_threshold$threshold)
unique(hidden_lake_threshold$Year)       # 2016       2017      2020
unique(hidden_lake_threshold$threshold) #116.78020  40.83511  117.26128
unique(hidden_lake_threshold$CDD)       #1217.194   1122.651  1095.902
#I will replace the 2014 measurement with the upper end of the range from Armstrong et al. 2010, trying to split the difference between that and the threshold seen in similar warm-ish years (2018 & 2019)




fishTemp_Length_edited <- fishTemp_Length %>% 
  mutate(threshold = ifelse(SiteYear == "Bear_Creek_2014", 70, threshold),
         threshold = ifelse(SiteYear == "Hidden_Lake_Creek_2017", 105, threshold))

fishTemp_Length_edited <- fishTemp_Length_edited %>% 
  mutate(age = ifelse(Length > threshold, 1, 0))



write_csv(fishTemp_Length_edited, "./fish/newdata/fishTemp_Length_edited.csv")





# Find the mean and mode length for each site year and age ---------------------

age0 <- fishTemp_Length_edited %>% 
  filter(age == 0) %>% 
  group_by(SiteYear) %>% 
  summarize(mean_length_age0 = mean(Length),
            mode_length_age0 = Mode(Length))

age1 <- fishTemp_Length_edited %>% 
  filter(age == 1) %>% 
  group_by(SiteYear) %>% 
  summarize(mean_length_age1 = mean(Length), 
            mode_length_age1 = Mode(Length))


mean_lengths <- merge(x = age0, y = age1, by = "SiteYear", all = TRUE)
mean_lengths

write_csv(mean_lengths, "./fish/newdata/mean_lengths.csv")


fishTemp_Length_edited <- merge(x = fishTemp_Length_edited, y = mean_lengths, by = "SiteYear", all.x = TRUE)

#write_csv(fishTemp_Length_edited, "./fish/newdata/fishTemp_Length_edited.csv")


# Add some plots ----------------------------------------------------------


fishTemp_Length <- fread("./fish/newdata/fishTemp_Length.csv")
#fishTemp_Length <- fread("./fish/newdata/fishTemp_Length_edited.csv")



fishTemp_Length <- fishTemp_Length %>% 
  mutate(labels = SiteYear) %>% 
  mutate(labels = str_replace_all(labels, "_", " ")) 



age_length_plot <- ggplot(data = fishTemp_Length, aes(x = Length, #group = SiteYear,
                                                      color = CDD)) +
  geom_density(linewidth = 1.5) +
  geom_vline(aes(xintercept = threshold), linewidth = 1.25, linetype = 2) +
  scale_color_continuous(low = "blue", high = "red") +
  xlab("Fork length (mm)") +  
  #facet_wrap(~SiteYear) +
  facet_wrap(~labels) +
  scale_y_continuous(name = "Count", breaks = NULL)+
  labs(color = "Cumulative Degree Days")+
  theme_cowplot()+
  #geom_polygon(data = polygon_data, aes(x = x, y = y), fill = "gray", alpha = 0.5)+
  theme(
    legend.position = c(.5, .07),
    legend.direction = "horizontal",
    #legend.key.size = unit(1.75, "lines"),
    legend.key.height = unit(1.75, "lines"),
    legend.key.width = unit(2, "lines"),
    #legend.justification = c("right", "top"),
    #legend.box.just = "right",
    legend.margin = margin(10, 10, 10, 10),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14, face = "bold")
  )


age_length_plot  

#ggsave(plot = age_length_plot,
#       "fish/plots/age_length_plot.jpeg",
#       height = 20, 
#       width = 30, 
#       units = "cm")





ggplot(data = fishTemp_Length, aes(x = Length, color = CDD)) +
  geom_density(linewidth = 1.5) +
  scale_color_continuous(low = "blue", high = "red") +
  geom_vline(aes(xintercept = threshold), linewidth = 1.25, linetype = 2) +
  scale_y_continuous(name = "Density", breaks = NULL) +
  facet_grid(rows = vars(Year), cols = vars(Site)) +
  theme_bw(8)












#Replicate Erik's Big Bear and Whitefish Creek plot


BearBigWhitefish <- fishTemp_Length  %>% 
  filter(Site %in% c("Bear_Creek", "Big_Whitefish_Creek"))

unique(BearBigWhitefish$Year)


BearBigWhitefishFL_byYear <- ggplot(data = BearBigWhitefish, aes(x = Length, color = Site)) +
  geom_density(linewidth = 1.5) +
  scale_y_continuous(name = "Count", breaks = NULL) +
  scale_color_manual(values = c("darkblue", "red")) +
  facet_grid(rows = vars(Year)) +  
  theme_bw()+
  theme(legend.position = "bottom")

BearBigWhitefishFL_byYear

#ggsave(plot = BearBigWhitefishFL_byYear,
#       "fish/plots/BearBigWhitefishFL_byYear.jpeg",
#       height = 15, 
#       width = 12, 
#       units = "cm")







x2019 <- BearBigWhitefish  %>% 
  filter(Year == 2019)



BearBigWhitefishFL_2019 <- ggplot(data = x2019, aes(x = Length, color = Site)) +
  geom_density(linewidth = 1.5) +
  scale_y_continuous(name = "Count", breaks = NULL, expand = c(0, 0)) +
  scale_color_manual(values = c("darkblue", "red")) +
  #facet_grid(rows = vars(Year)) +  
  theme_cowplot()+
  theme(legend.position = "bottom")

BearBigWhitefishFL_2019

#ggsave(plot = BearBigWhitefishFL_2019,
#       "fish/plots/BearBigWhitefishFL_2019.jpeg",
#       height = 15, 
#       width = 15, 
#       units = "cm")






SiteYear_distributions <- ggplot(data = fishTemp_Length, aes(x = Length, group = SiteYear, color = CDD)) +
  geom_density(linewidth = 1.25) +
  scale_y_continuous(name = "Count", breaks = NULL, expand = c(0, 0)) +
  scale_color_continuous(low = "blue", high = "red") +
  #facet_grid(rows = vars(Year)) +  
  theme_cowplot()+
  labs(color = "Cumulative\nDegree Days") +
  theme(
    legend.position = c(.73, .8),
    legend.direction = "vertical",
    #legend.key.size = unit(1.75, "lines"),
    #legend.key.height = unit(1.75, "lines"),
    #legend.key.width = unit(2, "lines"),
    #legend.justification = c("right", "top"),
    #legend.box.just = "right",
    legend.margin = margin(10, 10, 10, 10),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )


SiteYear_distributions

#ggsave(plot = SiteYear_distributions,
#       "fish/plots/SiteYear_distributions.jpeg",
#       height = 15, 
#       width = 15, 
#       units = "cm")








fishTemp_Length <- fishTemp_Length %>%
  mutate(DateSampled = as.Date(DateSampled),
         Month_day = format(DateSampled, "%m-%d"))
View(fishTemp_Length)





ggplot(fishTemp_Length, aes(x = Month_day, y = Length, group = SiteYear, color = CDD)) +
  geom_boxplot(lwd = 1) +
  scale_color_gradient(low = "darkblue", high = "red") +  # Use a continuous color scale
  theme_cowplot() +
  theme(legend.position = "bottom")



ggplot(fishTemp_Length, aes(x = Month_day, y = Length, group = SiteYear, color = CDD)) +
  geom_point(size = 2, alpha = 0.7, position = position_dodge(width = 0.7)) +
  scale_color_gradient(low = "darkblue", high = "red") +  # Use a continuous color scale
  theme_cowplot() +
  theme(legend.position = "bottom")





ggplot(fishTemp_Length, aes(x = Length, y = Mass)) +
  geom_point(size = 2, alpha = 0.3)+
  theme_cowplot()

length_weight <- glm(Mass ~ Length, data = fishTemp_Length)
summary(length_weight)
nagelkerke(length_weight)
accuracy(list(length_weight))

#predict(length_weight, newdata = c(100))




ggplot(fishTemp_Length, aes(x = Month_day, y = Length, color = CDD)) +
  geom_point(size = 2, alpha = 0.7, position = position_dodge(width = 0.7)) +
  geom_smooth(aes(group = age), method = "loess") + 
  scale_color_gradient(low = "darkblue", high = "red") +
  theme_cowplot() +
  theme(legend.position = "bottom")








