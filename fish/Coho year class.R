# Primary author: Will Samuel
#Identify Coho Salmon year classes in Bristol Bay streams 
#Started 2/12/2024


library(tidyverse)
library(ggplot2)
library(cowplot)


rm(list=ls(all=TRUE)) # clear the working environment
setwd("~/Development/SWSHP-Bristol-Bay-Thermal-Diversity")

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
Not sure what this is #$ CDD          : num  514 514 514 514 514 ...                                  
Not sure what this is #$ CDD_172      : num  445 445 445 445 445 ...
Not sure what this is #$ MWAT         : num  6.95 6.95 6.95 6.95 6.95 ...
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




# Identify year classes using Sethi et al. 2017 code ----------------------













