#Primary author of this script: Will Samuel
#Modeling Coho growth using bioenergetics
#Project PI: Erin Larson, ACCS
#Other collaborators/Co-PI's: Erik Schoen, Becky Shaftel, Sue Mauger, Daniel Schindler, Ben Meyer, Dustin Merrigan... 
#See the README file for more info...
#Started 3/7/2024


library(tidyverse) #For data manipulation
library(ggplot2) #For plotting
library(cowplot) #For plotting
#library(mclust) #Used for the mixure model from Sethi et al.
#library(rcompanion) #for pseudo R-squared
library(data.table) #Changes format to help the analysis run quicker
#library(DescTools) #Used to calculate Mode()
set.seed(1)


rm(list=ls(all=TRUE)) # clear the working environment
#setwd("~/Development/SWSHP-Bristol-Bay-Thermal-Diversity")





#Build this off of Erik's existing bioenergetics code. 



