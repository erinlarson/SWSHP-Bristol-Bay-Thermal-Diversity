#Primary authorw of this script: Erik Schoen and Will Samuel   
#Modeling Coho growth using bioenergetics
#Project PI: Erin Larson, ACCS
#Other collaborators/Co-PI's: Erik Schoen, Becky Shaftel, Sue Mauger, Daniel Schindler, Ben Meyer, Dustin Merrigan... 
#See the README file for more info...
#Started 3/7/2024


library(tidyverse) #For data manipulation
library(ggplot2) #For plotting
library(cowplot) #For plotting
library(data.table) #Changes format to help the analysis run quicker
library(lubridate) #For working with da
set.seed(1)


rm(list=ls(all=TRUE)) # clear the working environment
#setwd("~/Development/SWSHP-Bristol-Bay-Thermal-Diversity")


#Generate a dataframe with these columns: (drainage, site, year, date, and mean daily temp) that I can use as an input for bioenergetics simulations. This needs to be on a daily timestep, so interpolate temperatures between days if there are any gaps.
#For the Nushagak drainage only, generate another dataframe with multiple simulations for each site year, allowing fish to either stay put or move downriver to the next site within the river network progressively throughout the growing season. For simplicity, Id say we could allow fish to move or stay put at the end of each week, and then stay put at that site for the next 7 days. 



# 1) Daily time series for all the sites (drainage/HUC8, Site, Year, Date, Mean daily temp)
# 2) Spatial heirarchy in the NUSH    Mulchatna, upper nush, lower nush, nush bay 
# 3) Arc GIS map: Color points with fish growth potential for each site on the points on the map, compare a warm year and a cold year. 




# Generate a daily time series for the bioenergetics input ----------------
temp <- readRDS("fish/data/temp_data_2023-03-16.rds")

temp <- temp %>% 
  rename("Site" = StationName) %>%   
  mutate("Drainage" = str_extract(SiteID, "(?<=UW_)[^ ]+"),
         "DOY" = yday(sampleDate), 
         SiteYear = paste(Site, YearSampled, sep = "_")) %>% 
  select(-SiteID, -file_name)

unique(temp$Site) #8 sites
unique(temp$YearSampled) #9 years. Might be able to add more recent data too. 
unique(temp$Drainage) #9 years. Might be able to add more recent data too. 



full.temp <- read.csv("temperature/output/BBDailyTemps.csv")
metadata <- readRDS("temperature/output/bristol_bay_site_metadata/rds")

str(full.temp)

unique(full.temp$SiteID) #8 sites

full.temp <- full.temp %>% 
  rename("Site" = StationName) %>%   
  mutate("Drainage" = str_extract(SiteID, "(?<=UW_)[^ ]+"),
         "DOY" = yday(sampleDate), 
         SiteYear = paste(Site, YearSampled, sep = "_")) %>% 
  select(-SiteID, -file_name)


common_values <- intersect(full.temp$SiteID, )

num_common_values <- length(common_values)



# Build this off of Erik's existing bioenergetics code --------------------

 



