#Primary authors of this script: Erik Schoen and Will Samuel   
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





# Generate a daily time series for the bioenergetics input ----------------
full.temp <- read.csv("temperature/output/BBDailyTemps.csv")

full.temp <- full.temp %>% 
  rename("Date" = sampleDate) %>% 
  mutate("Year" = year(ymd(Date)),
         "DOY" = yday(Date))

str(full.temp)

unique(full.temp$SiteID) #113 sites
unique(full.temp$YearSampled) #9 years. Might be able to add more recent data too. 
unique(full.temp$Drainage) #9 years. Might be able to add more recent data too.


unique_sites <- full.temp %>% 
  select(-X,-Date, -meanDT, -Year, -DOY) %>% 
  distinct(SiteID, Latitude, Longitude, HUC8, Waterbody_name)

write_csv(unique_sites, "./fish/newdata/unique_temp_sites.csv")


# For the Nushagak River, generate an occupancy matrix --------------------
#For the Nushagak drainage only, generate another dataframe with multiple simulations for each site year, allowing fish to either stay put or move downriver to the next site within the river network progressively throughout the growing season. For simplicity, Id say we could allow fish to move or stay put at the end of each week, and then stay put at that site for the next 7 days. 


### Still in progress ###

#Nushagak daily temperatures
nush <- full.temp %>% 
  filter(HUC8 %in% c(19030303, 19030301, 19030302)) 

unique(nush$SiteID)

#Labeling the site numbers from most downstream (1) to furthest upstream)
#because this is a dendritic stream network (as opposed to a linear path), this is an approximation

nush <- nush %>% 
  mutate(site_num = ifelse(SiteID == "UW_Aleknagik Squaw Creek", 1, 
                     ifelse(SiteID == "cik_Squaw Creek", 2, 
                     ifelse(SiteID == "USGS_15302812", 3, 
                     ifelse(SiteID == "cik_Panaruqak Creek", 4, 
                     ifelse(SiteID == "cik_Tunravik Creek", 5, 
                     ifelse(SiteID == "cik_Sivanguq Creek", 6, 
                     ifelse(SiteID == "cik_Napotoli Creek", 7, 
                     ifelse(SiteID == "usgs_15302250", 8, 
                     ifelse(SiteID == "usgs_15301500", 9, 
                     ifelse(SiteID == "usgs_15302300", 10, 
                     ifelse(SiteID == "usgs_15302320", 11, 
                     ifelse(SiteID == "accs_mustu17", 12, 
                     ifelse(SiteID == "usgs_15302250", 13, 
                     ifelse(SiteID == "accs_musfk01", 14, 
                     ifelse(SiteID == "accs_mutsk02", 15, 
                     ifelse(SiteID == "accs_mutsk09", 16, 
                     ifelse(SiteID == "usgs_15302200", 17, 
                     ifelse(SiteID == "accs_mussm15", 18, 
                     ifelse(SiteID == "accs_muekm23", 19, 
                     ifelse(SiteID == "accs_AKBB-040", 20, 
                     ifelse(SiteID == "accs_mubon10", 21, 
                     ifelse(SiteID == "accs_AKBB-011", 22, 
                     ifelse(SiteID == "accs_AKBB-020", 23, NA)))))))))))))))))))))))) %>% 
  mutate(site_upstream = ifelse(site_num == 23, NA, site_num+1), 
         site_downstream = ifelse(site_num > 1, site_num-1, NA))
#Hmmm these site numbers are missing some of the USGS sites... 

str(nush)

range(nush$Date)















