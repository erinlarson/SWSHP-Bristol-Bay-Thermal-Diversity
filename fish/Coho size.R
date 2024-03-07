# Coho size.R
# Compile and plot juvenile coho size data from Wood River watershed
# Erik Schoen
# 3/9/2023

####### Load packages and read in data #######
library(tidyverse)
library(readxl)
library(lubridate)
theme_set(theme_bw(14)) # Set the default ggplot2 theme.  Basic black and white plots without grid 
# lines and with 14 point font.

rm(list=ls(all=TRUE)) # clear the working environment
setwd("~/Development/SWSHP-Bristol-Bay-Thermal-Diversity")

fish <- read_excel("fish/data/WR_coho.xlsx") %>% 
  mutate(StationName = case_when(grepl("Amou", StationName) ~ "Amou Creek",
                                 TRUE ~ StationName))

temp <- readRDS("fish/data/temp_data_2023-03-16.rds")
tempMetrics <- readRDS("fish/data/temp_metrics_2023-03-16.rds")

####### Explore data #########
# summary(fish)
# fish %>% arrange(desc(Length))
# fish %>% distinct(Species)
# 
# fish_tally <- fish %>% 
#   filter(!is.na(Length)) %>% 
#   count(StationName, YearSampled, name = "Coho_count")
# 
# fish %>% 
#   filter(!is.na(Length)) %>% 
#   count(StationName, YearSampled) %>%
#   pivot_wider(names_from = YearSampled, values_from = n, names_sort = TRUE)
# ggplot(aes(x = YearSampled, y = n, color = StationName)) +
#   geom_point()
# 
# 
# fish %>% 
#   filter(!is.na(Length), Length < 200) %>% 
#   ggplot(aes(x = Length, color = StationName)) +
#   facet_grid(rows = YearSampled) +
#   geom_freqpoly()

############ Clean up data ##############
tempMetricsClean <- tempMetrics %>%
  mutate(Year = factor(YearSampled),
         Site = factor(StationName))

write_csv(tempMetricsClean, "./fish/newdata/tempMetricsClean.csv")


hist(fish$Length)

# Select only August and September measurements
# Remove very large or small fish
fishClean <- fish %>%
  mutate(Month = month(DateSampled),
         Year = factor(YearSampled),
         Site = factor(StationName)) %>%
  filter(Month %in% 8:9) %>%
  filter(Length %in% 35:200)

write_csv(fishClean, "./fish/newdata/fishClean.csv")


nFL <- fishClean %>%
  filter(!is.na(Length))
nMass <- fishClean %>%
  filter(!is.na(Mass))

hist(fishClean$Length)

hist(fishClean$Mass)

fishSiteYears <- fishClean %>%
  # group_by(c("StationName", "YearSampled")) %>%  
  group_by(Site, Year) %>%
  filter(!is.na(Length)) %>%
  summarize(Length_n = n())

write_csv(fishSiteYears, "./fish/newdata/fishSiteYears.csv")

# Remove site-years with very small sample sizes
# minSampleSize <- 20

fishTempSiteYears <- tempMetricsClean %>%
  left_join(fishSiteYears, by = c("Site", "Year"))
# filter(Length_n >= minSampleSize)

fishTemp <- fishTempSiteYears %>%
  left_join(fishClean, by = c("Site", "Year"))

BearBigWhitefish <- fishTemp %>%
  filter(Site == "Big Whitefish Creek" | Site == "Bear Creek")

y2019 <- fishTemp %>%
  filter(Year == "2019")



########### Plot data ###############

overallFL <- ggplot(data = fishClean, aes(x = Length)) +
  geom_density() + 
  xlab("Fork length (mm)") +
  scale_y_continuous(name = "Density", breaks = NULL)
overallFL
ggsave("fish/plots/overallFL.png", width = 6, height = 5)

overallFL.CDD <- ggplot(data = fishTemp, aes(x = Length, group = paste(Site, Year),
                                             color = CDD)) +
  geom_density() +
  scale_color_continuous(low = "blue", high = "red") +
  xlab("Fork length (mm)") +
  scale_y_continuous(name = "Density", breaks = NULL)
overallFL.CDD
ggsave("fish/plots/FL_by site-year CDD.png", width = 6, height = 5)


FL_by_site_year_all <- ggplot(data = fishClean, aes(x = Length)) +
  geom_density() +
  xlab("Fork length (mm)") +
  scale_y_continuous(name = "Density", breaks = NULL) +
  facet_grid(rows = vars(Year), cols = vars(Site)) +
  theme_bw(8)
FL_by_site_year_all
ggsave("fish/plots/FL by site-year_all.png")

FL_by_site_year_all_histo <- ggplot(data = fishClean, aes(x = Length)) +
  geom_histogram() +
  xlab("Fork length (mm)") +
  scale_y_continuous(name = "Count", breaks = NULL) +
  facet_grid(rows = vars(Year), cols = vars(Site)) +
  theme_bw(8)
FL_by_site_year_all_histo
ggsave("fish/plots/FL by site-year_all_histo.png")

FL_by_site_year_temp <- ggplot(data = fishTemp, aes(x = Length)) +
  geom_density() +
  scale_y_continuous(name = "Density", breaks = NULL) +
  facet_grid(rows = vars(Year), cols = vars(Site)) +
  theme_bw(8)
FL_by_site_year_temp
ggsave("fish/plots/FL by site-year_temp.png")

FL_overall_BearBigWhitefish <- ggplot(data = BearBigWhitefish, aes(x = Length, color = Site)) +
  geom_density() +
  scale_y_continuous(name = "Density", breaks = NULL) +
  scale_color_manual(values = c("dark blue", "red")) +  
  theme(legend.position = c(0.7, 0.8))
FL_overall_BearBigWhitefish
ggsave("fish/plots/FL overall_Bear Big Whitefish.png", width = 6, height = 5)

BearBigWhitefishFL_byYear <- ggplot(data = BearBigWhitefish, aes(x = Length, color = Site)) +
  geom_density() +
  scale_y_continuous(name = "Density", breaks = NULL) +
  scale_color_manual(values = c("dark blue", "red")) +
  facet_grid(rows = vars(Year)) +
  theme(legend.position = "top")

BearBigWhitefishFL_byYear
ggsave("fish/plots/FL Bear Big Whitefish_by year.png", width = 6, height = 8)
