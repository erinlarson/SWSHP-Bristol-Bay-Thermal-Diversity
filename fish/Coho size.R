# Coho size.R
# Compile and plot juvenile coho size data from Wood River watershed
# Erik Schoen
# 3/9/2023

####### Load packages and read in data #######
library(tidyverse)
library(readxl)
library(lubridate)

fish <- read_excel("fish/data/WR_coho.xlsx") %>% 
  mutate(StationName = case_when(grepl("Amou", StationName) ~ "Amou Creek",
                                 TRUE ~ StationName))

temp <- readRDS("fish/data/temp_data_2023-02-18.rds")

####### Explore data #########
summary(fish)
fish %>% arrange(desc(Length))
fish %>% distinct(Species)

fish_tally <- fish %>% 
  filter(!is.na(Length)) %>% 
  count(StationName, YearSampled, name = "Coho_count")

fish %>% 
  filter(!is.na(Length)) %>% 
  count(StationName, YearSampled) %>%
  pivot_wider(names_from = YearSampled, values_from = n, names_sort = TRUE)
ggplot(aes(x = YearSampled, y = n, color = StationName)) +
  geom_point()


fish %>% 
  filter(!is.na(Length), Length < 200) %>% 
  ggplot(aes(x = Length, color = StationName)) +
  facet_grid(rows = YearSampled) +
  geom_freqpoly()
