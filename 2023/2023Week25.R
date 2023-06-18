# 2023-06-18
# Carly Levitz
# Tidy Tuesday:

# Set up, bring in the data

rm(list=ls())
library(ggplot2)
library(tidytuesdayR)
library(ggtext)
library(dplyr)
library(tidyr)

savedirectory <- "/Users/carlylevitz/Documents/Data/tidytuesday/"

## Select the data I want to use
## Look at proportion of sightings by time of day for just the countries with
##     more than 100 sightings over time
sightings0 <- tidytuesdayR::tt_load(2023, week = 25)$ufo_sightings %>%
  filter(country_code %in% c("AU","CA","GB","IN","MX","NZ","US","ZA"))

# clean the data: there are some with day-part missing
  table(sightings0$reported_date_time[is.na(sightings0$day_part)])
  # there are too many of these to research. So I'm going to put them as
  #     'not reported'

  sightings0$day_part[is.na(sightings0$day_part)] <- "unknown"

# analyze the data
sightings <- sightings0 %>%
  mutate(year = substr(reported_date_time,1,4)) %>%
  select(country_code,day_part,year) %>%
  group_by(country_code,day_part,year) %>%
  summarise(count=n()) %>%
  ungroup() %>% group_by(country_code,year) %>%
  mutate(total = sum(count)) %>%
  mutate(percent = count/total) %>%
  filter(!(is.na(day_part)))

##
sightings %>%
  ggplot(aes(x=year,y=percent,color=day_part)) +
  geom_point() +
  facet_wrap(~country_code)






