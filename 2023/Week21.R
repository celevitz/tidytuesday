# Carly Levitz
# Date written: 2023-05-28
# Date updated: 2023-05-28
# Purpose: create a viz for 2023 Week 21 tidy tuesday

rm(list=ls())

library(tidyverse); library(tidytuesdayR); library(ggplot2)

tuesdata <- tidytuesdayR::tt_load(2023, week = 21)

squirrel_data <- tuesdata$squirrel_data

## Age and activities: keep only the data I'm interested in

sq <- squirrel_data %>%
  # drop unknown ages
  filter(Age %in% c("Adult","Juvenile")) %>%
  select(`Unique Squirrel ID`,Age, Running, Chasing, Climbing, Eating, Foraging) %>%
  pivot_longer(!c(`Unique Squirrel ID`,Age),values_to = "truefalse",names_to="activity") %>%
  group_by(Age,activity,truefalse) %>%
  summarise(n=n()) %>%
  # get the percent
  left_join(squirrel_data %>% group_by(Age) %>% summarise(N=n())) %>%
  mutate(percent=n/N,fulllabel=paste0(round(percent,3)*100,"%\nn=",n)) %>%
  # Compute the position of labels
  arrange(fulllabel) %>%
  mutate(prop = percent / sum(percent) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

## Plot the figure
  sq %>%
    ggplot(aes(x="", y=percent, fill=truefalse,label=fulllabel),color="cyan4")+
    geom_bar(width = 1, stat = "identity") +
    facet_wrap(activity~Age,ncol=2)+
    coord_polar("y", start=0) +
    ylab("") + xlab("") +
    scale_fill_manual(values=c("gray75","cyan4")) +
    theme_void() +
    theme(legend.position="none")









