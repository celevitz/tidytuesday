# 2023-06-18
# Carly Levitz
# Tidy Tuesday: UFO sightings
# Source: https://github.com/rfordatascience/tidytuesday/blob/master/data/2019/2019-06-25/readme.md
# My goal this week: incorporate icons or pictures.

# Set up, bring in the data

rm(list=ls())
library(ggplot2)
library(tidytuesdayR)
library(stringr)
library(dplyr)
library(tidyr)
library(colorBlindness)
library(ggimage)
library(ggpubr)

savedirectory <- "/Users/carlylevitz/Documents/Data/tidytuesday/"

## Select the data I want to use
## Look at proportion of sightings by time of day for just the countries with
##     more than 100 sightings over time
sightings0 <- tidytuesdayR::tt_load(2023, week = 25)$ufo_sightings %>%
  filter(country_code %in% c("AU","CA","GB","IN","MX","NZ","US","ZA"))

# clean the data: there are some with day-part missing
  #table(sightings0$reported_date_time[is.na(sightings0$day_part)])
  # there are too many of these to research. So I'm going to put them as
  #     'not reported'

  sightings0$day_part[is.na(sightings0$day_part)] <- "unknown"
  sightings0$day_part[sightings0$day_part == "NA"] <- "unknown"

# analyze the data
sightings <- sightings0 %>%
  mutate(year = substr(reported_date_time,1,4)) %>%
  select(country_code,day_part,year) %>%
  group_by(country_code,day_part,year) %>%
  summarise(count=n()) %>%
  ungroup() %>% group_by(country_code,year) %>%
  mutate(total = sum(count)) %>%
  mutate(percent = count/total) %>%
  mutate(day_part = factor(day_part,
                           levels = c("astronomical dawn","nautical dawn"
                                      ,"civil dawn","morning","afternoon"
                                      ,"civil dusk","nautical dusk"
                                      ,"astronomical dusk","night"
                                      ,"unknown"))) %>%
  # exclude years prior 1960, because all but one year had fewer than
  # 10 sightings across all countries combined
  filter(year >= 1960)

### Explore the data: Looking at it by country, I decided that it was too
### much to look at all at once
sightings %>%
  ggplot(aes(x=year,y=percent,fill=day_part,color=day_part)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~country_code) +
  scale_fill_manual(values=c(Brown2Blue10Steps[1:9],"gray50")) +
  scale_color_manual(values=c(Brown2Blue10Steps[1:9],"gray50"))

### So I decided to just look at it for the US
usSightings <- sightings %>%
  filter(country_code == "US") %>%
  mutate(labelstartstop = case_when(year %in% c("1960", "2023") ~
                                paste0(day_part," (",round(percent*100,1),"%)")
                               ,TRUE ~ NA))

graphtitle1 <- str_glue("At what time of day were UFOs sighted in the US from<br>1960 onward?")
graphsubtitle1 <- str_glue("Most reported sightings of UFOs in the US were during the night, followed by the<br>afternoon, astronomical dusk, and nautical dusk.")
graphtitle2 <- str_glue("How many UFO sightings were reported in the US each year?")
graphsubtitle2 <- str_glue("Reports of UFO sightings increased starting in 1995. There was a decrease in sightings<br>after 2020...do you think COVID-19 affects aliens? #maskup #publichealth")
graphcaption <- str_glue("<br><br>Tidy Tuesday 2023 Week 25 /// Source: National UFO Reporting Center /// Flying Saucer Emoji from Emojipedia.org <br><br>Visualization: Twitter @carlylevitz /// code: github.com/celevitz/tidytuesday <br> Tools: rstats, ggplot, tidyverse, ggtext, ggimage, ggpubr, colorBlindness <br>This week I learned: how to incorporate images into ggplot, and how to allow labels to go outside plot margin.")

graph1 <- usSightings %>%
    ggplot(aes(x=year,y=percent,group=day_part,color=day_part
               ,fill=day_part,label=labelstartstop)) +
    coord_cartesian(clip = "off") +
    geom_line()+
    geom_text(data=usSightings %>% filter(year == "1960")
              ,aes(x=year,y=percent,label=labelstartstop)
              ,hjust=1,size = 8) +
    geom_text(data=usSightings %>% filter(year == "2023")
              ,aes(x=year,y=percent,label=labelstartstop)
              ,hjust=0, size = 8) +
    labs(title = graphtitle1
         ,subtitle = graphsubtitle1) +
    scale_fill_manual(values=c(Brown2Blue10Steps[2:10],"gray95")) +
    scale_color_manual(values=c(Brown2Blue10Steps[2:10],"gray95")) +
    scale_y_continuous(lim=c(0,.65)) +
    scale_x_discrete(breaks=usSightings$year[seq(1
                                         ,length(unique(usSightings$year)),5)]
                     ,labels=paste0("'",substr(usSightings$year,3,4) ) [seq(1
                                      ,length(unique(usSightings$year)),5)])  +
    theme_minimal() +
    theme(panel.grid=element_blank()
          ,panel.background = element_rect(fill="gray40",color="gray40")
          ,plot.background = element_rect(fill="gray40",color="gray40")
          ,legend.position = "none"
          ,axis.line.x = element_line(color="gray95")
          ,axis.ticks.x = element_line(color="gray95")
          ,axis.title.x = element_blank()
          ,axis.text.x = element_markdown(color = "gray95",size = 25)
          ,axis.line.y = element_blank()
          ,axis.ticks.y = element_blank()
          ,axis.title.y = element_blank()
          ,axis.text.y = element_blank()
          ,plot.title  = element_markdown(color = "gray95",size = 40
                                             , face = "bold",hjust=0
                                             ,margin = margin(t=10,b=10,l=10,r=50))
          ,plot.margin    = margin(t=10,r=300,b=10,l=300)
          ,plot.subtitle  = element_markdown(size=30,hjust=0,color="gray95"))

graph2data <- usSightings %>%
  group_by(year) %>%
  summarise(total = sum(total)) %>%
  mutate(labelstartstop = case_when(year %in% c("1960","2014", "2023") ~paste0(total," in ",year)
                                           ,TRUE ~ NA)
    ,image = paste(savedirectory,"TidyTuesday2023Week25flying-saucer_1f6f8.png",sep=""))

graph2 <-
  graph2data %>%
  ggplot(aes(x = year,y=total)) +
  coord_cartesian(clip="off")+
  geom_image(aes(image=image), size=.05) +
  geom_text(data=graph2data %>% filter(year == "1960")
            ,aes(x=year,y=total,label=labelstartstop)
            ,hjust=1,size = 8,color = "gray95",nudge_x = -1) +
  geom_text(data=graph2data %>% filter(year %in% c("2014", "2023"))
            ,aes(x=year,y=total,label=labelstartstop)
            ,hjust=0, size = 8,color="gray95",nudge_x = 1) +
  scale_x_discrete(breaks=graph2data$year[seq(1
                                         ,length(unique(graph2data$year)),5)]
                   ,labels=paste0("'",substr(graph2data$year,3,4) ) [seq(1
                                        ,length(unique(graph2data$year)),5)])+
  labs(title = graphtitle2
       ,subtitle = graphsubtitle2
       ,caption = graphcaption) +
  theme_minimal() +
  theme(panel.grid=element_blank()
        ,panel.background = element_rect(fill="gray40",color="gray40")
        ,plot.background = element_rect(fill="gray40",color="gray40")
        ,legend.position = "none"
        ,axis.line.x = element_line(color="gray95")
        ,axis.ticks.x = element_line(color="gray95")
        ,axis.title.x = element_blank()
        ,axis.text.x = element_markdown(color = "gray95",size = 25)
        ,axis.line.y = element_blank()
        ,axis.ticks.y = element_blank()
        ,axis.title.y = element_blank()
        ,axis.text.y = element_blank()
        ,plot.title  = element_markdown(color = "gray95",size = 40
                                        , face = "bold",hjust=0
                                        ,margin = margin(t=10,b=10,l=10))
        ,plot.margin    = margin(t=10,r=300,b=10,l=300)
        ,plot.caption  = element_markdown(size=25,hjust=.5,color="green")
        ,plot.subtitle  = element_markdown(size=30,hjust=0,color="gray95"))




ggarrange(graph1,graph2,nrow=2,ncol=1)
  dev.print(png, file = paste(savedirectory,"TidyTuesday2023Week25.png",sep="")
            , width = 1600, height = 1600)
  dev.off()




