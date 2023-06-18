# 2023-06-14
# Carly Levitz
# Tidy Tuesday: SAFI data - https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-06-13/readme.md

# Set up, bring in the data

rm(list=ls())
library(ggplot2); library(tidytuesdayR); library(tidyverse); library(ggtext)

rawdata <-tidytuesdayR::tt_load(2023, week = 24)$`safi_data`
savedirectory <- "/Users/carlylevitz/Documents/Data/tidytuesday/"

# Understanding the data
summary(rawdata$years_liv)
firstquartile <- summary(rawdata$years_liv)[1]
secondquartile <- summary(rawdata$years_liv)[2]
median <- summary(rawdata$years_liv)[3]
fourthquartile <- summary(rawdata$years_liv)[5]
max <- summary(rawdata$years_liv)[6]

rawdata %>% ggplot(aes(x=years_liv)) +geom_histogram()
table(rawdata$affect_conflicts)

# Data Cleaning
  data <- rawdata %>%
    # Not needed, but it felt good to clean this up
    mutate(respondent_wall_type = case_when(respondent_wall_type == " burntbricks" ~ "burntbricks"
                                           ,respondent_wall_type == " muddaub" ~ "muddaub"
                                           ,TRUE ~ respondent_wall_type)
           # create categories of how long people had lived there
           ,yearslived = factor(case_when(years_liv >= firstquartile & years_liv < secondquartile ~ "1st quartile"
                                  ,years_liv >= secondquartile& years_liv < median ~ "2nd quartile"
                                  ,years_liv >= median & years_liv < fourthquartile ~ "3rd quartile"
                                  ,years_liv >= fourthquartile & !(is.na(years_liv)) ~ "4th quartile")
                                ,levels=c("4th quartile","3rd quartile","2nd quartile","1st quartile"))
           # clean up and order the categories of whether they've been affected by a conflict with another irrigator
           ,affectedbyconflicts = factor(case_when(affect_conflicts == "more_once" ~ "more than once"
                                            ,is.na(affect_conflicts) ~ "unknown"
                                            ,affect_conflicts == "NULL" ~ "unknown"
                                            ,TRUE ~ affect_conflicts)
                                         ,levels=c("unknown","frequently","more than once","once","never"))) %>%
    # summarise the data
    group_by(yearslived,affectedbyconflicts) %>%
    mutate(n=n()) %>%
    ungroup() %>% group_by(yearslived) %>%
    mutate(N=n()) %>%
    select(yearslived,affectedbyconflicts,n,N) %>%
    distinct() %>%
    mutate(percent = n/N
           ,graphlabel = paste0(round(percent*100,1),"%\n(n=",n,")"))

## Set up the graph stuff
  xaxistitle <- "Percent of households"
  yaxistitle <- "Years lived in this village"
  maintitle <- "How is length of time lived in a village related to conflict frequency?"
  subtitletext <- "Interviews took place between Nov. 2016 and June 2017 in the villages of Chirodzo, God, and Ruaca"

  captiontext <- str_glue("Tidy Tuesday 2023 Week 23 /// Note: 'Years lived' was categorized by quartiles.
                          Source: Data comes from the SAFI (Studying African Farmer-Led Irrigation) survey, a subset of the data used in the Data Carpentry Social Sciences workshop.
                          Citation: Woodhouse, P; Veldwisch, GJ; Brockington, D; Komakech, HC; Manjichi, A; Venot, JP (2018): SAFI Survey Results. doi:10.6084/m9.figshare.6262019.v1
                          Visualization: Twitter @carlylevitz /// code: github.com/celevitz/tidytuesday /// Tools: #rstats #ggplot #tidyverse #ggtext")


## Graph it
  data %>% ggplot(aes(x=percent,y=yearslived,fill=affectedbyconflicts)) +
    geom_bar(stat="identity",color="gray20") +
    annotate("text",x=0.1,y=data$yearslived[data$affectedbyconflicts == "never"]
             ,label=data$graphlabel[data$affectedbyconflicts == "never"]
             ,color="gray20",hjust=0,size=5) +
    annotate("text",x=.49
             ,y=data$yearslived[data$affectedbyconflicts == "more than once"]
             ,label=data$graphlabel[data$affectedbyconflicts == "more than once"]
             ,color="gray20",hjust=0,size=5) +
    annotate("text",x=.94
             ,y=data$yearslived[data$affectedbyconflicts == "unknown"]
             ,label=data$graphlabel[data$affectedbyconflicts == "unknown"]
             ,color="gray20",hjust=1,size=5) +
    scale_fill_manual(values=c("#C5D6D8","#60695C","#ABDF75","#99F7AB","#D6F9DD")) +
    xlab(xaxistitle) + ylab(yaxistitle) +
    scale_x_continuous(lim=c(0,1),breaks=seq(0,1,.2),labels=paste0(round(seq(0,1,.2)*100,1),"%"))+
    scale_y_discrete(labels=c(paste0("[",fourthquartile," to ",max,"] years")
                              ,paste0("[",median," to ",fourthquartile,") years")
                              ,paste0("[",secondquartile," to ",median,") years")
                              ,paste0("[",firstquartile," to ",secondquartile,") years") )) +
    labs(title = maintitle,subtitle = subtitletext,caption = captiontext) +
    guides(fill=guide_legend(title="Frequency of conflicts") ) +
    theme_minimal() +
    theme(panel.grid = element_blank()
          ,panel.background = element_rect(color="beige", fill="beige")
          ,plot.background  = element_rect(color="beige", fill="beige")
          ,plot.title       = element_markdown(color = "gray20",size = 32, face = "bold",margin = margin(t=10,b=10,l=-140))
          ,plot.margin       = margin(t=10,r=40,b=10,l=10)
          ,axis.line.x      = element_line(color = "gray20")
          ,axis.text        = element_markdown(color="gray20",size=20)
          ,axis.title        = element_markdown(color="gray20",size=20,face = "bold")
          ,plot.caption     = element_text(size=12,hjust=.5,color="gray20")
          ,plot.subtitle     = element_text(size=20,hjust=.5,color="gray20")
          ,legend.key.size = unit(2, 'cm')
          ,legend.title = element_markdown(size=15,color="gray20",face = "bold")
          ,legend.text = element_text(size=15,color="gray20") )

dev.print(png, file = paste(savedirectory,"TidyTuesday2023Week24.png",sep=""), width = 1100, height = 900)
dev.off()

