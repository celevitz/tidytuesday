# Carly Levitz
# 2023-05-30
# Tidy Tuesday: Week 22
# Centenarians

rm(list=ls())
library(ggplot2); library(tidytuesdayR); library(mapdata); library(maps); library(tidyverse); library(highcharter); library(RColorBrewer); library(maptools)

ddf <- tidytuesdayR::tt_load(2023, week = 22)$centenarians %>%
  group_by(place_of_death_or_residence) %>%
  summarise(value=n()) %>%
  rename(country=place_of_death_or_residence)


plotPascal <- function() {

  pal <- colorRampPalette(brewer.pal(9, 'Reds'))(length(ddf$value))
  pal <- pal[with(ddf, findInterval(value, sort(unique(value))))]

  col <- rep(grey(0.8), length(wrld_simpl@data$NAME))
  col[match(ddf$country, wrld_simpl@data$NAME)] <- pal

  plot(wrld_simpl, col = col)

}


# align colors to countries

ddf$brk <- cut(ddf$value,
               breaks=c(sort(ddf$value)),
               labels=as.character(ddf[order(ddf$value),]$country),
               include.lowest=TRUE)

# this lets us use the contry name vs 3-letter ISO
wrld_simpl@data$id <- wrld_simpl@data$NAME

wrld <- fortify(wrld_simpl, region="id")
wrld <- subset(wrld, id != "Antarctica") # we don't rly need Antarctica

gg <- ggplot()

# setup base map
gg <- gg + geom_map(data=wrld, map=wrld, aes(map_id=id, x=long, y=lat), fill="white", color="#7f7f7f", size=0.25)

# add our colored regions
gg <- gg + geom_map(data=ddf, map=wrld, aes(map_id=country, fill=value),  color="white", size=0.25)

# this sets the scale and, hence, the legend
gg <- gg + scale_fill_manual(values=colorRampPalette(brewer.pal(9, 'Reds'))(length(ddf$value)),
                             name="Country")

# this gives us proper coords. mercator proj is default
gg <- gg + coord_map()
gg <- gg + labs(x="", y="")
gg <- gg + theme(plot.background = element_rect(fill = "transparent", colour = NA),
                 panel.border = element_blank(),
                 panel.background = element_rect(fill = "transparent", colour = NA),
                 panel.grid = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 legend.position = "right")
gg
