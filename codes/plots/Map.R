# Empty the environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(readxl)
library(ggOceanMaps)
library(sp)

# Import data -------------------------------------------------------------
D_geo1 <- read_excel("Data/Site_Coords.xlsx")

# Map of the studies ------------------------------------------------------

## Convert coordinates
D_geo <- D_geo1

for(n in 1:nrow(D_geo) ){
  D_geo$lat[n] <- D_geo1$lat[n] %>%
    sub('-', 'd', .) %>%
    sub('-', '\'', .) %>%
    sub('\\\\', '" ', .) %>%
    char2dms %>%
    as.numeric
  D_geo$long[n] <- D_geo1$lon[n] %>%
    sub('-', 'd', .) %>%
    sub('-', '\'', .) %>%
    sub('\\\\', '" ', .) %>%
    char2dms %>%
    as.numeric
}

D_geo <- D_geo %>% mutate_at(vars(lat, long), as.numeric)

d <- as.matrix(dist(cbind(D_geo$lat, D_geo$long)))
rownames(d) <- D_geo$site; colnames(d) <- D_geo$site

(map <- basemap(limits = c(50,-50, 50, 80), shapefiles = "ArcticStereographic", land.col = "#F8D7C5") + 
    geom_spatial_label_repel(data = D_geo, aes(x = long, y = lat, label = study_ind), color = "black", max.overlaps = 15))

ggsave("Plots/MapStudy.png", map, width = 5.5, height = 5, dpi = 500, bg = "white")
