---
  title: "SSURGO Data"
date: "`r format(Sys.Date())`"


  
  
library(readxl)
library(knitr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)


#114 locations have no latitude or longitude
#57 are duplicates
#Both have been removed from this data.

insight <- read_excel("insight.xlsx")
insight <- insight %>% select(TrialName, GrowerName, latitude = FarmLatitude, longitude = FarmLongitude, GrowerState) %>% filter(!is.na(latitude), !is.na(longitude))

#Remove duplicate
insight <- insight[-which(duplicated(insight$latitude)), ]   

# There are 12 locations where the latitude and longatude are fliped below code with find it fix it and remove it.
fix <- insight %>% filter(latitude < 5) %>% select(TrialName, GrowerName, latitude = longitude, longitude = latitude, GrowerState)
insight <- rbind(fix, insight)
insight <- insight[ ! ( ( insight$latitude < 5)) , ]

insight$longitude <- gsub("107.07168","-107.07168", insight$longitude)
insight$longitude <- gsub("98.21478","-98.21478", insight$longitude)


world_map <- map_data("state")

# Draw the map and add the data points in myData
ggplot() +
  geom_path(data = world_map, aes(x = long, y = lat, group = group)) +
  geom_point(data = insight, aes(x = longitude, y = latitude), color = "red") +
  labs("Origin of Cocoa Beans", x = "Longitude", y = "Latitude") +
  theme(plot.title=element_text(size=20))


