---
  title: "SSURGO Data"
date: "`r format(Sys.Date())`"


  
  
library(readxl)
library(knitr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(soilDB)   
library(sp)

# I have to remove 280 from this dataset if I want soil data.
#114 locations have no latitude or longitude
#57 are duplicates
#Canada: 81 locations 
#Mexico: 28 locations 
#Both have been removed from this data.

insight <- read_excel("insight.xlsx")
insight <- insight %>% select(TrialName, GrowerName, latitude = FarmLatitude, longitude = FarmLongitude, GrowerState) %>% filter(!is.na(latitude), !is.na(longitude), longitude != -57.86900)

#Remove duplicate
insight <- insight[-which(duplicated(insight$latitude)), ]   

# There are 12 locations where the latitude and longatude are fliped below code with find it fix it and remove it.
fix <- insight %>% filter(latitude < 5) %>% 
                   select(TrialName, GrowerName, latitude = longitude, longitude = latitude, GrowerState)


insight <- rbind(fix, insight)
insight <- insight[ ! ( ( insight$latitude < 5)) , ]

#Fix two longitudes that are missing the negitive sign.
insight$longitude[ insight$longitude  ==  98.21478 ] <- -98.21478
insight$longitude[ insight$longitude  ==  107.07168 ] <- -107.07168

#Add a column for ID to join to after I get soil data.
insight <- tibble::rowid_to_column(insight, "ID")



#Save as csv as clean data if I need to map it with ArcGIS
write_csv(insight, "clean_insight.csv" )


#Get a map of Canada, Mexico and the USA
world_map <- map_data("world")
world_map <- world_map %>% filter(region == c("Canada", "Mexico"))
usa_map <- map_data("state")


# Draw the map and add the data points in myData
ggplot() +
  geom_path(data = usa_map, aes(x = long, y = lat, group = group)) +
  #geom_path(data = world_map, aes(x = long, y = lat, group = group)) +
  geom_point(data = clean_soil, aes(x = longitude, y = latitude), color = "red") +
  labs("Origin of Cocoa Beans", x = "Longitude", y = "Latitude") +
  theme(plot.title=element_text(size=20))    

#Need to remove Canada and Mexico because I cant get soil data out of the USA
#Canada: QC = 62, ON = 19 means we lose 81 locations 
#Mexico: (loc 11) = 2, (loc 5) = 12, (loc 8) = 14 means we lose 28 locations 
locations <- insight %>% filter(GrowerState != "QC", GrowerState != "11", GrowerState != "5", GrowerState != "8", GrowerState != "ON") %>% select(ID, latitude, longitude, GrowerName)

#Data frame of lat lon
xy <- locations[,c(3,2)]

#convert to spatial point data frame
soil <- SpatialPointsDataFrame(coords = xy, data = locations,
                               proj4string = CRS("+proj=longlat +datum=WGS84"))

#Get soil data
system.time(soil <-SDA_query_features(soil, id='ID'))


mid_soil <- soil %>% separate(muname, c("soil", "sand", "slope", "con"), ", ") %>% 
             tidyr::extract(soil, c("Name", "soil"), "([^ ]+) (.*)") %>% 
             select(ID, soil) 


mid_soil  <- left_join(mid_soil, locations, by = "ID")

#Rename to save
clean_soil <- mid_soil


clean_soil$soil[clean_soil$soil == "loams"] <- "loam"

clean_soil$soil <- gsub("loams", "loam", clean_soil$soil)
clean_soil$soil <- gsub("very ", "", clean_soil$soil)
clean_soil$soil <- gsub("and ", "", clean_soil$soil)
clean_soil$soil <- gsub("gravelly ", "", clean_soil$soil)
clean_soil$soil <- gsub("Charles ", "", clean_soil$soil)
clean_soil$soil <- gsub("Carroll ", "", clean_soil$soil)
clean_soil$soil <- gsub("Sueur ", "", clean_soil$soil)
clean_soil$soil <- gsub("Bend-Beotia ", "", clean_soil$soil)
clean_soil$soil <- gsub("Byron ", "", clean_soil$soil)
clean_soil$soil <- gsub("Park ", "", clean_soil$soil)
clean_soil$soil <- gsub("Reinach ", "", clean_soil$soil)
clean_soil$soil <- gsub("Bay ", "", clean_soil$soil)
clean_soil$soil <- gsub("Center ", "", clean_soil$soil)
clean_soil$soil <- gsub("channery ", "", clean_soil$soil)
clean_soil$soil <- gsub("Creek-Romnell ", "", clean_soil$soil)
clean_soil$soil <- gsub("Etowah ", "", clean_soil$soil)
clean_soil$soil <- gsub("Coly ", "", clean_soil$soil)
clean_soil$soil <- gsub("Farge ", "", clean_soil$soil)
clean_soil$soil <- gsub("Fear ", "", clean_soil$soil)
clean_soil$soil <- gsub("Funmar ", "", clean_soil$soil)
clean_soil$soil <- gsub("Funmar ", "", clean_soil$soil)
clean_soil$soil <- gsub("gullied ", "", clean_soil$soil)
clean_soil$soil <- gsub("Kill shaly ", "", clean_soil$soil)
clean_soil$soil <- gsub("land-Bloomfield-Alvin ", "", clean_soil$soil)
clean_soil$soil <- gsub("land-Hayden-Kingsley ", "", clean_soil$soil)
clean_soil$soil <- gsub("land-Zimmerman ", "", clean_soil$soil)
clean_soil$soil <- gsub("lcomplex", "complex", clean_soil$soil)
clean_soil$soil <- gsub("Naron ", "", clean_soil$soil)
clean_soil$soil <- gsub("Ostrander ", "", clean_soil$soil)
clean_soil$soil <- gsub("outcrop-Copaston ", "", clean_soil$soil)
clean_soil$soil <- gsub("Rey ", "", clean_soil$soil)
clean_soil$soil <- gsub("River ", "", clean_soil$soil)
clean_soil$soil <- gsub(" eroded undulating phase", "", clean_soil$soil)
clean_soil$soil <- gsub("Sueur-Lester ", "", clean_soil$soil)
clean_soil$soil <- gsub("Sullivan ", "", clean_soil$soil)
clean_soil$soil <- gsub("Twin-Walnut grove ", "", clean_soil$soil)
clean_soil$soil <- gsub("variant ", "", clean_soil$soil)

clean_soil <- clean_soil %>% filter(soil != "(Nauvoo) fine sandy loam",
                                    soil != "soils",
                                    soil != "Digital Data Available",
                                    soil != "land",
                                    soil != "mucky silty clay loam",
                                    soil != "or quarry",
                                    soil != "pits",
                                    soil != "Purdam soils",
                                    soil != "Rexor soils")

n4 <- clean_soil %>% group_by(soil) %>%
             summarise(count = n()) %>%
             arrange(desc(count))


