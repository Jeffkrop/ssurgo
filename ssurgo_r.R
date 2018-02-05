
  
library(readxl)
library(knitr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(soilDB)   
library(sp)


#From prisum the dataset is 1266 rows
# I have to remove 280 from this dataset if I want soil data.
#142 locations have no latitude or longitude
#57 are duplicates
#Canada: 81 locations 
#Mexico: 28 locations 
#Both have been removed from this data.

insight <- read.csv("/Users/jeffkropelnicki/Desktop/Winfield files/2017 Corn Answer Plot Master list.csv")

insight <- insight %>% select(TrialName, GrowerName, latitude = FarmLatitude, longitude = FarmLongitude, GrowerState) %>% filter(!is.na(latitude), !is.na(longitude), longitude != -57.86900)

#Remove duplicate
insight <- insight[-which(duplicated(insight$latitude)), ]   

# There are 12 locations where the latitude and longatude are fliped below code with find it fix it and remove it.
fix_flipped <- insight %>% filter(latitude < 5) %>% 
                   select(TrialName, GrowerName, latitude = longitude, longitude = latitude, GrowerState)

insight <- rbind(fix_flipped, insight)
insight <- insight[ ! ( ( insight$latitude < 5)) , ]

fix_neg <- insight %>% filter(longitude > 1) %>% mutate(longitude = longitude * -1)
insight <- rbind(fix_neg, insight) 
insight <- insight[ ! ( ( insight$longitude > 1)) , ]

#Fix two longitudes that are missing the negitive sign.
insight$latitude[ insight$latitude  ==  442440.21000 ] <- 44.244021
insight$longitude[ insight$longitude  ==  -95102.52000 ] <- -95.10252


#Add a column for ID to join to after I get soil data.
insight <- tibble::rowid_to_column(insight, "ID")

#Save as csv as clean data if I need to map it with ArcGIS
#write_csv(insight, "clean_soybean_insight.csv" )


#Get a map of Canada, Mexico and the USA
world_map <- map_data("world")
world_map <- world_map %>% filter(region == c("Canada", "Mexico"))
usa_map <- map_data("state")


# Draw the map and add the data points in myData
ggplot() +
  geom_path(data = usa_map, aes(x = long, y = lat, group = group)) +
  geom_path(data = world_map, aes(x = long, y = lat, group = group)) +
  geom_point(data = insight, aes(x = longitude, y = latitude), color = "red") +
  labs("Origin of Cocoa Beans", x = "Longitude", y = "Latitude") +
  theme(plot.title=element_text(size=20))    

#Need to remove Canada and Mexico because I cant get soil data out of the USA
#Canada: QC = 62, ON = 19 means we lose 81 locations 
#Mexico: (loc 11) = 2, (loc 5) = 12, (loc 8) = 14 means we lose 28 locations 
locations <- insight %>% filter(GrowerState != "QC", GrowerState != "11", GrowerState != "5", GrowerState != "8", GrowerState != "ON") %>% select(ID, latitude, longitude)

locations <- insight %>% filter(State != "CA", State != "QC") %>% select(ID, latitude, longitude)

#locations <- locations[1:2,]
#Data frame of lat lon
xy <- locations[,c(3,2)]

#convert to spatial point data frame
soil <- SpatialPointsDataFrame(coords = xy, data = locations,
                               proj4string = CRS("+proj=longlat +datum=WGS84"))

#Get soil data https://cran.r-project.org/web/packages/soilDB/soilDB.pdf
#http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html
system.time(soil <-SDA_query_features(soil, id='ID'))


mid_soil <- soil %>% separate(muname, c("soil", "sand", "slope", "con"), ", ") %>% 
             tidyr::extract(soil, c("Name", "soil"), "([^ ]+) (.*)") %>% 
             select(ID, soil) 


mid_soil  <- left_join(mid_soil, insight, by = "ID")

#write.csv(mid_soil, "not_clean_insight_soil_data.csv")


#Rename to save
clean_soil <- mid_soil

clean_soil$soil <- gsub("\\s*\\([^\\)]+\\)\\s*$","",clean_soil$soil)
clean_soil$soil[clean_soil$soil == "loams"] <- "loam"
clean_soil$soil <- gsub("sands", "sand", clean_soil$soil)
clean_soil$soil <- gsub("clays", "clay", clean_soil$soil)
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
clean_soil$soil <- gsub("Page-McIntosh ", "", clean_soil$soil)
clean_soil$soil <- gsub("Desha ", "", clean_soil$soil)
clean_soil$soil <- gsub("land-Udorthents ", "", clean_soil$soil)
clean_soil$soil <- gsub("Monongahela ", "", clean_soil$soil)
clean_soil$soil <- gsub("Poxmash ", "", clean_soil$soil)
clean_soil$soil <- gsub("Prairie ", "", clean_soil$soil)
clean_soil$soil <- gsub("Shelby ", "", clean_soil$soil)
clean_soil$soil <- gsub(" (occasionally flooded)", "", clean_soil$soil)
clean_soil$soil <- gsub("Uly ", "", clean_soil$soil)
clean_soil$soil <- gsub("outcrop-Nordness ", "", clean_soil$soil)

clean_soil <- clean_soil %>% filter(soil != "(Nauvoo) fine sandy loam",
                                    soil != "soils",
                                    soil != "Digital Data Available",
                                    soil != "land",
                                    soil != "mucky silty clay loam",
                                    soil != "or quarry",
                                    soil != "pits",
                                    soil != "Purdam soils",
                                    soil != "Rexor soils",
                                    soil !=  "Cathro")

#write.csv(clean_soil, "clean_insight_soil_data.csv")

#After removing 33 locations that do not return good sail data we are left with 925



n4 <- clean_soil %>% group_by(soil) %>%
             summarise(count = n()) %>%
             arrange(desc(count))


map_soil <- clean_soil 
map_soil$soil_texture <- map_soil$soil
map_soil$soil_texture[ map_soil$soil_texture ==  "loam" ] <- "Medium"
map_soil$soil_texture[ map_soil$soil_texture ==  "silt loam" ] <- "Medium"
map_soil$soil_texture[ map_soil$soil_texture ==  "silty clay loam" ] <- "Fine"
map_soil$soil_texture[ map_soil$soil_texture ==  "complex" ] <- "Medium"
map_soil$soil_texture[ map_soil$soil_texture ==  "clay loam" ] <- "Fine"
map_soil$soil_texture[ map_soil$soil_texture ==  "fine sandy loam" ] <- "Coarse"
map_soil$soil_texture[ map_soil$soil_texture ==  "sandy loam" ] <- "Coarse"
map_soil$soil_texture[ map_soil$soil_texture ==  "loamy fine sand" ] <- "Coarse"
map_soil$soil_texture[ map_soil$soil_texture ==  "clay" ] <- "Fine"
map_soil$soil_texture[ map_soil$soil_texture ==  "loamy sand" ] <- "Coarse"
map_soil$soil_texture[ map_soil$soil_texture ==  "silty clay" ] <- "Fine"
map_soil$soil_texture[ map_soil$soil_texture ==  "fine sand" ] <- "Coarse"
map_soil$soil_texture[ map_soil$soil_texture ==  "sand" ] <- "Coarse"
map_soil$soil_texture[ map_soil$soil_texture ==  "muck" ] <- "Fine"
map_soil$soil_texture[ map_soil$soil_texture ==  "Black clay" ] <- "Fine"
map_soil$soil_texture[ map_soil$soil_texture ==  "sandy clay loam" ] <- "Medium"

map_soil <- map_soil %>% select(Book.Name, soil, Soil.Texture, soil_texture, latitude, longitude)

map_soil <- map_soil %>% summarise(see = map_soil$Soil.Texture ==  map_soil$soil_texture)

#write_csv(map_soil, "soybean insight soil data.csv")


n5 <- map_soil %>% group_by(soil_texture) %>%
  summarise(count = n()) %>%
  arrange(desc(count))



