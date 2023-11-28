library(ggplot2)
library(usmap)
library(ggmap)
library(maps)
library(dplyr)
library(data.table)
library(stringr)
library(tidyverse)

Flights <- read.csv('Flights1_2019_1.csv')
airports <- unique(c(Flights$ORIGIN_CITY_NAME, Flights$DEST_CITY_NAME))
airports <- data.frame(airport=airports)
airports <- strsplit(airports$airport, ',')
airports <- data.frame(airport=airports)
airports <- transpose(airports)
colnames(airports) <- c("City", "State")
airports$State = str_trim(airports$State)

airports = data.frame(airports) %>% 
  unite('City', c("City", "State"), sep = ",", remove = FALSE)
airports <- airports[,"City"]
airports <- data.frame(airports)

Cities = data.frame(Cities) %>% 
  unite('city', c("city", "state"), sep = ",", remove = FALSE)
Cities <- Cities[,c("city","lat","lon")]

coords <- subset(Cities, city %in% airports$airports)
airports <- data.frame(airport=airports, coords)

Cities <- read.csv('uscities.csv')
Cities = Cities[c("city","state_id","lat","lng")]
colnames(Cities) <- c("city","state","lat","lon")

plot_usmap(regions = "states") + 
  labs(title = "U.S. States",
       subtitle = "This is a blank map of the United States.") + 
  theme(panel.background=element_blank())

usa <- map_data('usa')

usmap = ggplot(usa, aes(x=long, y=lat)) +
  geom_polygon(fill='white') +
  geom_point(data=coords, aes(x=lon, y=lat), color="red", size=0.3) + 
  labs(title = 'Map of US Airports',
       subtitle = "This is a map of the United States airports",
       x="Longitude", y="Latitude")

usmap + theme(
  panel.grid = element_blank()
)

+
  geom_line(data=coords, aes(x=lon, y=lat))