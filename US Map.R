library(ggplot2)
library(usmap)
library(ggmap)
library(maps)
library(mapdata)
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

airports <- Flights %>%
  group_by(ORIGIN_CITY_NAME) %>%
  summarise(Num_Flights = n())

flights <- Flights %>%
  group_by(ORIGIN_CITY_NAME, DEST_CITY_NAME) %>%
  summarise(
    Num_Flights = n()
  )

Cities <- read.csv('uscities.csv')
Cities = Cities[c("city","state_id","lat","lng")]
colnames(Cities) <- c("city","state","lat","lon")
Cities = data.frame(Cities) %>% 
  unite('city', c("city", "state"), sep = ", ", remove = FALSE)
Cities <- Cities[,c("city","lat","lon")]

Outlier_List <- read.csv('Outliers - Feuille 3.csv')

coords <- subset(Cities, city %in% airports$ORIGIN_CITY_NAME)

ExtraCoords <- data.frame(city=c('College Station/Bryan, TX','Islip, NY','Devils Lake, ND',
                                 'North Bend/Coos Bay, OR','Deadhorse, AK','Adak Island, AK',
                                 'Champaign/Urbana, IL','Iron Mountain/Kingsfd, MI'),
                          lat=c(30.8251,40.7298,48.1128,43.4065,70.2002,51.7616,40.1164,45.8202),
                          lon=c(-96.4930,-73.2104,-98.8651,-124.2243,-148.4597,-176.6217,-88.2434,-88.0660),
                          Num_Flights=c(179,474,53,31,85,9,204,58))

colnames(coords) <- c("city","lat","lon")
Coords <- subset(coords, city %in% Outlier_List$City.Name)
Coords <- Coords[order(Coords$city),]
FlightNUM <- subset(airports, ORIGIN_CITY_NAME %in% Coords$city)
Coords <- cbind(Coords, FlightNUM$Num_Flights)
colnames(Coords) <- c("city","lat","lon","Num_Flights")
Coords <- rbind(Coords, ExtraCoords)

usa <- map_data('usa')

plot_usmap(data = Coords)

ggplot(state, aes(x=long, y=lat)) +
  geom_polygon(fill='white') +
  coord_map() +
  geom_point(data=Coords, aes(x=lon, y=lat, size=Num_Flights), color="red") + 
  labs(title = 'Map of US Airports',
       subtitle = "This is a map of the United States airports",
       x="Longitude", y="Latitude") 
