# Lauren Achee, Mandy Cameron, Jenny Evans

#Dataframes
  # Seattle Pet Liscences Dataframe
  pet_data <- read.csv("https://raw.githubusercontent.com/jenniferoseevans/seattlepetlicenses/main/Seattle_Pet_Licenses.csv")

  # Vet Locations Dataframe
  seattle_vet <- read.csv("https://raw.githubusercontent.com/jenniferoseevans/seattlepetlicenses/main/vet_data%20-%20Sheet1.csv")

  # Seattle Parks Dataframe
  parks_seattle <- read.csv("https://raw.githubusercontent.com/jenniferoseevans/seattlepetlicenses/main/Seattle_Parks.csv")

  # Seattle Zip codes dataframe
  seattle_codes_df <- read.csv("https://raw.githubusercontent.com/jenniferoseevans/seattlepetlicenses/main/All%20seattle%20zip%20codes%20excel%20-%20Sheet1.csv")


#GoogleMaps
#API Google Maps Key:AIzaSyBhNbglNb3yvusAbdKJ-JQW2teoreFky_I
  
# Install SF and rgdal
install.packages("sf")
install.packages("rgdal")
install.packages("zipcodeR")
install.packages("data.table")
install.packages("devtools")
install.packages("ggrepel")
install.packages("lubridate")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)


#libraries used.
library(tidyverse)
library(ggplot2)
library (plotly)
library(dplyr)
library("sf")
library("leaflet")
library("rgdal")
library(broom)
library(tidygeocoder)
library(zipcodeR)
library(data.table)
library("ggmap")
library(viridis)
library(maps)
library(ggrepel)
library(maptools)
library(sp)
library(lubridate)


#Separating the Date, Month, and year into separate columns.
pet_df <- pet_data %>% separate(License.Issue.Date, c("Month", "Day", "Year"))


#How has pet ownership changed overtime? Shown with a line graph
pets_by_year <- pet_df %>% group_by(Year) %>% summarise(total_pets = n())

ggplot(pets_by_year) +
  geom_line(aes(x = Year, 
                 y = total_pets,
                 group = 1)) +
  labs(title = "Pet Ownership over Time",
       x = "Year",
       y = "Amount")

#How did pet ownership change over the course of 2020?
pets_by_month <- pet_df %>% group_by(Month, Year) %>% filter(Year %in% c("2020")) %>% summarise(total_2020 = n())

  #Ordering the months chronologically
  pets_by_month$Month <- factor(pets_by_month$Month, levels = c("January", "February","March","April", "May", "June", "July", "August","September","October","November","December"))
  pets_by_month <- arrange(pets_by_month, Month)

  #Making a Line Graph of the data
  ggplot(pets_by_month) +
    geom_line(aes(x = Month, 
                y = total_2020,
                group = 1)) +
    labs(title = "Pet Ownership over 2020",
       x = "Month",
       y = "Amount")

#Where is pet ownership most popular? Grouping the code then showing in a Bar Graph
pets_by_zip <- pet_df %>% group_by(ZIP.Code) %>% summarise(sum_pet_zip = n())

  zip_popular_plot <- ggplot(pets_by_zip, aes(x = ZIP.Code,  y = sum_pet_zip, width = 1, text = paste("Zip-code:", ZIP.Code, "\nAmount:",sum_pet_zip), fill = ZIP.Code)) +
    geom_bar(stat ='identity') +
    xlim(98101,98199)+
    labs(title = "Pet Ownership by Zip code",
       x = "Zip Code",
       y = "Amount")+
    theme(legend.position = "none")
  ggplotly(zip_popular_plot, tooltip = c("text"))

#What species are found in what zipcodes?
  pets_by_type_and_zip <- pet_df %>% group_by(ZIP.Code, Species) %>% summarise(n())
  
  #Dataframe for the google maps
  king_county_zipcodeR <- search_county('King','WA')
  combined_by_zip <- merge(x=pet_data, y=king_county_zipcodeR, 
                           by.x="ZIP.Code", by.y="zipcode", na.rm=TRUE)
  
  #plain map of seattle
  ggmap::register_google(key = "AIzaSyBhNbglNb3yvusAbdKJ-JQW2teoreFky_I")
  map <- ggmap(get_googlemap(center = c(lon = -122.335167, lat = 47.608013),
                             zoom = 10, scale = 2,
                             maptype ='terrain',
                             color = 'color'))
  #DensityMap by Species
  map+stat_density2d(aes(x = lng, y = lat, fill = Species, alpha = 0.5),
                     size = 2, bins = 4, data = combined_by_zip,
                     geom = "polygon")
  
  #Densitymap overall
  map+stat_density2d(aes(x = lng, y = lat, fill = ..level.., alpha = ..level..),
                     bins = 5, geom = "polygon",
                     data = combined_by_zip) +
    scale_fill_gradient(low = "black", high = "red")
  
  #Scatterplot
  map+ geom_point(aes(x = lng, y = lat, color = Species), data = combined_by_zip,size = 1) + 
    theme(legend.position="bottom")
  

#what establishments correspond to these zip codes?
  vets_by_zip <- seattle_vet %>% group_by(zip_code) %>% summarise(n())

  vets_geo <- seattle_vet %>% geocode(vet_address, method = 'osm',
                                      lat = latitude , long = longitude)
  map_vets <- leaflet() %>%
    addTiles() %>% 
    setView(lng = -122.33410, lat = 47.606, zoom = 12) %>%  
    addMarkers(lat = vets_geo$latitude,
               lng = vets_geo$longitude, 
               popup = vets_geo$vet_name,
               label = vets_geo$vet_name)
  map_vets

#do park locations correspond with higher amounts of pet ownership?
  parks_geo <- parks_seattle %>%
    geocode(NAME, method = 'osm',
            lat = latitude , long = longitude)
  map_parks <- leaflet() %>%
    addTiles() %>% 
    setView(lng = -122.33410, lat = 47.606, zoom = 12) %>%  
    addMarkers(lat = parks_geo$latitude,
               lng = parks_geo$longitude, 
               popup = parks_geo$NAME,
               label = parks_geo$NAME)
  map_parks

#what are the top ten names amongst the dataset?

  #to summarize, sort, then slice the data for the top ten names
  top_names_over_time <- pet_df %>% group_by(Animal.s.Name) %>% summarise(top_names = n())

  top_names_first <- top_names_over_time %>% arrange(desc(top_names))

  just_top_ten <- top_names_first %>% slice_head(n = 10)

  #to make a pie chart of top ten pet names for all species
  slices <- c(410, 328, 316, 239, 221, 192, 187, 175, 174, 174)
  lbls <- c("Luna", "Charlie", "Lucy", "Bella", "Daisy", "Max", "Lily", "Rosie", "Oliver", "Penny")
  pct <- round(slices/sum(slices)*100)
  lbls <- paste(lbls, pct) 
  lbls <- paste(lbls,"%",sep="") 
  pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Top Ten Pet Names (all species)")


#What species are the most popular over time?
  pets_by_type <- pet_df %>% group_by(Species, Year) %>% summarise(pet_type_total = n())
  ggplot(pets_by_type) +
    geom_line(aes(x = Year, 
                 y = pet_type_total,
                 group = Species,
                color = Species,
                 size = .5)) +
  labs(title = "Species Ownership over Time",
        x = "Year",
        y = "Amount")

