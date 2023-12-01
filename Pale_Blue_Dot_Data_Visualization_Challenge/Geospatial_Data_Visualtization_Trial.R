#### 1.1 Loading packages. ####

library(sf)
install.packages("raster")
library(raster)
install.packages("leaflet")
library(leaflet)
install.packages("tidyverse")
suppressMessages(library(tidyverse))


#### 1.2 Loading data. ####
ETH_malaria_data <- read.csv("https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/course_materials/week1/Lab_files/Data/mal_data_eth_2009_no_dups.csv",
                             header=T)

## 2 Understanding data.
names(ETH_malaria_data)
options(tibble.width = Inf)
head(ETH_malaria_data,4)
install.packages("Hmisc")
plot(Hmisc::describe(ETH_malaria_data))

## 3 Plotting and mapping spatial data
# 3.1 Plotting using csv file.

ETH_malaria_data %>% 
  ggplot(aes(x = longitude, y = latitude))+
  geom_point()+
  labs(title = "Mapping school locations using csv dataset")
## 
ETH_malaria_data %>% 
  ggplot(aes(x = longitude, y = latitude, size = pf_pr))+
  geom_point()+
  labs(title = "Location of schools with their size representing the falciparum rate in the school")
##

### 3.2 Converting to spatial dataframe
dat <- st_as_sf(
  ETH_malaria_data,
  coords = c("longitude", "latitude"),
  crs = 4326
)


# 3.3 Visualising geometry of sf object
dat %>% 
  ggplot()+
  geom_sf()+
  labs(title = "Location of Schools")+
  xlab("Longitude")+
  ylab("Latitude")

## Elaborating the above graph
install.packages("ggspatial")
library(ggspatial)
ggplot()+
geom_sf(data = dat, aes(size = pf_pr, color = pf_pr))+
  labs(title = "Location of schools with size and color according to falciparum rate",
       color = "Falciparum Rate",
       size = "Falciparum\nRate")+
  xlab("Longitude")+
  ylab("Latitude")+
  ggspatial::annotation_north_arrow(location = "br")+
  ggspatial::annotation_scale(location = "bl")

## 3.4 Visualisation of map shape file
path <- "C:\\Users\\jsatnamsinghbhoga\\Documents\\Pale_Blue_Dot_Data_Visualization_Challenge\\ETH_Adm1_shapefile\\"
shp <- st_read(paste0(path,"\\ETH_Adm1_shapefile\\ETH_Adm_1.shp"))

head(shp)


## Overview of the Ethiopia shape file
ggplot()+
  geom_sf(data = shp)+
  labs(title = "Ethopia shapefile overview")


# 3.5 Visualisation of point data on the map.
ggplot()+
  geom_sf(data = shp)+
  geom_sf(data = dat, aes(size = pf_pr, color = pf_pr))+
  labs(title = "Location of schools studied in Ethopia with size and color according to falciparum rate",
       color = "Falciparum Rate",
       size = "Falciparum\nRate")+
  xlab("Longitude")+
  ylab("Latitude")+
  ggspatial::annotation_north_arrow(location = "br")+
  ggspatial::annotation_scale(location = "bl")

# 4 Visualisation using web maps
# 4.1 Open Street Map