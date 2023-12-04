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




#### Experiment from 4th Dec #####
## Note: First replicated a code from web for a similar dataset. Then downloaded one of the suggested datasets for 1 variable.
# For this one variable, created 2 plots from the lat, long, and the value of the varaibel of interest viz. CO values

install.packages("rhdf5")
library(rhdf5)
install.packages("hdf5r")
library(hdf5r)
library(hdf5r)
path <- "/Users/jyotibhogal/Downloads/"
file <- h5file(paste0(path,"AIRS.2006.12.31.L3.RetStd001.v6.0.9.0.G13155192744.hdf"), 'r')



install.packages("ncdf4")

install.packages("raster")
install.packages("rgdal")
install.packages("markovchain")
install.packages("ggplot2")
install.packages("rgdal")
library(sp)
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(markovchain)
library(ggplot2) # package for plotting
library(ncdf4)
nc_data <- nc_open("/Users/jyotibhogal/Downloads/SNDR.AQUA.AIRS_IM.20160101T2353.m06.g239.L2_CLIMCAPS_RET.std.v02_39.G.210412151203.SUB.nc4")

 
# Save the print(nc) dump to a text file to get the metadata
{
  sink('AIRS_metadata.txt')
  print(nc_data)
  sink()
}

### 
lon <- ncvar_get(nc_data, "xtrack")
lat <- ncvar_get(nc_data, "atrack", verbose = F)
t <- ncvar_get(nc_data, "obs_time_utc")

head(lon) # look at the first few entries in the longitude vector
head(lat)
head(t)


## Variable of interest is 
ndvi.array <- ncvar_get(nc_data, "air_temp") # store the data in a 3-dimensional array
dim(ndvi.array) 


### 
ndvi.slice <- ndvi.array[, , 1] 

###
dim(ndvi.slice)

####
library(raster)
r <- raster(t(ndvi.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r <- flip(r, direction='y')
plot(r)

####
install.packages("tidyverse")
library(tidyverse)
df.ndvi.slice <- ncvar_get(nc_data, "clim_surf_ir_wnum_cnt")
#ndvi <- data.frame(ndvi.array[[1]])
df.ndvi.slice %>% 
  ggplot(aes(lon,lat))+
  geom_point()+
  labs(title = "air_temp")


head(ndvi.array)




######## R Pubs Example ######
path <- "/Users/jyotibhogal/Downloads/GIMMS3g_NDVI_Trends_1275/data/"
library(ncdf4)
nc_data <- nc_open(paste0(path,'gimms3g_ndvi_1982-2012.nc4'))
# Save the print(nc) dump to a text file
{
  sink(paste0(path,'gimms3g_ndvi_1982-2012_metadata.txt'))
  print(nc_data)
  sink()
}

###
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

head(lon) # look at the first few entries in the longitude vector


###
ndvi.array <- ncvar_get(nc_data, "NDVI") # store the data in a 3-dimensional array
dim(ndvi.array) 

###
fillvalue <- ncatt_get(nc_data, "NDVI", "_FillValue")
fillvalue

nc_close(nc_data) 


###
ndvi.array[ndvi.array == fillvalue$value] <- NA
ndvi.slice <- ndvi.array[, , 1] 
dim(ndvi.slice)


###
r <- raster(t(ndvi.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')

plot(r,main = "NDVI for 1982")

####
writeRaster(r, paste0(path,"GIMMS3g_1982.tif"), "GTiff", overwrite=TRUE)


####
r_brick <- brick(ndvi.array, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

# note that you may have to play around with the transpose (the t() function) and flip() before the data are oriented correctly. In this example, the netcdf file recorded latitude on the X and longitude on the Y, so both a transpose and a flip in the y direction were required.
r_brick <- flip(t(r_brick), direction='y')

##
toolik_lon <- -149.5975
toolik_lat <- 68.6275
toolik_series <- extract(r_brick, SpatialPoints(cbind(toolik_lon,toolik_lat)), method='simple')

ts.plot(t(toolik_series))
## OR
# ALternative to plot the time series:

library(ggplot2)
toolik_df <- data.frame(year= seq(from=1982, to=2012, by=1), NDVI=t(toolik_series))
ggplot(data=toolik_df, aes(x=year, y=NDVI, group=1)) +
  geom_line() + # make this a line plot
  ggtitle("Growing season NDVI at Toolik Lake Station") +     # Set title
  theme_bw() # use the black and white theme


#### 
## (Earlier data was for year 1982) Data for the year 2012:
ndvi.slice.2012 <- ndvi.array[, , 31] 


#ndvi.slice <- ndvi.array[, , 1] 
dim(ndvi.slice.2012)


###
r2012 <- raster(t(ndvi.slice.2012), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r2012 <- flip(r2012, direction='y')

plot(r2012,main = "NDVI for 2012")


ndvi.diff <- ndvi.slice.2012 - ndvi.slice
r_diff <- raster(t(ndvi.diff), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_diff <- flip(r_diff, direction='y')
plot(r_diff,main = "Difference in NDVI between 2012 & 1982")                 

layout(matrix(c(1,2,3,3),2,2,byrow = TRUE))



#### AIRS data for 1 variavle: CO Vartiables: CO_dof_A

path <- "/Users/jyotibhogal/Downloads/"
library(ncdf4)
nc_data <- nc_open(paste0(path,'AIRS.2002.09.01.L3.RetSup_IR030.v6.0.9.0.G13215031528.hdf.nc4'))
# Save the print(nc) dump to a text file
{
  sink(paste0(path,'AIRS.2002.09.01.L3.RetSup_IR030.v6.0.9.0.G13215031528.txt'))
  print(nc_data)
  sink()
}

###
lon <- ncvar_get(nc_data, "Longitude")
lat <- ncvar_get(nc_data, "Latitude", verbose = F)
#t <- ncvar_get(nc_data, "time")

head(lon) # look at the first few entries in the longitude vector


###
CO <- ncvar_get(nc_data, "CO_dof_A") # store the data in a 3-dimensional array
dim(CO) 

###
fillvalue <- ncatt_get(nc_data, "CO_dof_A", "_FillValue")
fillvalue

nc_close(nc_data) 


###
CO[CO == fillvalue$value] <- NA
#ndvi.slice <- ndvi.array[, , 1] 
dim(CO)


###
r <- raster(t(CO), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')
par(mfrow = c(1,1))
plot(r,main = "CO for Sep 2002")

#### MAKING INTERACTIVE PLOTS IN R ####

library(dplyr)
install.packages("leaflet")
library(leaflet)
# Create a basic leaflet map
# Note: following code requires that the data is in raster form
map <- leaflet(data = r) %>%
  addTiles()  # Add a default tile layer

# Display the map
map


# Add markers
map <- map %>%
  addMarkers(map$x,lng = lon,lat = lat)

# Add polygons


map <- map %>%
  addPolygons(fillColor = "red", fillOpacity = 0.5, color = "white", weight = 1)

# Add popups
map <- map %>%
  addPopups(lng = lon, lat = lat, popup = r)


# Get the coordinates of the raster cells
raster_points <- rasterToPoints(r)

# Convert to a SpatialPointsDataFrame
spdf <- SpatialPointsDataFrame(raster_points[, 1:2], data = data.frame(value = raster_points[, 3]))


# Create a leaflet map
map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = spdf, ~x, ~y, color = ~colorNumeric("viridis", value), radius = 5, stroke = FALSE, fillOpacity = 0.8) %>%
  addLegend("bottomright", pal = colorNumeric("viridis", spdf$value),values = spdf$value)

# Display the map
map
###### Making interactive map ####
spdf$value

