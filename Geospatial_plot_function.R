# Co2 data for Oct 2023 
install.packages("hdf5r")
library(hdf5r)
library(raster)

path <- "/Users/jyotibhogal/Downloads/"
file <- h5file(paste0(path,"AIRS.2023.10.01.L3.RetStd_IR031.v7.0.7.0.G23307150525.hdf"), 'r')


library(ncdf4)
library(sp)
library(raster) # package for raster manipulation
install.packages("rgdal")
library(rgdal) # package for geospatial analysis
library(markovchain)
library(ggplot2) # package for plotting

nc_data <- nc_open(paste0(path,"AIRS.2023.10.01.L3.RetStd_IR031.v7.0.7.0.G23307150525.hdf"))


h5ls(paste0(path,"AIRS.2023.10.01.L3.RetStd_IR031.v7.0.7.0.G23307150525.hdf"))
                   

#####
install.packages("hdf5")
library(hdf5)
file <- h5file(paste0(path,"AIRS.2023.10.01.L3.RetStd_IR031.v7.0.7.0.G23307150525.hdf"), 'r')
h5ls(file)



#####
install.packages("rhdf5")
library(rhdf5)
install.packages("ncdf4")



#####
#setRepositories()

### Following 2 lines of if() statement are really imp. Due to these two lines, 
# the packages rhdf5 and hdf5r got intalled. Prior to using this if() statemnet, 
# there was error in installation.

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("rhdf5")


library(rhdf5)
install.packages("ncdf4")
library(ncdf4)

install.packages("hdf5r")
library(hdf5r)


file <- h5file(paste0(path,"AIRS.2023.10.01.L3.RetStd_IR031.v7.0.7.0.G23307150525.hdf"), 'r')
h5ls()

#h5read(paste0(path,"AIRS.2023.10.01.L3.RetStd_IR031.v7.0.7.0.G23307150525.hdf"))

#ncl_convert2nc(paste0(path,"AIRS.2023.10.01.L3.RetStd_IR031.v7.0.7.0.G23307150525.hdf"))
file_path <- paste0(path,"AIRS.2023.10.01.L3.RetStd_IR031.v7.0.7.0.G23307150525.hdf")
h5_file <- H5Fopen(file_path)


library(ncdf4)
file_nc <- nc_open(paste0(path,"oco2_LtCO2_230331_B11014Ar_230503203455s.nc4"))



# Save the print(nc) dump to a text file
{
  sink(paste0(path,'oco2_LtCO2_230331_B11014Ar_230503203455s.txt'))
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
plot(r,main = "CO2 - Daily")


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
#### GEOSPATIAL PLOT FUNCTION ####
#### Above code of statics and interactive plot for Co2: VERY IMP Daily data, .nc4 files, ongoing since 1st June 2014: 

library(ncdf4)
geospatial_plot <- function(file_path)
{
nc_data <- nc_open(file_path)
# Save the print(nc) dump to a text file
{
  sink(paste0(path,'oco2_GEOS_L3CO2_day_20220228_B10206Ar.txt'))
  print(nc_data)
  sink()
}

###
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")
#t <- ncvar_get(nc_data, "time")

#head(lon) # look at the first few entries in the longitude vector


###
CO2 <- ncvar_get(nc_data, "XCO2") # store the data in a 3-dimensional array
dim(CO2) 

###
fillvalue <- ncatt_get(nc_data, "XCO2", "_FillValue")
fillvalue

nc_close(nc_data) 

###
CO2[CO2 == fillvalue$value] <- NA
#ndvi.slice <- ndvi.array[, , 1] 
dim(CO2)


###
r <- raster(t(CO2), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')
par(mfrow = c(1,1))
plot(r,main = "CO2")


}

path <- "/Users/jyotibhogal/Downloads/"
file_path <- paste0(path,"oco2_GEOS_L3CO2_day_20220228_B10206Ar.nc4")
geospatial_plot(file_path = file_path)





#### MAKING INTERACTIVE PLOTS IN R ####

library(dplyr)
#install.packages("leaflet")
library(leaflet)
# Create a basic leaflet map
# Note: following code requires that the data is in raster form
map <- leaflet(data = r) %>%
  addTiles()  # Add a default tile layer

# Display the map
map


# Add markers
map <- map %>%
  addMarkers(map$x,lng = lon[1:361],lat = lat)

# Add polygons


map <- map %>%
  addPolygons(fillColor = "red", fillOpacity = 0.5, color = "white", weight = 1)

# Add popups
map <- map %>%
  addPopups(lng = lon[1:361], lat = lat, popup = r)


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

