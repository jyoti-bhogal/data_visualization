#### GEOSPATIAL PLOT FUNCTION ####

# Installing and loading essential R packages
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("rhdf5")

install.packages("hdf5r")
install.packages("rgdal")
install.packages("markovchain")
install.packages("hdf5")
install.packages("rhdf5")
install.packages("leaflet")
install.packages(c("sf", "rnaturalearth", "rnaturalearthdata"))
install.packages(c("stringr","stringi"))
install.packages("ncdf4")


library(rhdf5)
library(hdf5r)
library(rgdal) # package for geospatial analysis
library(markovchain)
library(ggplot2) # package for plotting
library(hdf5)
library(rhdf5)
library(leaflet)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringi)
library(stringr)
library(ncdf4)
library(sp)
library(raster) # package for raster manipulation

# After installing essential libraries, define the function 'geospatial_plot'

# User defined function 'geospatial_plot' 
# Input: path of the .nc4 or .netCDF file which contains the values of the variable of interest
# Output: 2-D geospatial plot of the variable of interest 

geospatial_plot <- function(file_path)
{
  
  nc_data <- nc_open(file_path)
  # Save the print(nc) dump to a text file which contains the metadata for the .netCDF file 
  {
    sink(paste0(path,'Text',count,'.txt'))
    print(nc_data)
    sink()
  }
  # To extract the date variable from the .netCDF file
  date <-   nc_data$dim$time$units
  # To extract year from date
  d0 <- strsplit(date,split = '-')[[1]]
  d1 <- d0[1]
  d2 <- strsplit(d1,split = ' ')
  d3 <- d2[[1]]
  len_d3 <- length(d3)
  year <- d3[len_d3]
  # To extract month from date
  d4 <- d0[2]
  # To convert the months from month number to month name 
  # Note: the month numbers are mentioned within double quotes because their data 
  # type or class is 'character' rather than 'numeric' 
  month <- ifelse(d4=="01","Jan",ifelse(d4=="02","Feb",ifelse(d4=="03","Mar",
              ifelse(d4=="04","Apr",ifelse(d4=="05","May",ifelse(d4=="06","Jun",
          ifelse(d4=="07","Jul",ifelse(d4=="08","Aug",ifelse(d4=="09","Sep",
                ifelse(d4=="10","Oct",ifelse(d4=="11","Nov","Dec")))))))))))
  
  # To extract the name of the variable of interest
  title <- nc_data$var$XCO2$longname
  

  # To extract the values of latitude, longitude, and time from the .netCDF file
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat")
  t <- ncvar_get(nc_data, "time")
  

  # To extract the values of the variable of interest
  CO2 <- ncvar_get(nc_data, "XCO2")
  
  # To extract what filler value is used for the variable of interest when the
  # actual variable value is unavailable.
  fillvalue <- ncatt_get(nc_data, "XCO2", "_FillValue")
  
  # to close the .netCDF file as it is no more required
  nc_close(nc_data) 
  
  # To replace the filler values by 'NA' 
  CO2[CO2 == fillvalue$value] <- NA
  
  # To convert the variable of interest into a raster object. This raster object 
  # 'r' consists the values of the variable of interest for each combination of 
  # the latitude and longitude values
  r <- raster(t(CO2), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
    crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  # To orient the rater object properly
  r <- flip(r, direction='y')
  # To define a color palette for the geospatial plot of the variable of interest 
  pal <- colorRampPalette(c("green4","green3","yellow","orange","red"))
  
  plot(r,ylab = "Latitude", xlab = "Longitude", main = paste(title,"for",'\n',month,year),col = pal(50))
  # To store the international boundaries into a object of class 'sf', which is 'simple feature'
  world <- ne_countries(scale = "medium", returnclass = "sf")
  # To add the international boundaries on the previous plot
  plot(st_geometry(world), add = TRUE, border = "black")
  
}

# To run the above function 'geospatial_plot()' for the datasets available from the URL for NASA Earth Observation Data:
# https://search.earthdata.nasa.gov/search/granules?p=C2240248762-GES_DISC!C2248652664-GES_DISC&pg%5B1%5D%5Bv%5D=t&pg%5B1%5D%5Bgsk%5D=-start_date&pg%5B1%5D%5Bm%5D=download&q=v10&fi=OCO-2&fl=3%2B-%2BGridded%2BObservations!2%2B-%2BGeophys.%2BVariables%252C%2BSensor%2BCoordinates&gdf=NetCDF&tl=1701769251!3!!&long=-0.3125&zoom=0

# To read CSV file which contains the names of the 2616 data files downloaded 
# from the above URL.
path <- "/Users/jyotibhogal/Desktop/CO2_Datasets_Full/"
file_name_csv <- read.csv(paste0(path,"File Names_2015_to_2022 - Sheet1.csv"))
file_name <- file_name_csv$File.Name
len_file_name <- length(file_name)

Sys.time()
Sys.sleep(5)
# To run the function 'geospatial_plot()' for all the datasets downloaded from the above URL.
for(count in 1:len_file_name)
{
  file_name_count <- file_name[count]
  file_path <-paste0(path,file_name_count)
  #if (!file.exists(file_path)) {
  # stop("File not found at the specified path.")
  #}
  geospatial_plot(file_path = file_path)
  
}
Sys.time()

# End of code
