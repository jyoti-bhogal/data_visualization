#### Interactive Time Series Plots ####

## Setting up a CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

##  Installing and loading essential R packages
install.packages("markovchain")
library(markovchain)
install.packages("ggplot2")
library(ggplot2) # package for plotting
install.packages("dplyr")
library(dplyr)
install.packages(c("sf", "rnaturalearth", "rnaturalearthdata"))
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
install.packages(c("stringr","stringi"))
library(stringi)
library(stringr)
install.packages("ncdf4")
library(ncdf4)
library(sp)
library(raster) # package for raster manipulation
install.packages("dygraphs") # to create interactive time series
library(dygraphs)
install.packages("xts") # create time series objects (class xs)
library(xts)
install.packages("tidyverse")
library(tidyverse)

## User defined function 'geospatial_plot' 

# Input: path of th e.nc4 or .netCDF file which contains the values of the variable of interest
# Output: 
### 1) Global average, global north average, and global south average of daily assimilated CO2 values
### 2) 2-D geospatial plot of the daily mean CO2 assimilated

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
  date <- nc_data$dim$time$units
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
  
  mean_CO2_global <- mean(CO2)
  n_col_CO2 <- ncol(CO2)
  lat_0 <- (n_col_CO2+1)/2
  mean_CO2_south <- mean(CO2[,1:lat_0])
  mean_CO2_north <- mean(CO2[,(lat_0+1):n_col_CO2])
  # To convert the variable of interest into a raster object. This raster object 
  # 'r' consists the values of the variable of interest for each combination of 
  # the latitude and longitude values
  r <- raster(t(CO2), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  # To orient the raster object properly
  r <- flip(r, direction='y')
  # To define a color palette for the geospatial plot of the variable of interest 
  pal <- colorRampPalette(c("green4","green3","yellow","orange","red"))
  
  ## Please uncomment the following chunk to see the world maps for the daily 
  ## values of assimalted CO2 for each region across the globe.
  # plot(r,ylab = "Latitude", xlab = "Longitude", main = paste(title,"for",'\n',month,year),col = pal(50))
  # To store the international boundaries into a object of class 'sf', which is 'simple feature'
  #world <- ne_countries(scale = "medium", returnclass = "sf")
  # To add the international boundaries on the previous plot
  #plot(st_geometry(world), add = TRUE, border = "black")
  return(list(mean_CO2_global,mean_CO2_north,mean_CO2_south))
}

# To run the above function 'geospatial_plot()' for the datasets available from the URL for NASA Earth Observation Data:
# https://search.earthdata.nasa.gov/search/granules?p=C2240248762-GES_DISC!C2248652664-GES_DISC&pg%5B1%5D%5Bv%5D=t&pg%5B1%5D%5Bgsk%5D=-start_date&pg%5B1%5D%5Bm%5D=download&q=v10&fi=OCO-2&fl=3%2B-%2BGridded%2BObservations!2%2B-%2BGeophys.%2BVariables%252C%2BSensor%2BCoordinates&gdf=NetCDF&tl=1701769251!3!!&long=-0.3125&zoom=0

# To read CSV file which contains the names of the 2616 data files downloaded 
# from the above URL. This file can be downloaded from the Github folder containing this R script.
path <- "/Users/jyotibhogal/Desktop/CO2_Datasets_Full/"
file_name_csv <- read.csv(paste0(path,"File Names_2015_to_2022 - Sheet1.csv"))
file_name <- file_name_csv$File.Name
len_file_name <- length(file_name)

# To run the function 'geospatial_plot()' for all the datasets downloaded from the above URL.
global_mean_vector <- north_mean_vector <- south_mean_vector <- rep(NA,len_file_name)

for(count in 1:len_file_name)
{
  file_name_count <- file_name[count]
  file_path <-paste0(path,file_name_count)
  global_mean_vector[count] <- geospatial_plot(file_path = file_path)[[1]]
  north_mean_vector[count] <- geospatial_plot(file_path = file_path)[[2]]
  south_mean_vector[count] <- geospatial_plot(file_path = file_path)[[3]]
}
# To look at the summary statistics for the three time series
summary(global_mean_vector)
summary(north_mean_vector)
summary(south_mean_vector)
# To convert the average vector into a time series object
global_mean_vector <- ts(global_mean_vector, start = c(2015,1,1), frequency = 365)
north_mean_vector <- ts(north_mean_vector, start = c(2015,1,1), frequency = 365)
south_mean_vector <- ts(south_mean_vector, start = c(2015,1,1), frequency = 365)

# Writing the 3 time series, so that they can be used for any further analysis
write.csv(global_mean_vector,file = paste0(path,"/Global_Average_CO2_Assimilated.csv"))
write.csv(north_mean_vector,file = paste0(path,"/Global_North_Average_CO2_Assimilated.csv"))
write.csv(south_mean_vector,file = paste0(path,"/Global_South_Average_CO2_Assimilated.csv"))


input_file_name_global <- "Global_Average_CO2_Assimilated.csv"
input_file_name_north <- "Global_North_Average_CO2_Assimilated.csv"
input_file_name_south <- "Global_South_Average_CO2_Assimilated.csv"
data_global <- read.csv(paste0(path,input_file_name_global), header = TRUE)
data_north <- read.csv(paste0(path,input_file_name_north), header = TRUE)
data_south <- read.csv(paste0(path,input_file_name_south), header = TRUE)


##### Interactive Time Series Plotting ####

# create time series object
data_global$date_time <- data_north$date_time <- data_south$date_time <- seq(as.Date("2015-01-01"),as.Date("2022-02-28"),by="1 day")
timeSeries_global <- xts(x = data_global$x, order.by = data_global$date_time)
colnames(timeSeries_global) <- "Global"
timeSeries_north <- xts(x = data_north$x, order.by = data_global$date_time)
colnames(timeSeries_north) <- "Global North"

timeSeries_south <- xts(x = data_south$x, order.by = data_global$date_time)
colnames(timeSeries_south) <- "Global South"

# Combine the global north series with the global series
timeSeries_global <- cbind(timeSeries_global, North = timeSeries_north$`Global North`)

# Combine the global South series with the global series
timeSeries_global <- cbind(timeSeries_global, South = timeSeries_south$`Global South`)



# to create a interactive time series plot
luminous_palette <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12")

interact_time <- dygraph(timeSeries_global,main = "Global Average CO2 Assimilated") %>%
  dyRangeSelector() %>%
  dyAxis("x",label = "Time")%>%
  dyAxis("y", label = "CO2 Levels") %>%
  dyAxis("y2", label = "CO2 Levels")%>%
  dyLegend(show = "always", hideOnMouseOut = TRUE )%>%
  dySeries("Global", label = "Global CO2 Levels", color = luminous_palette[2], strokeWidth = 2) %>%
  dySeries("Global.North", label = "Global North CO2 Series", color = luminous_palette[1], strokeWidth = 2) %>%
  dySeries("Global.South", label = "Global South CO2 Series", color = luminous_palette[3], strokeWidth = 2) %>%
  dyOptions(gridLineColor = "lightblue", gridLineWidth = 1,
           drawGrid = TRUE, fillGraph = TRUE, fillAlpha = 0.15)%>%
  dyHighlight(highlightCircleSize = 7, highlightSeriesBackgroundAlpha = 3, 
              hideOnMouseOut = TRUE)

  
interact_time


# In this time series plot, please move your cursor to look at the different (x,y) 
# coordinates which represent the date and the global averages of the daily 
# assimilated CO2 values. At the bottom of the plot, one can see a time range 
# selector. This can be used to zoom in/zoom out to look at the values of CO2 
# for a specific time period.
