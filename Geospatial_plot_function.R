#### GEOSPATIAL PLOT FUNCTION ####

# Installing and loading essential R packages
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("rhdf5")

library(rhdf5)
install.packages("hdf5r")
library(hdf5r)
install.packages("rgdal")
library(rgdal) # package for geospatial analysis
install.packages("markovchain")
library(markovchain)
library(ggplot2) # package for plotting
install.packages("hdf5")
library(hdf5)
install.packages("rhdf5")
library(rhdf5)
install.packages("leaflet")
library(leaflet)
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

# User defined function 'geospatial_plot' 
# Input: path of th e.nc4 or .netCDF file which contains the values of the variable of interest
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
  # To orient the rater object properly
  r <- flip(r, direction='y')
  # To define a color palette for the geospatial plot of the variable of interest 
  pal <- colorRampPalette(c("green4","green3","yellow","orange","red"))
  
  #plot(r,ylab = "Latitude", xlab = "Longitude", main = paste(title,"for",'\n',month,year),col = pal(50))
  # To store the international boundaries into a object of class 'sf', which is 'simple feature'
  world <- ne_countries(scale = "medium", returnclass = "sf")
  # To add the international boundaries on the previous plot
  return(list(mean_CO2_global,mean_CO2_north,mean_CO2_south))
  #plot(st_geometry(world), add = TRUE, border = "black")
  
}

# To run the above function 'geospatial_plot()' for the datasets available from the URL for NASA Earth Observation Data:
# https://search.earthdata.nasa.gov/search/granules?p=C2240248762-GES_DISC!C2248652664-GES_DISC&pg%5B1%5D%5Bv%5D=t&pg%5B1%5D%5Bgsk%5D=-start_date&pg%5B1%5D%5Bm%5D=download&q=v10&fi=OCO-2&fl=3%2B-%2BGridded%2BObservations!2%2B-%2BGeophys.%2BVariables%252C%2BSensor%2BCoordinates&gdf=NetCDF&tl=1701769251!3!!&long=-0.3125&zoom=0

# To read CSV file which contains the names of the 2616 data files downloaded 
# from the above URL.
path <- "/Users/jyotibhogal/Desktop/CO2_Datasets_Full/"
file_name_csv <- read.csv(paste0(path,"File Names_2015_to_2022 - Sheet1.csv"))
file_name <- file_name_csv$File.Name
len_file_name <- length(file_name)

#Sys.time()
#Sys.sleep(5)
# To run the function 'geospatial_plot()' for all the datasets downloaded from the above URL.
global_mean_vector <- north_mean_vector <- south_mean_vector <- rep(NA,len_file_name)


for(count in 1:len_file_name)
{
  file_name_count <- file_name[count]
  file_path <-paste0(path,file_name_count)
  #if (!file.exists(file_path)) {
  # stop("File not found at the specified path.")
  #}
  global_mean_vector[count] <- geospatial_plot(file_path = file_path)[[1]]
  north_mean_vector[count] <- geospatial_plot(file_path = file_path)[[2]]
  south_mean_vector[count] <- geospatial_plot(file_path = file_path)[[3]]
  
}
summary(global_mean_vector)
summary(north_mean_vector)
summary(south_mean_vector)
global_mean_vector <- ts(global_mean_vector, start = c(2015,1,1), frequency = 365)
north_mean_vector <- ts(north_mean_vector, start = c(2015,1,1), frequency = 365)
south_mean_vector <- ts(south_mean_vector, start = c(2015,1,1), frequency = 365)

#ts.plot(mean_vector,main= "Global Average Assimilated CO2", col = "green",lwd = 3) # Simple plot. Hence won't use it further.
Sys.time()
write.csv(global_mean_vector,file = paste0(path,"/Global_Average_CO2_Assimilated.csv"))
write.csv(north_mean_vector,file = paste0(path,"/Global_North_Average_CO2_Assimilated.csv"))
write.csv(south_mean_vector,file = paste0(path,"/Global_South_Average_CO2_Assimilated.csv"))

install.packages("plotly")
library(plotly)
#path <- "C:\\Users\\jsatnamsinghbhoga\\Downloads\\"

input_file_name_global <- "Global_Average_CO2_Assimilated.csv"
input_file_name_north <- "Global_North_Average_CO2_Assimilated.csv"
input_file_name_south <- "Global_South_Average_CO2_Assimilated.csv"
data_global <- read.csv(paste0(path,input_file_name_global), header = TRUE)
data_north <- read.csv(paste0(path,input_file_name_north), header = TRUE)
data_south <- read.csv(paste0(path,input_file_name_south), header = TRUE)


###### Skip from here

# #View(data)
# #summary(data)
# #ts.plot(data$x)
# #data_ts <- ts(data$x,start = c(2015,1,1),frequency = 365)
#
# #ts.plot(data_ts,col = "green",lwd = 3)
# #####
# # create interactive plotly plot
# #ggplotly(data$x)
#### Skip till here
#####
# interactive time series
install.packages("dygraphs")
library(dygraphs)
# create time series objects (class xs)
install.packages("xts")
library(xts)

# create time series object
data_global$date_time <- data_north$date_time <- data_south$date_time <- seq(as.Date("2015-01-01"),as.Date("2022-02-28"),by="1 day")
timeSeries_global <- xts(x = data_global$x, order.by = data_global$date_time)
colnames(timeSeries_global) <- "Global"
timeSeries_north <- xts(x = data_north$x, order.by = data_global$date_time)
colnames(timeSeries_north) <- "Global North"

timeSeries_south <- xts(x = data_south$x, order.by = data_global$date_time)
colnames(timeSeries_south) <- "Global South"

# Combine the global series with the North series
timeSeries_global <- cbind(timeSeries_global, North = timeSeries_north$`Global North`)

# Combine the global series with the South series
timeSeries_global <- cbind(timeSeries_global, South = timeSeries_south$`Global South`)

plot(timeSeries)
acf(timeSeries,lag.max = 365*7,main = "ACF")
pacf(timeSeries,lag.max = 365*2,main="PACF")
# Note: PACF is insignificant for all lags of 1 onwards. Therefore, there is no AR component.


# time series has seasonality, as well as trend. therefore it is not stationary. 
diff_timeSeries <- diff(timeSeries,lag = 366/2,differences = 2) 
plot(diff_timeSeries,col="green")
length(diff_timeSeries)
diff_timeSeries[c(1:3,2614:2616)]
acf(na.omit(diff_timeSeries),lag.max = 365*7,main = "ACF")
pacf(na.omit(diff_timeSeries),lag.max = 365*2,main="PACF")


log_diff_timeSeries <- diff(log(timeSeries),lag = 366)
plot(log_diff_timeSeries,col="blue")
length(log_diff_timeSeries)
log_diff_timeSeries[c(1:3,2614:2616)]
acf(na.omit(log_diff_timeSeries),lag.max = 365*7,main = "ACF")
pacf(na.omit(log_diff_timeSeries),lag.max = 365*2,main="PACF")


fit2 <- auto.arima(log_diff_timeSeries)
fit2  
f2<-forecast(fit2, level=c(95), h=8*365)
plot(f2)


#### Predictive Analysis of time series
library(forecast)
fit <- auto.arima(timeSeries,seasonal = TRUE)
fit



#pred <- predict(fit,n.ahead = 365*8)
#pred_values <- pred$pred
#pred_se <- pred$se
#UCL <- pred_values - 1.96*pred_se
#LCL <- pred_values + 1.96*pred_se

#ts.plot(pred_values)
#lines(UCL, col = "green")
#lines(LCL,col="blue")


f1<-forecast(fit1, level=c(95), h=8*365)
plot(f1)

d<-decompose(timeSeries)
plot(d)

### Commented out because this time series does not have the Time Range Selector at the bottom.
# # create a basic interactive element
# interact_time <- dygraph(timeSeries)
# interact_time
# library(forecast)
# fit_global <- auto.arima(timeSeries_global$Global)
# forecast_global <- forecast(fit_global,level = 95, h = 8*365)
# plot(forecast_global)

### 15 Jan 2024
library(tidyverse)
# create a basic interactive element
luminous_palette <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12")


interact_time2 <- dygraph(timeSeries_global,main = "Global Average CO2 Assimilated") %>%
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

  
  interact_time2
# 
#   #### 3 D interactive plot using plotly ####
#   library(tidyverse)
#   library(plotly)
#   
#   # Assuming 'timeSeries_global' is an xts object with a time index
#   # If it's a data frame, convert it to an xts object first
#   
#   # Create a color palette
#   luminous_palette <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12")
#   
#   # Create a plotly plot
#   plot_ly() %>%
#     add_trace(
#       type = "scatter3d",
#       mode = "lines",
#       x = index(timeSeries_global),
#       y = rep("Global", nrow(timeSeries_global)),
#       z = timeSeries_global$Global,
#       name = "Global",
#       line = list(color = luminous_palette[2], width = 2)
#     )
#   
#   plot_ly() %>%
#     add_trace(
#       type = "scatter3d",
#       mode = "lines",
#       x = index(timeSeries_global),
#       y = rep("Global North", nrow(timeSeries_global)),
#       z = timeSeries_global$Global.North,
#       name = "Global North",
#       line = list(color = luminous_palette[1], width = 2)
#     )
#   
#   plot_ly() %>%
#     add_trace(
#       type = "scatter3d",
#       mode = "lines",
#       x = index(timeSeries_global),
#       y = rep("Global South", nrow(timeSeries_global)),
#       z = timeSeries_global$Global.South,
#       name = "Global South",
#       line = list(color = luminous_palette[3], width = 2)
#     ) %>%
#     layout(
#       scene = list(
#         xaxis = list(title = "Time"),
#         yaxis = list(title = "Series"),
#         zaxis = list(title = "CO2 Levels")
#       ),
#       legend = list(orientation = "h")
#     )
#   
#   
#   
#   
#   library(tidyverse)
#   library(plotly)
#   
#   # Generate some random data
#   set.seed(123)
#   time_series_data <- data.frame(
#     Time = seq(as.POSIXct("2022-01-01"), by = "days", length.out = 100),
#     Global = rnorm(100),
#     Global_North = rnorm(100),
#     Global_South = rnorm(100)
#   )
#   
#   # Convert to xts
#   timeSeries_global <- xts(x = time_series_data[, -1], order.by = time_series_data$Time)
#   
#   # Create a color palette
#   luminous_palette <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12")
#   
#   # Create a plotly plot
#   plot_ly() %>%
#     add_trace(
#       type = "scatter3d",
#       mode = "lines",
#       x = index(timeSeries_global),
#       y = rep("Global", nrow(timeSeries_global)),
#       z = timeSeries_global$Global,
#       name = "Global",
#       line = list(color = luminous_palette[2], width = 2)
#     ) %>%
#     add_trace(
#       type = "scatter3d",
#       mode = "lines",
#       x = index(timeSeries_global),
#       y = rep("Global North", nrow(timeSeries_global)),
#       z = timeSeries_global$Global_North,
#       name = "Global North",
#       line = list(color = luminous_palette[1], width = 2)
#     ) %>%
#     add_trace(
#       type = "scatter3d",
#       mode = "lines",
#       x = index(timeSeries_global),
#       y = rep("Global South", nrow(timeSeries_global)),
#       z = timeSeries_global$Global_South,
#       name = "Global South",
#       line = list(color = luminous_palette[3], width = 2)
#     ) %>%
#     layout(
#       scene = list(
#         xaxis = list(title = "Time"),
#         yaxis = list(title = "Series"),
#         zaxis = list(title = "CO2 Levels")
#       ),
#       legend = list(orientation = "h")
#     )
#   
  ######### end of 3D interactive plot using plotly ####
  
 


##### Making Interactive Maps ##### 
# Raster with Color Brewer schema

#View(r)
#class(r)
leaflet() %>% addTiles() %>% 
  addRasterImage(, colors = "Spectral")




#########
# Install and load required libraries

# Sample data (replace this with your raster data)
# Create a sample raster with random values
set.seed(42)
elevation_projected <- raster(matrix(runif(100, min = 0, max = 1), ncol = 10))
# Set a dummy CRS (replace with your actual CRS)
projection(elevation_projected) <- CRS("+proj=longlat +datum=WGS84")

# Define color breaks and colors for the raster
color_breaks <- c(0, 0.25, 0.5, 0.75, 1)
colors <- c("forestgreen", "yellow", "tan", "brown")

# Create a color palette
pal <- colorQuantile(palette = colors, domain = values(elevation_projected), n = length(colors))

# Create the leaflet map
leaflet() %>% 
  addTiles() %>% 
  addRasterImage(elevation_projected, 
                 colors = ~pal(value),  # Use the palette
                 opacity = 0.7) %>%
  addLegend(pal = pal, 
            values = ~values(elevation_projected),  # Get raster values
            opacity = 0.7,
            title = "Elevation") %>%
  addMarkers(lng = 0, lat = 0, popup = "Center")  # Example marker (replace with your actual raster points)

####


#########
# Load required libraries
#library(leaflet)

# Install and load required libraries
install.packages("leaflet", dependencies = TRUE)
install.packages("raster", dependencies = TRUE)

library(leaflet)
library(raster)

# Sample data (replace this with your raster data)
# Create a sample raster with random values
set.seed(42)
elevation_projected <- raster(matrix(runif(100, min = 0, max = 1), ncol = 10))

# Define color breaks and colors for the raster
color_breaks <- c(0, 0.25, 0.5, 0.75, 1)
colors <- c("forestgreen", "yellow", "tan", "brown")

# Create the leaflet map
leaflet() %>% 
  addTiles() %>% 
  addRasterImage(elevation_projected,
                 colors = colors,
                 breaks = color_breaks,
                 opacity = 0.7) %>%
  addLegend(pal = colorNumeric(palette = colors, domain = color_breaks),
            values = color_breaks,
            opacity = 0.7,
            title = "Elevation") %>%
  addMarkers(lng = 0, lat = 0, popup = "Center")  # Example marker (replace with your actual raster points)

####
# Install and load required libraries
install.packages("leaflet")
install.packages("raster")
library(leaflet)
library(raster)

# Sample data (replace this with your raster data)
# Create a sample raster with random values
set.seed(42)
elevation_projected <- raster(matrix(runif(100, min = 0, max = 1), ncol = 10))

# Define color breaks and colors for the raster
color_breaks <- c(0, 0.25, 0.5, 0.75, 1)
colors <- c("forestgreen", "yellow", "tan", "brown")

# Create a color palette
pal <- colorQuantile(palette = colors, domain = elevation_projected, n = length(colors))

# Create the leaflet map
leaflet() %>% 
  addTiles() %>% 
  addRasterImage(elevation_projected, 
                 colors = ~pal(value),  # Use the palette
                 opacity = 0.7) %>%
  addLegend(pal = pal, 
            values = ~getValues(elevation_projected),  # Get raster values
            opacity = 0.7,
            title = "Elevation") %>%
  addMarkers(lng = 0, lat = 0, popup = "Center")  # Example marker (replace with your actual raster points)



########
# Install and load required libraries
install.packages("leaflet")
install.packages("raster")
library(leaflet)
library(raster)

# Sample data (replace this with your raster data)
# Create a sample raster with random values
set.seed(42)
elevation_projected <- raster(matrix(runif(100, min = 0, max = 1), ncol = 10))
# Set a dummy CRS (replace with your actual CRS)
projection(elevation_projected) <- CRS("+proj=longlat +datum=WGS84")

# Define color breaks and colors for the raster
color_breaks <- c(0, 0.25, 0.5, 0.75, 1)
colors <- c("forestgreen", "yellow", "tan", "brown")

# Create a color palette
pal <- colorQuantile(palette = colors, domain = values(elevation_projected), n = length(colors))

# Create the leaflet map
leaflet() %>% 
  addTiles() %>% 
  addRasterImage(elevation_projected, 
                 colors = ~pal(value),  # Use the palette
                 opacity = 0.7) %>%
  addLegend(pal = pal, 
            values = ~values(elevation_projected),  # Get raster values
            opacity = 0.7,
            title = "Elevation") %>%
  addMarkers(lng = 0, lat = 0, popup = "Center")  # Example marker (replace with your actual raster points)




############## Using Plotly

# Create a sample raster with random values
set.seed(42)
elevation_projected <- raster(matrix(runif(100, min = 0, max = 1), ncol = 10))

# Extract raster values and coordinates
raster_values <- values(elevation_projected)
raster_coords <- xyFromCell(elevation_projected, 1:ncell(elevation_projected))

# Create a data frame with raster values and coordinates
map_data <- data.frame(lon = raster_coords[, 1],
                       lat = raster_coords[, 2],
                       value = raster_values)

# Create an interactive map with plot_ly
map <- plot_ly(data = map_data, type = "heatmap", 
               z = ~value, colors = "Viridis", 
               text = ~paste("Value: ", round(value, 2)),
               hoverinfo = "text",
               showscale = FALSE) %>%
  add_trace(type = "scattermapbox", mode = "markers", 
            lon = ~lon, lat = ~lat, 
            marker = list(size = 1, opacity = 0)) %>%
  layout(mapbox = list(style = "open-street-map", zoom = 10),
         geo = list(lonaxis = list(range = c(min(map_data$lon), max(map_data$lon))),
                    lataxis = list(range = c(min(map_data$lat), max(map_data$lat)))))

# Show the map
map




