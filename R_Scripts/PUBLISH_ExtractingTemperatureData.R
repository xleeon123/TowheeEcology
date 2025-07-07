#Author: Meklit Mesfin with help from Ximena Leon and Nicole Creanza

################################################################################
################################################################################

# Install climate package from GitHub
remotes::install_github("bczernecki/climate")

# Load the library
library(remotes)
library(climate)

# Find 10 nearest US stations to the given location (Longitude first, then Latitude)
nearest_stations_ogimet(
  country = "United+States",
  date = Sys.Date(),
  add_map = TRUE,
  point = c(-103.6183333, 37.18),
  no_of_stations = 10
)

date <- "2000-04-13"

library(climate)
o = meteo_ogimet(date = c(date, date), 
                 interval = "daily",
                 coords = FALSE, 
                 station = 72202)
head(o)


songdata <- read.csv("~/ALL_combined_metadata_OLDandNEW.csv")

# Convert Date column to the "YYYY-MM-DD" format
#songdata$Date <- as.Date(mdy(songdata$Date))

# Convert Date column to Date format
songdata$Date <- mdy(songdata$Date)  # Convert to date first

# Fix 2-digit year interpretation
songdata$Date <- ifelse(
  year(songdata$Date) > 2025,  # If the year is in the future (e.g., 2050-2099)
  songdata$Date - years(100),  # Subtract 100 years to correct it
  songdata$Date  # Keep valid dates unchanged
)

# Ensure it's still in Date format
songdata$Date <- as.Date(songdata$Date)

start_row <- 2702

# Ensure the monitor metadata columns exist before loop starts
songdata$nearest_monitor_distance <- NA
songdata$closest_monitor_latitude <- NA
songdata$closest_monitor_longitude <- NA

# Create columns to store data from each of the 5 monitors
for (i in 1:5) {
  songdata[[paste0("Temp_", i)]] <- NA
  songdata[[paste0("Temp_min_", i)]] <- NA
  songdata[[paste0("Temp_max_", i)]] <- NA
  songdata[[paste0("Monitor_Dist_", i)]] <- NA
  songdata[[paste0("Monitor_Lat_", i)]] <- NA
  songdata[[paste0("Monitor_Lon_", i)]] <- NA
}


# Loop through each recording
#for (entry in 1:nrow(songdata)) {
for (entry in start_row:nrow(songdata)) {
  lat <- songdata$Latitude[entry]
  lon <- songdata$Longitude[entry]
  date <- songdata$Date[entry]
  
  if (is.na(lat) || is.na(lon)) next
  
  monitors <- nearest_stations_ogimet(
    country = "United+States",
    add_map = FALSE,
    point = c(lon, lat),
    no_of_stations = 5
  )
  
  min_dist <- min(monitors$distance)
  if (min_dist > 250) next  # Skip if nearest is > 250 km #changed 4-9-25
  
  # Record info for the *nearest* monitor
  songdata$nearest_monitor_distance[entry] <- min_dist
  min_index <- which.min(monitors$distance)
  songdata$closest_monitor_latitude[entry] <- monitors$lat[min_index]
  songdata$closest_monitor_longitude[entry] <- monitors$lon[min_index]
  
  # Loop through 5 monitors and store individual values
  for (mon in 1:nrow(monitors)) {
    station_ID <- monitors$wmo_id[mon]
    
    climate_data <- meteo_ogimet(
      date = c(date, date),
      interval = "daily",
      coords = FALSE,
      station = station_ID
    )
    
    # Store temperature and metadata if available
    if (nrow(climate_data) > 0) {
      songdata[entry, paste0("Temp_", mon)] <- climate_data$TemperatureCAvg
      songdata[entry, paste0("Temp_min_", mon)] <- climate_data$TemperatureCMin
      songdata[entry, paste0("Temp_max_", mon)] <- climate_data$TemperatureCMax
    }
    
    # Always store monitor metadata (even if temp is missing)
    songdata[entry, paste0("Monitor_Dist_", mon)] <- monitors$distance[mon]
    songdata[entry, paste0("Monitor_Lat_", mon)] <- monitors$lat[mon]
    songdata[entry, paste0("Monitor_Lon_", mon)] <- monitors$lon[mon]
  }
}
#songdata$min_Temperature[entry] <- climate_data$TminC
#songdata$max_Temperature[entry] <- climate_data$TmaxC
#songdata$Precipitation_total[entry] <- climate_data$Precmm
#songdata$total_Precipitation[entry] <- climate_data$pr24
#songdata$nearest_monitor_distance[entry] <- monitors$distance[min_index]





#   # Filter out monitors without temperature data
#   if (!is.null(monitors$loc) && nrow(monitors$loc$id) > 0) {
#     # Loop through monitors to find the nearest one with temperature data
#     for (j in 1:nrow(monitors$loc)) {
#       closest_monitor_lat <- monitors$loc$latitude[j]
#       closest_monitor_lon <- monitors$loc$longitude[j]
#       distance_to_monitor <- distGeo(c(lon, lat), c(closest_monitor_lon, closest_monitor_lat)) / 1000 # Convert to kilometers
# 
#       # Retrieve weather data from this monitor
#       weather_data <- meteo_pull_monitors(
#         monitors = monitors$loc$id[j],
#         date_min = date,
#         date_max = date
#       )
# 
#       # Check if temperature data exists
#       if (!all(is.na(weather_data$tavg)) || !all(is.na(weather_data$tmin)) || !all(is.na(weather_data$tmax))) {
#         # If valid data found, save to songdata and break the loop
#         songdata$nearest_monitor_distance[entry] <- distance_to_monitor
#         songdata$closest_monitor_latitude[entry] <- closest_monitor_lat
#         songdata$closest_monitor_longitude[entry] <- closest_monitor_lon
#         songdata$Avg_Temperature[entry] <- (weather_data$tavg, na.rm = TRUE)
#         songdata$Avg_Humidity[entry] <- (weather_data$rhav, na.rm = TRUE)
#         songdata$min[entry] <- (weather_data$tmin, na.rm = TRUE)
#         songdata$max[entry] <- (weather_data$tmax, na.rm = TRUE)
#         break
#       }
#     }
#   } else {
#     # If no monitors are found or no valid data, assign NA
#     songdata$Avg_Humidity[i] <- NA
#     songdata$Avg_Temperature[i] <- NA
#   }
# }

# Display the updated data frame
print(head(songdata))
print(lat)
print(climate_data)
print(date)

