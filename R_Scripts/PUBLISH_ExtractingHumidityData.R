#Author: Meklit Mesfin with help from Ximena Leon and Nicole Creanza

################################################################################
################################################################################

# Install climate package from GitHub
remotes::install_github("bczernecki/climate")

# Load the library
library(remotes)
library(climate)
library(lubridate)
library(dplyr)

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


songdata <- read.csv("~/ALL_combined_metadata_OLDandNEW_9.19.24.csv") 

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

# newFilter to only recordings with valid coordinates (keep all years)
songdata <- songdata %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  arrange(Date)

# newCreate an empty dataframe to hold accepted entries
accepted_data <- songdata[0, ]

# newCounter for accepted rows
accepted <- 0

# newwwwInitialize columns so they exist across all rows
songdata$nearest_monitor_distance <- NA
songdata$closest_monitor_latitude <- NA
songdata$closest_monitor_longitude <- NA
for (i in 1:5) {
  songdata[[paste0("Monitor_Dist_", i)]] <- NA
  songdata[[paste0("Monitor_Lat_", i)]] <- NA
  songdata[[paste0("Monitor_Lon_", i)]] <- NA
  songdata[[paste0("Avg_Humidity_", i)]] <- NA
}


# Create columns to store data from each of the 5 monitors
#for (entry in 948:nrow(songdata)) {
start_entry <- 1958  # or whatever row you want to start at




for (entry in start_entry:nrow(songdata)) {
  
  lat <- songdata$Latitude[entry]
  lon <- songdata$Longitude[entry]
  date <- songdata$Date[entry]
  
  # Get 5 nearest OGIMET stations
  monitors <- tryCatch(
    nearest_stations_ogimet(
      country = "United+States",
      add_map = FALSE,
      point = c(lon, lat),
      no_of_stations = 5
    ),
    error = function(e) return(NULL)
  )
  
  if (is.null(monitors) || min(monitors$distance) > 250) next
  

  # -- Required: Get humidity from the nearest (Monitor 1) --
  station_ID <- monitors$wmo_id[1]
  humidity_data <- tryCatch(
    meteo_ogimet(date = c(date, date), interval = "daily", coords = FALSE, station = station_ID),
    error = function(e) data.frame()
  )
  
  if (nrow(humidity_data) > 0 && !is.na(humidity_data$HrAvg)) {
    songdata[entry, "Avg_Humidity_1"] <- humidity_data$HrAvg
    
    
    # Store nearest monitor metadata
    songdata$nearest_monitor_distance[entry] <- monitors$distance[1]
    songdata$closest_monitor_latitude[entry] <- monitors$lat[1]
    songdata$closest_monitor_longitude[entry] <- monitors$lon[1]
    
    # Optional: Try to get humidity from monitors 2–5
    if (nrow(monitors) > 1) {
      for (mon in 2:min(5, nrow(monitors))) {
        station_ID_extra <- monitors$wmo_id[mon]
        extra_data <- tryCatch(
          meteo_ogimet(date = c(date, date), interval = "daily", coords = FALSE, station = station_ID_extra),
          error = function(e) data.frame()
        )
        
        if (nrow(extra_data) > 0 && !is.na(extra_data$HrAvg)) {
          songdata[entry, paste0("Avg_Humidity_", mon)] <- extra_data$HrAvg
        }
        songdata[entry, paste0("Monitor_Dist_", mon)] <- monitors$distance[mon]
        songdata[entry, paste0("Monitor_Lat_", mon)] <- monitors$lat[mon]
        songdata[entry, paste0("Monitor_Lon_", mon)] <- monitors$lon[mon]
        Sys.sleep(1)  # Pause to respect OGIMET API
      }
    }
    
    # Append to accepted data
    accepted_data <- bind_rows(accepted_data, songdata[entry, ])
    accepted <- accepted + 1
    cat("✅ Accepted", accepted, "- Recording date:", as.character(date), "\n")
  } else {
    cat("⚠️ Skipped:", entry, "- No humidity from Monitor 1\n")
  }
  
  Sys.sleep(1)
}
