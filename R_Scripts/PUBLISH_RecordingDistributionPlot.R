#Author: Ximena Leon

#####################################################################################################
#####################################################################################################

library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(readr)
library(stats)

All_TowheeEcology_Data <- read.csv("~/PUBLISH_FINAL_SongData_wEcologyClimateUrbanizationElevation_6.6.25_XL.csv")

world <- ne_states(returnclass = "sf") # Obtaining map data for plotting
#world <- ne_countries(scale = "medium", returnclass = "sf") # Obtaining map data for plotting

# Define a named vector of hex codes for each species
species_colors <- c(
  "Pipilo maculatus" = "#54b04a",
  "Pipilo erythrophthalmus" = "#487bb6"
)


# Define shapes for each species
species_shapes <- c(
  "Pipilo maculatus" = 16,  # solid circle
  "Pipilo erythrophthalmus" = 17 #triangles
)

# Create the map
RecordingDistribution_Map <- ggplot(All_TowheeEcology_Data, show.legend = FALSE) + 
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, color = Species, shape = Species), 
             alpha = 0.7, size = 1.5, show.legend = FALSE) +
  coord_sf(xlim = c(-140, -55), ylim = c(10, 55), expand = FALSE) +
  scale_shape_manual(values = species_shapes) +
  scale_color_manual(values = species_colors) +
  theme(plot.title = element_text(size = 15)) +
  theme_classic()

RecordingDistribution_Map

# Save PDF as 5x6 landscape
