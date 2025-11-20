#Author: Ximena Leon

################################################################################
################# Is there evidence for geo dist affecting song ################
###################### differences within each ecoregion? ######################
################################################################################
install.packages('geodist')
library(geodist)
library(dplyr)
library(vegan)

# Load and preprocess towhee dataframe
TowheeData <- read.csv("~/PUBLISH_ALL_FINAL_TowheeResultsData_CLEANED.csv")

# Filter to one species #repeat for Eastern towhee
TowheeDataSpecies <- TowheeData[TowheeData$Species == "Pipilo maculatus", ]

TowheeDataSpecies <- TowheeDataSpecies %>%
  group_by(Level1Ecoregion_Edited) %>%
  filter(n() >= 10) %>%
  ungroup()

# Log transform and Z-score (safer than log if zeros present)
TowheeDataSpecies[, 7:14] <- scale(log(TowheeDataSpecies[, 7:14]))

# Song features and unique ecoregions
song_feature_cols <- 7:14
ecoregions <- unique(na.omit(TowheeDataSpecies$Level1Ecoregion_Edited))

# Initialize results dataframe
within_results <- data.frame()

# Loop through each song feature
for (feature_col in song_feature_cols) {
  feature_name <- names(TowheeDataSpecies)[feature_col]
  
  for (eco in ecoregions) {
    # Subset data to this ecoregion
    eco_data <- TowheeDataSpecies %>%
      filter(Level1Ecoregion_Edited == eco)
    
    if (nrow(eco_data) < 3) next
    
    # Geographic distance matrix
    coords <- eco_data[, c("Longitude", "Latitude")]
    geo_dist <- as.dist(geodist(coords, measure = "geodesic"))
    
    # Song feature distance matrix
    feature_dist <- dist(eco_data[[feature_name]], method = "euclidean")
    
    # Mantel test
    mantel_result <- mantel(geo_dist, feature_dist, method = "pearson", permutations = 999)
    
    # Store results
    within_results <- rbind(within_results, data.frame(
      Ecoregion = eco,
      Feature = feature_name,
      Mantel_r = mantel_result$statistic,
      P_value = mantel_result$signif,
      n = nrow(eco_data)
    ))
  }
}

# Apply FDR correction to p-values
within_results$P_value_FDR <- p.adjust(within_results$P_value, method = "fdr")

# View significant results after FDR correction
significant_results <- within_results %>%
  filter(P_value_FDR < 0.05) %>%
  arrange(P_value_FDR)

# Output results
print(within_results)
#write.csv(within_results, "~/GeoDist_WithinEcoregion_Spotted.csv", row.names = FALSE)
#repeat for Eastern towhee


################################################################################
################################################################################
################################### PLOTTING ###################################
library(dplyr)
library(ggplot2)
library(patchwork)

# "Pipilo erythrophthalmus" = "#487bb6",
# "Pipilo maculatus" = "#54b04a"

################################################################################
# ------------------------------Eastern Towhee-------------------------------- #
################################################################################

GeoDist_Ecoregion_Data <- read.csv("~/GeoDist_WithinEcoregion_Eastern.csv")

# Compute absolute value of Mantel R and mark significant cells
df_E <- GeoDist_Ecoregion_Data %>%
  mutate(
    Significant = ifelse(P_value_FDR < 0.05, "*", "")
  )


# Plot
ggplot(df_E, aes(x = Feature, y = Ecoregion, fill = Mantel_r)) +
  geom_tile(color = "white") +  # box borders
  geom_text(aes(label = Significant), color = "black", size = 8) +  # stars
  scale_fill_gradient2(
    low = "darkgrey", 
    mid = "#f5f5f5",
    high = "#487bb6",  # you can reverse or customize
    midpoint = 0,
    name = "Mantel r",
    limits = c(-1,1)
  ) +
  theme_minimal(base_size = 14) +
  coord_equal() +
  labs(x = "Song Feature", y = "Ecoregion",
       title = "Song Feature Significance by Ecoregion") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

#save PDF as 10x12 landscape


################################################################################
# ------------------------------Spotted Towhee-------------------------------- #
################################################################################

GeoDist_Ecoregion_Data <- read.csv("~/GeoDist_WithinEcoregion_Spotted.csv")

# Compute absolute value of Mantel R and mark significant cells
df_S <- GeoDist_Ecoregion_Data %>%
  mutate(
    Significant = ifelse(P_value_FDR < 0.05, "*", "")
  )



# Plot
ggplot(df_S, aes(x = Feature, y = Ecoregion, fill = Mantel_r)) +
  geom_tile(color = "white") +  # box borders
  geom_text(aes(label = Significant), color = "black", size = 8) +  # stars
  scale_fill_gradient2(
    low = "darkgrey", 
    mid = "#f5f5f5",
    high = "#54b04a",  # you can reverse or customize
    midpoint = 0,
    name = "Mantel r",
    limits = c(-1,1)
  ) +
  theme_minimal(base_size = 14) +
  coord_equal() +
  labs(x = "Song Feature", y = "Ecoregion",
       title = "Song Feature Significance by Ecoregion") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

#save PDF as 10x12 landscape