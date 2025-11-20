#Author: Ximena Leon

################################################################################
################ Is there a stronger geographic pattern within #################
################## ecoregions than we would expect given the ###################
################### distribution of the species as a whole? ####################
################################################################################
library(geodist)
library(vegan)
library(dplyr)

# Load and preprocess
TowheeData <- read.csv("~/PUBLISH_ALL_FINAL_TowheeResultsData_CLEANED.csv")

# Filter to one species #repeat for Spotted towhee
TowheeDataSpecies <- TowheeData[TowheeData$Species == "Pipilo erythrophthalmus", ]

TowheeDataSpecies <- TowheeDataSpecies %>%
  group_by(Level1Ecoregion_Edited) %>%
  filter(n() >= 10) %>%
  ungroup()

# Transform features (log, then z-score)
TowheeDataSpecies[, 7:14] <- scale(log(TowheeDataSpecies[, 7:14]))

# Set song feature columns and ecoregions
song_feature_cols <- 7:14
ecoregions <- unique(na.omit(TowheeDataSpecies$Level1Ecoregion_Edited))

# Set permutations
n_perm <- 999

# Initialize results
IBD_vs_Null_results <- data.frame()

# Loop over each feature
for (feature_col in song_feature_cols) {
  feature_name <- names(TowheeDataSpecies)[feature_col]
  
  for (eco in ecoregions) {
    # Subset data for this ecoregion
    eco_data <- TowheeDataSpecies %>%
      filter(Level1Ecoregion_Edited == eco)
    
    n_eco <- nrow(eco_data)
    if (n_eco < 4) next  # too few samples
    
    # Observed distance matrices
    geo_dist_obs <- as.dist(geodist(eco_data[, c("Longitude", "Latitude")], measure = "geodesic"))
    feature_dist_obs <- dist(eco_data[[feature_name]])
    
    # Mantel test: observed r
    mantel_obs <- vegan::mantel(geo_dist_obs, feature_dist_obs, method = "pearson", permutations = 0)
    r_obs <- mantel_obs$statistic
    
    # Build null distribution
    r_null <- numeric(n_perm)
    for (i in 1:n_perm) {
      rand_sample <- TowheeDataSpecies[sample(nrow(TowheeDataSpecies), n_eco), ]
      geo_rand <- as.dist(geodist(rand_sample[, c("Longitude", "Latitude")], measure = "geodesic"))
      feature_rand <- dist(rand_sample[[feature_name]])
      mantel_null <- vegan::mantel(geo_rand, feature_rand, method = "pearson", permutations = 0)
      r_null[i] <- mantel_null$statistic
    }
    
    # P-value: proportion of null r ≥ observed r
    p_val <- mean(r_null >= r_obs)  #proportion of true vs false 
    
    # Save result
    IBD_vs_Null_results <- rbind(IBD_vs_Null_results, data.frame(
      Ecoregion = eco,
      Feature = feature_name,
      SampleSize = n_eco,
      Mantel_r_obs = r_obs,
      Mean_r_null = mean(r_null),
      SD_r_null = sd(r_null),
      P_value = p_val
    ))
  }
}

# FDR correction
IBD_vs_Null_results$P_value_adj <- p.adjust(IBD_vs_Null_results$P_value, method = "fdr")

# View significant results after FDR correction
significant_results <- IBD_vs_Null_results %>%
  filter(P_value_adj < 0.05) %>%
  arrange(P_value_adj)

# View results
print(IBD_vs_Null_results)
#write.csv(IBD_vs_Null_results, "~/GeoDist_WithinEcoregion_vs_Null_MantelTest_Eastern.csv", row.names = FALSE)
#repeat for Spotted towhee



################################################################################
################################################################################
################################### PLOTTING ###################################

library(dplyr)
library(ggplot2)

################################################################################
# ------------------------------Eastern Towhee-------------------------------- #
################################################################################

GeoDist_Ecoregion_NULL_Data_Eastern <- read.csv("~/GeoDist_WithinEcoregion_vs_Null_MantelTest_Eastern.csv")

# Mark significant cells
df_E_NULL <- GeoDist_Ecoregion_NULL_Data_Eastern %>%
  mutate(
    Significant = ifelse(P_value_adj < 0.05, "*", "")
  )

# Plot
ggplot(df_E_NULL, aes(x = Feature, y = Ecoregion, fill = Mantel_r_obs)) +
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


################################################################################
# ------------------------------Spotted Towhee-------------------------------- #
################################################################################

GeoDist_Ecoregion_NULL_Data_Spotted <- read.csv("~/GeoDist_WithinEcoregion_vs_Null_MantelTest_Spotted.csv")

# Compute absolute value of Mantel R and mark significant cells
df_S_NULL <- GeoDist_Ecoregion_NULL_Data_Spotted %>%
  mutate(
    Significant = ifelse(P_value_adj < 0.05, "*", "")
  )

# Plot
ggplot(df_S_NULL, aes(x = Feature, y = Ecoregion, fill = Mantel_r_obs)) +
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

