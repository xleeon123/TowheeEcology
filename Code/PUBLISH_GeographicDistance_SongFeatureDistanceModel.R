
#Author: Ximena Leon

################################################################################
################################################################################
################################################################################
################### Each song feature separately to assess if ##################
################### song feature correlates with geo distance ##################
################################################################################

################################################################################
############################# SPOTTED + EASTERN ################################
################################################################################
library(geodist)
library(vegan)

# Load and preprocess
TowheeData <- read.csv("~/PUBLISH_ALL_FINAL_TowheeResultsData_CLEANED.csv")

#Log transform
TowheeData[,c(7:14)] <- log(TowheeData[,c(7:14)])

# Z-score transformation for all features
TowheeData[, c(7:14)] <- scale(TowheeData[, c(7:14)])

# Prepare geographic distance matrix
coords <- TowheeData[, c(6, 5)]  # Longitude, Latitude
colnames(coords) <- c("longitude", "latitude")
geo_dist_matrix <- geodist(coords, measure = "geodesic")
geo_dist <- as.dist(geo_dist_matrix)

# Loop over song feature columns 13 to 20
song_feature_cols <- 7:14
results <- data.frame(
  Feature = names(TowheeData)[song_feature_cols],
  Mantel_r = NA,
  P_value = NA,
  P_value_adj = NA
)

for (i in seq_along(song_feature_cols)) {
  feature_col <- song_feature_cols[i]
  feature_name <- names(TowheeData)[feature_col]
  
  # Create distance matrix for this feature
  feature_dist <- dist(TowheeData[[feature_col]], method = "euclidean")
  
  # Run Mantel test
  mantel_res <- vegan::mantel(geo_dist, feature_dist, method = "pearson", permutations = 999)
  
  # Store results
  results$Mantel_r[i] <- mantel_res$statistic
  results$P_value[i] <- mantel_res$signif
}

# Apply Bonferroni correction
results$P_value_adj <- p.adjust(results$P_value, method = "fdr")


significant_results <- results %>%
  filter(P_value_adj < 0.05) %>%
  arrange(P_value_adj)


# Print results
print(results)
IBD_SongFeatureResults <- results

#write.csv(IBD_SongFeatureResults, row.names = FALSE, "~/GeoDist_SongFeatureResults_SpottedandEasternCombined.csv")

################################################################################
########################## EACH SPECIES SEPARATELY #############################
################################################################################


library(geodist)
library(vegan)

# Load and preprocess
TowheeData <- read.csv("~/PUBLISH_ALL_FINAL_TowheeResultsData_CLEANED.csv")

#choose the species #repeat with Spotted towhee
TowheeDataSpecies <- TowheeData[TowheeData$Species == "Pipilo erythrophthalmus", ] #Pipilo maculatus #Pipilo erythrophthalmus

#Log transform
TowheeDataSpecies[,c(7:14)] <- log(TowheeDataSpecies[,c(7:14)])

# Z-score transformation for all features
TowheeDataSpecies[, c(7:14)] <- scale(TowheeDataSpecies[, c(7:14)])

# Prepare geographic distance matrix
coords <- TowheeDataSpecies[, c(6,5)]  # Longitude, Latitude
colnames(coords) <- c("longitude", "latitude")
geo_dist_matrix <- geodist(coords, measure = "geodesic")
geo_dist <- as.dist(geo_dist_matrix)

# Loop over song feature columns 13 to 20
song_feature_cols <- 7:14
results <- data.frame(
  Feature = names(TowheeDataSpecies)[song_feature_cols],
  Mantel_r = NA,
  P_value = NA,
  P_value_adj = NA
)

for (i in seq_along(song_feature_cols)) {
  feature_col <- song_feature_cols[i]
  feature_name <- names(TowheeDataSpecies)[feature_col]
  
  # Create distance matrix for this feature
  feature_dist <- dist(TowheeDataSpecies[[feature_col]], method = "euclidean")
  
  # Run Mantel test
  mantel_res <- vegan::mantel(geo_dist, feature_dist, method = "pearson", permutations = 999)
  
  # Store results
  results$Mantel_r[i] <- mantel_res$statistic
  results$P_value[i] <- mantel_res$signif
}

# Apply FDR correction
results$P_value_adj <- p.adjust(results$P_value, method = "fdr")

# Print results
print(results)
IBD_SongFeatureResults_Species <- results

significant_results <- results %>%
  filter(P_value_adj < 0.05) %>%
  arrange(P_value_adj)

#write.csv(IBD_SongFeatureResults_Species, row.names = FALSE, "~/GeoDist_SongFeatureResults_EasternTowheeONLY.csv")
#repeat with Spotted towhee