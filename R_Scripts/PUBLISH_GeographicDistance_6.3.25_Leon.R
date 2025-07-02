
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
TowheeData <- read.csv("~/PUBLISH_FINAL_SongData_wEcologyClimateUrbanizationElevation_6.6.25_XL.csv")

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
TowheeData <- read.csv("~/FINAL_SONG_DATA/PUBLISH_FINAL_SongData_wEcologyClimateUrbanizationElevation_6.6.25_XL.csv")

#choose the species
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




################################################################################
################################################################################
################################################################################
################# Combining all the song features to assess if #################
############# overall song similarity correlates with geo distance #############
################################################################################

################################################################################
############################# SPOTTED + EASTERN ################################
################################################################################
library(geodist)
library(vegan)

TowheeData <- read.csv("~/PUBLISH_FINAL_SongData_wEcologyClimateUrbanizationElevation_6.6.25_XL.csv")

#to log transform:
TowheeData[,c(7:14)] <- log(TowheeData[,c(7:14)]) 

#to do z-score transformation
TowheeData[,c(7:14)] <- scale(TowheeData[,c(7:14)])

coords <- TowheeData[,c(6,5)]
colnames(coords) <- c("longitude", "latitude")
geo_dist = geodist(coords, measure = "geodesic") #matrix should be same as syll dist
geo_dist <- as.dist(geo_dist)

songdist = dist(TowheeData[,c(7:14)], method = "euclidean")

#each point is the geographic distance between two points
#and I am plotting the geographical distance of those points against the syllable distance (so how different the syllables are)
#testdist=cbind(geo_dist, songdist)
#plot(geo_dist, songdist)
#modelresults=lm(songdist~geo_dist,as.data.frame(testdist))
#summary(modelresults)
#abline(modelresults, col = "red")

#mantel test of matrices 
#library(ade4)
#mantel.rtest(geo_dist, songdist, nrepet = 999)

mantel_res2 <- vegan::mantel(geo_dist, songdist, method = "pearson", perm = 999)

results2 <- data.frame(
  Mantel_r = NA,
  P_value = NA
)

results2$Mantel_r <- mantel_res2$statistic
results2$P_value <- mantel_res2$signif


IBD_SongDistanceModelResults <- results2
#write.csv(IBD_SongDistanceModelResults, row.names = FALSE, "~GeoDist_SongDistanceModelResults_SpottedandEasternCombined.csv")
  


################################################################################
########################## EACH SPECIES SEPARATELY #############################
################################################################################
library(geodist)
library(vegan)

# Load and preprocess
TowheeData <- read.csv("~/PUBLISH_FINAL_SongData_wEcologyClimateUrbanizationElevation_6.6.25_XL.csv")

#choose the species
TowheeDataSpecies <- TowheeData[TowheeData$Species == "Pipilo maculatus", ] #Pipilo maculatus #Pipilo erythrophthalmus

#to log transform:
TowheeDataSpecies[,c(7:14)] <- log(TowheeDataSpecies[,c(7:14)]) 

#to do z-score transformation
TowheeDataSpecies[,c(7:14)] <- scale(TowheeDataSpecies[,c(7:14)])

coords <- TowheeDataSpecies[,c(6,5)]
colnames(coords) <- c("longitude", "latitude")
geo_dist = geodist(coords, measure = "geodesic") #matrix should be same as syll dist
geo_dist <- as.dist(geo_dist)

songdist = dist(TowheeDataSpecies[,c(7:14)], method = "euclidean")

mantel_res2 <- vegan::mantel(geo_dist, songdist, method = "pearson", perm = 999)

results2 <- data.frame(
  Mantel_r = NA,
  P_value = NA
)

results2$Mantel_r <- mantel_res2$statistic
results2$P_value <- mantel_res2$signif


IBD_SongDistanceModelResults <- results2
#write.csv(IBD_SongDistanceModelResults, row.names = FALSE, "~/GeoDist_SongDistanceModelResults_SpottedTowheeONLY.csv")
################################################################################
################################################################################





################################################################################
################################################################################
################################################################################
############ Is there a difference in the song feature between #################
############# ecoregions when accounting for geographic distance? ##############
################################################################################
library(geodist)
library(dplyr)
library(tidyr)
library(stats)

# Load and preprocess
TowheeData <- read.csv("~/PUBLISH_FINAL_SongData_wEcologyClimateUrbanizationElevation_6.6.25_XL.csv")

# Filter to one species
TowheeDataSpecies <- TowheeData[TowheeData$Species == "Pipilo maculatus", ]

TowheeDataSpecies <- TowheeDataSpecies %>%
  group_by(Level1Ecoregion_Edited) %>%
  filter(n() >= 10) %>%
  ungroup()

# Log and z-score transform features
TowheeDataSpecies[, 7:14] <- scale(log(TowheeDataSpecies[, 7:14]))

# Geographic distance matrix
coords <- TowheeDataSpecies[, c("Longitude", "Latitude")]
geo_dist_matrix <- geodist(coords, measure = "geodesic")
geo_dist <- as.dist(geo_dist_matrix)

# Add row IDs to match distances
TowheeDataSpecies$RowID <- 1:nrow(TowheeDataSpecies)

# Create all pairwise combinations
pairwise_df <- expand.grid(ID1 = TowheeDataSpecies$RowID, ID2 = TowheeDataSpecies$RowID) %>%
  filter(ID1 < ID2)

# Add ecoregion info
pairwise_df <- pairwise_df %>%
  left_join(TowheeDataSpecies[, c("RowID", "Level1Ecoregion_Edited")], by = c("ID1" = "RowID")) %>%
  rename(Ecoregion1 = Level1Ecoregion_Edited) %>%
  left_join(TowheeDataSpecies[, c("RowID", "Level1Ecoregion_Edited")], by = c("ID2" = "RowID")) %>%
  rename(Ecoregion2 = Level1Ecoregion_Edited)

# Define song features
song_feature_cols <- 7:14
results_all <- data.frame()

for (feature_col in song_feature_cols) {
  feature_name <- names(TowheeDataSpecies)[feature_col]
  
  # Song feature distance matrix
  feature_dist <- dist(TowheeDataSpecies[[feature_col]], method = "euclidean")
  
  # Convert matrices to vectors
  geo_vec <- as.vector(geo_dist)
  feat_vec <- as.vector(feature_dist)
  
  # Linear model and residuals
  lm_fit <- lm(feat_vec ~ geo_vec)
  residuals_vec <- resid(lm_fit)
  
  # Add residuals to pairwise dataframe
  pairwise_df$Residual <- residuals_vec
  
  # Remove NA and make sure ecoregion pairs are labeled consistently
  pairwise_df_clean <- pairwise_df %>%
    filter(!is.na(Ecoregion1) & !is.na(Ecoregion2)) %>%
    mutate(Pair = ifelse(Ecoregion1 < Ecoregion2,
                         paste(Ecoregion1, Ecoregion2, sep = "_"),
                         paste(Ecoregion2, Ecoregion1, sep = "_")))
  
  overall_mean <- mean(TowheeDataSpecies[[feature_col]], na.rm = TRUE)
  
  # Compute t-test per ecoregion pair
  pairwise_summary <- pairwise_df_clean %>%
    group_by(Pair) %>%
    summarize(
      Mean_Residual = mean(Residual, na.rm = TRUE),
      T_test_pval = tryCatch(t.test(Residual, mu = overall_mean, alternative = "two.sided")$p.value, error = function(e) NA),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(Feature = feature_name)
  
  results_all <- bind_rows(results_all, pairwise_summary)
}

# FDR correction
results_all$P_value_adj <- p.adjust(results_all$T_test_pval, method = "fdr")

significant_results <- results_all %>%
  filter(P_value_adj < 0.05) %>%
  arrange(P_value_adj)

# Output results
print(results_all)
#write.csv(results_all, "~/GeoDist_EcoregionResidualTtests_AcrossEcoregions_SpottedTowhee.csv", row.names = FALSE)

#for spotted towhee, 166 out of 289 comparisons are statistically significant
#for eastern towhee, 62 out of 81 comparisons were statistically significant




################################################################################
################################################################################
################################################################################
################ Is there  a stronger geographic pattern within ################
################## ecoregions than we would expect given the ###################
################### distribution of the species as a whole? ####################
################################################################################
library(geodist)
library(vegan)
library(dplyr)

# Load and preprocess
TowheeData <- read.csv("~/PUBLISH_FINAL_SongData_wEcologyClimateUrbanizationElevation_6.6.25_XL.csv")

# Filter to one species
TowheeDataSpecies <- TowheeData[TowheeData$Species == "Pipilo erythrophthalmus", ]

TowheeDataSpecies <- TowheeDataSpecies %>%
  group_by(Level1Ecoregion_Edited) %>%
  filter(n() >= 10) %>%
  ungroup()

# Transform features (log1p for safety, then z-score)
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
#write.csv(IBD_vs_Null_results, "~/GeoDist_WithinEcoregion_vs_Null_MantelTest_Spotted.csv", row.names = FALSE)


#P_value_adj > 0.05 for all ecoregions and song features and species: none of the observed r-values 
#were extreme enough to be statistically significant after correcting for multiple comparisons.




################################################################################
################################################################################
################################################################################
################# Is there evidence for geo dist affecting song ################
###################### differences within each ecoregion? ######################
################################################################################
install.packages('geodist')
library(geodist)
library(dplyr)
library(vegan)

# Load and preprocess towhee dataframe
TowheeData <- read.csv("~/PUBLISH_FINAL_SongData_wEcologyClimateUrbanizationElevation_6.6.25_XL.csv")

# Filter to one species
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

