#Author: Ximena Leon

################################################################################
############ Is there a difference in the song feature between #################
################ ecoregions compared to within ecoregions when #################
##################### accounting for geographic distance? ######################
################################################################################
library(geodist)
library(dplyr)
library(tidyr)
library(stats)

# Load and preprocess
TowheeData <- read.csv("~/PUBLISH_ALL_FINAL_TowheeResultsData_CLEANED.csv")

# Filter one species #repeat for Eastern towhee
TowheeDataSpecies <- TowheeData %>%
  filter(Species == "Pipilo maculatus") %>%
  group_by(Level1Ecoregion_Edited) %>%
  filter(n() >= 10) %>%
  ungroup()

# Log and z-score transform features
TowheeDataSpecies[, 7:14] <- scale(log(TowheeDataSpecies[, 7:14]))

# Geographic distance matrix
coords <- TowheeDataSpecies[, c("Longitude", "Latitude")]
geo_dist_matrix <- geodist(coords, measure = "geodesic")
geo_dist <- as.dist(geo_dist_matrix)

# Add Row IDs
TowheeDataSpecies$RowID <- 1:nrow(TowheeDataSpecies)

# All pairwise combinations
pairwise_df <- expand.grid(ID1 = TowheeDataSpecies$RowID,
                           ID2 = TowheeDataSpecies$RowID) %>%
  filter(ID1 < ID2)

# Add ecoregion info
pairwise_df <- pairwise_df %>%
  left_join(TowheeDataSpecies[, c("RowID", "Level1Ecoregion_Edited")],
            by = c("ID1" = "RowID")) %>%
  rename(Ecoregion1 = Level1Ecoregion_Edited) %>%
  left_join(TowheeDataSpecies[, c("RowID", "Level1Ecoregion_Edited")],
            by = c("ID2" = "RowID")) %>%
  rename(Ecoregion2 = Level1Ecoregion_Edited)

# Define song feature columns
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
  
  # Clean and label pairs consistently
  pairwise_df_clean <- pairwise_df %>%
    filter(!is.na(Ecoregion1) & !is.na(Ecoregion2)) %>%
    mutate(Pair = ifelse(Ecoregion1 < Ecoregion2,
                         paste(Ecoregion1, Ecoregion2, sep = "_"),
                         paste(Ecoregion2, Ecoregion1, sep = "_")),
           PairType = ifelse(Ecoregion1 == Ecoregion2, "Within", "Between"))
  
  # Define background as all within-ecoregion residuals
  within_residuals <- pairwise_df_clean %>%
    filter(PairType == "Within") %>%
    pull(Residual)
  
  mean_within_resid <- mean(within_residuals, na.rm = TRUE)
  
  # Skip if too few within-ecoregion pairs
  if (length(within_residuals) < 5) next
  
  # Compare between-ecoregion pairs to within-ecoregion residuals
  pairwise_summary <- pairwise_df_clean %>%
    filter(PairType == "Between") %>%
    group_by(Pair) %>%
    summarize(
      Mean_Residual_Between = mean(Residual, na.rm = TRUE),
      Mean_Residual_Within = mean_within_resid,  # new column
      T_test_pval = tryCatch(
        t.test(Residual, within_residuals, alternative = "two.sided")$p.value,
        error = function(e) NA
      ),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(Feature = feature_name)
  
  results_all <- bind_rows(results_all, pairwise_summary)
}

# FDR correction
results_all$P_value_adj <- p.adjust(results_all$T_test_pval, method = "fdr")

# Significant results
significant_results <- results_all %>%
  filter(P_value_adj < 0.05) %>%
  arrange(P_value_adj)

# Output
print(significant_results)

# Optionally save all results
#write.csv(results_all, "~/GeoDist_EcoregionResidualTtests_WithinVSAcrossEcoregions_SpottedTowhee.csv", row.names = FALSE)
#repeat for Eastern towhee



################################################################################
################################################################################
################################### PLOTTING ###################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(patchwork)


################################################################################
# ------------------------------Eastern Towhee-------------------------------- #
################################################################################

GeoDist_Residual_Eastern <- read.csv("~/GeoDist_EcoregionResidualTtests_WithinVSAcrossEcoregions_EasternTowhee.csv")

df_prepared <- GeoDist_Residual_Eastern %>%
  separate(Pair, into = c("Ecoregion1", "Ecoregion2"), sep = "_") %>%
  mutate(Significant = ifelse(P_value_adj < 0.05, "*", "")) 
# %>%
#   mutate(Delta_Mean_Residual = Overall_Mean_Residual - Mean_Residual)


# Define plotting function
plot_feature <- function(df, feature_name) {
  df_feature <- df %>% filter(Feature == feature_name)
  
  ggplot(df_feature, aes(x = Ecoregion1, y = Ecoregion2, fill = Mean_Residual_Between)) +
    geom_tile(color = "black") +
    geom_text(aes(label = Significant), color = "black", size = 8) +
    scale_fill_gradient2(
      low = "darkgrey",
      mid = "white",
      high = "#487bb6",
      name = "Mean_Residual",
      midpoint = 0,
      limits = c(-0.5,0.5) #min = -0.2457704 #max = 0.3941132
    ) +
    coord_equal() +
    labs(
      # x = "Ecoregion1",
      # y = "Ecoregion2",
      title = paste0("Feature: ", feature_name)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}


features <- unique(df_prepared$Feature)
plots <- map(features, ~ plot_feature(df_prepared, .x))

# Remove individual legends
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))

# Remove axes conditionally
remove_axes <- function(p, show_x = TRUE, show_y = TRUE) {
  p + theme(
    axis.title.x = if (show_x) element_text() else element_blank(),
    axis.text.x  = if (show_x) element_text() else element_blank(),
    axis.ticks.x = if (show_x) element_line() else element_blank(),
    axis.title.y = if (show_y) element_text() else element_blank(),
    axis.text.y  = if (show_y) element_text() else element_blank(),
    axis.ticks.y = if (show_y) element_line() else element_blank()
  )
}

ncol_grid <- 4
nrow_grid <- ceiling(length(plots_no_legend) / ncol_grid)

plots_adjusted <- list()
for(i in seq_along(plots_no_legend)) {
  row <- ceiling(i / ncol_grid)
  col <- ifelse(i %% ncol_grid == 0, ncol_grid, i %% ncol_grid)
  
  show_y <- col == 1            # Only left column shows y-axis
  show_x <- row == nrow_grid    # Only bottom row shows x-axis
  
  plots_adjusted[[i]] <- remove_axes(plots_no_legend[[i]], show_x, show_y)
}

# Combine plots 
wrap_plots(plots_adjusted, ncol = ncol_grid, guides = "collect") &
  theme(legend.position = "bottom",
        legend.key.width  = unit(1.5, "cm"))

#save PDF as 12x18 landscape


################################################################################
# ------------------------------Spotted Towhee-------------------------------- #
################################################################################

GeoDist_Residual_Spotted <- read.csv("~/GeoDist_EcoregionResidualTtests_WithinVSAcrossEcoregions_SpottedTowhee.csv")

df_prepared <- GeoDist_Residual_Spotted %>%
  separate(Pair, into = c("Ecoregion1", "Ecoregion2"), sep = "_") %>%
  mutate(Significant = ifelse(P_value_adj < 0.05, "*", ""))

# Define plotting function
plot_feature <- function(df, feature_name) {
  df_feature <- df %>% filter(Feature == feature_name)
  
  ggplot(df_feature, aes(x = Ecoregion1, y = Ecoregion2, fill = Mean_Residual_Between)) +
    geom_tile(color = "black") +
    geom_text(aes(label = Significant), color = "black", size = 8) +
    scale_fill_gradient2(
      low = "darkgrey",
      mid = "white",
      high = "#54b04a",
      name = "Mean_Residual",
      midpoint = 0,
      limits = c(-0.5,0.5) #min = -0.3583466 #max = 0.3918827
    ) +
    coord_equal() +
    labs(
      # x = "Ecoregion1",
      # y = "Ecoregion2",
      title = paste0("Feature: ", feature_name)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}

features <- unique(df_prepared$Feature)
plots <- map(features, ~ plot_feature(df_prepared, .x))

# Remove individual legends
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))

# Remove axes conditionally
remove_axes <- function(p, show_x = TRUE, show_y = TRUE) {
  p + theme(
    axis.title.x = if (show_x) element_text() else element_blank(),
    axis.text.x  = if (show_x) element_text() else element_blank(),
    axis.ticks.x = if (show_x) element_line() else element_blank(),
    axis.title.y = if (show_y) element_text() else element_blank(),
    axis.text.y  = if (show_y) element_text() else element_blank(),
    axis.ticks.y = if (show_y) element_line() else element_blank()
  )
}

ncol_grid <- 4
nrow_grid <- ceiling(length(plots_no_legend) / ncol_grid)

plots_adjusted <- list()
for(i in seq_along(plots_no_legend)) {
  row <- ceiling(i / ncol_grid)
  col <- ifelse(i %% ncol_grid == 0, ncol_grid, i %% ncol_grid)
  
  show_y <- col == 1            # Only left column shows y-axis
  show_x <- row == nrow_grid    # Only bottom row shows x-axis
  
  plots_adjusted[[i]] <- remove_axes(plots_no_legend[[i]], show_x, show_y)
}

# Combine plots
wrap_plots(plots_adjusted, ncol = ncol_grid, guides = "collect") &
  theme(legend.position = "bottom",
        legend.key.width  = unit(1.5, "cm"))

#save PDF as 12x18 landscape