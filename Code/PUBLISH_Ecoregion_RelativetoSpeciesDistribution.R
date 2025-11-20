################################################################################
################################################################################
########################## Analysis of Ecoregions ##############################
################################################################################
################################################################################

# Plotting species distribution and ecoregion specific distribution
library(ggplot2)
library(dplyr)
library(patchwork)

################################################################################
All_TowheeEcology_Data <- read.csv("~/PUBLISH_ALL_FINAL_TowheeResultsData_CLEANED.csv")

# Log transform
All_TowheeEcology_Data[,c(7:14)] <- log(All_TowheeEcology_Data[,c(7:14)])

# Assign specific colors to ecoregions
ecoregion_colors <- c(
  "WATER" = "#d1e7f2",
  "TAIGA" = "#c5d7e1",
  "TUNDRA" = "#afcde9",
  "ARCTIC CORDILLERA" = "#225595",
  "NORTHERN FORESTS" = "#506d6c",
  "EASTERN TEMPERATE FORESTS" = "#a5bf92",
  "GREAT PLAINS" = "#f6f2d2",
  "NORTH AMERICAN DESERTS" = "#f5eeab",
  "MEDITERRANEAN CALIFORNIA" = "#e7b95e",
  "NORTHWESTERN FORESTED MOUNTAINS" = "#d78133",
  "MARINE WEST COAST FOREST" = "#b15c4e",
  "TROPICAL WET FORESTS" = "#b1638c",
  "TROPICAL DRY FORESTS" = "#816993",
  "TEMPERATE SIERRAS" = "#a091ac",
  "SOUTHERN SEMIARID HIGHLANDS" = "#cdc9d6",
  "HUDSON PLAIN" = "#a7b1d2"
)

################################################################################
############################## SPOTTED TOWHEE ##################################
################################################################################

SpottedONLY <- All_TowheeEcology_Data %>%
  filter(Species == "Pipilo maculatus")

SpottedONLY_Filtered <- SpottedONLY %>%
  group_by(Level1Ecoregion_Edited) %>%
  filter(n() >= 10) %>%
  ungroup()

# Get song feature names (assumed to be columns 7 to 14)
song_features <- colnames(SpottedONLY_Filtered[7:14])

# Filter color palette to match ecoregions in data
ecoregions_in_data <- unique(SpottedONLY_Filtered$Level1Ecoregion_Edited)
colors_present <- ecoregion_colors[names(ecoregion_colors) %in% ecoregions_in_data]

# Set factor levels for consistent order
SpottedONLY_Filtered$Level1Ecoregion_Edited <- factor(SpottedONLY_Filtered$Level1Ecoregion_Edited, levels = names(colors_present))

################################################################################
# Add "All Spotted Towhee" group to dataset
################################################################################
# Tag original samples with group label = ecoregion
SpottedONLY_Filtered$GroupLabel <- SpottedONLY_Filtered$Level1Ecoregion_Edited

# Create new dataset for species-wide distribution
SpottedONLY_AllGroup <- SpottedONLY %>%
  mutate(GroupLabel = "All Spotted Towhee") %>%
  select(colnames(SpottedONLY_Filtered))  # ensure column match

# Combine both datasets
Spotted_PlotData <- bind_rows(SpottedONLY_Filtered, SpottedONLY_AllGroup)

# Define plotting order: All first, then ecoregions
group_levels <- c("All Spotted Towhee", levels(SpottedONLY_Filtered$Level1Ecoregion_Edited))
Spotted_PlotData$GroupLabel <- factor(Spotted_PlotData$GroupLabel, levels = group_levels)

################################################################################
# Plotting Violin Plots
################################################################################
violin_plots_ecoregion_spotted <- list()

for (feature in song_features) {
  p <- ggplot(Spotted_PlotData, aes_string(x = "GroupLabel", y = feature, fill = "GroupLabel")) +
    geom_violin(trim = FALSE, color = "black", alpha = 1) +
    geom_boxplot(width = 0.2, outlier.shape = 16, outlier.size = 1.5, fatten = 1, color = "black") +
    stat_summary(fun = mean, geom = "crossbar", width = 0.2, fatten = 0.7, linetype = "dotted", color = "black") +
    scale_fill_manual(values = c("All Spotted Towhee" = "gray50", colors_present)) +
    labs(x = "Group (Species-wide + Ecoregion)", y = feature, title = feature) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 6),
      legend.position = "none",
      plot.title = element_text(size = 10, face = "bold")
    )
  
  violin_plots_ecoregion_spotted[[feature]] <- p
}

################################################################################
# Combine and display all plots
################################################################################
grid_plot <- wrap_plots(violin_plots_ecoregion_spotted, ncol = 2)
grid_plot

# PDF 9x8 portrait

table(Spotted_PlotData$GroupLabel)


# Store p-values for each feature and ecoregion comparison
stat_results_spotted_ecoregion <- data.frame()

for (feature in song_features) {
  for (ecoregion in levels(SpottedONLY_Filtered$Level1Ecoregion_Edited)) {
    
    # Extract values for each group
    all_values <- Spotted_PlotData %>%
      filter(GroupLabel == "All Spotted Towhee") %>%
      pull(!!sym(feature))
    
    eco_values <- Spotted_PlotData %>%
      filter(GroupLabel == ecoregion) %>%
      pull(!!sym(feature))
    
    # Run Wilcoxon rank-sum test
    if (length(eco_values) >= 3 && length(all_values) >= 3) {
      test_result <- wilcox.test(eco_values, all_values)
      
      stat_results_spotted_ecoregion <- rbind(stat_results_spotted_ecoregion, data.frame(
        Feature = feature,
        Ecoregion = ecoregion,
        W_Statistic = test_result$statistic,
        P_Value = test_result$p.value
      ))
    }
  }
}

# Adjust p-values for multiple testing (optional)
stat_results_spotted_ecoregion$Adj_P <- p.adjust(stat_results_spotted_ecoregion$P_Value, method = "fdr")
stat_results_spotted_ecoregion %>% filter(Adj_P < 0.05)


#write.csv(stat_results_spotted_ecoregion, "~/Ecoregion_WilcoxonResults_SpottedONLY.csv", row.names = FALSE)

################################################################################
################################################################################
############################## EASTERN TOWHEE ##################################
################################################################################
################################################################################

EasternONLY <- All_TowheeEcology_Data %>%
  filter(Species == "Pipilo erythrophthalmus")

EasternONLY_Filtered <- EasternONLY %>%
  group_by(Level1Ecoregion_Edited) %>%
  filter(n() >= 10) %>%
  ungroup()

# Get song feature names (assumed to be columns 7 to 14)
song_features <- colnames(EasternONLY_Filtered[7:14])

# Filter color palette to match ecoregions in data
ecoregions_in_data <- unique(EasternONLY_Filtered$Level1Ecoregion_Edited)
colors_present <- ecoregion_colors[names(ecoregion_colors) %in% ecoregions_in_data]

# Set factor levels for consistent order
EasternONLY_Filtered$Level1Ecoregion_Edited <- factor(EasternONLY_Filtered$Level1Ecoregion_Edited, levels = names(colors_present))

################################################################################
# Add "All Eastern Towhee" group to dataset
################################################################################
# Tag original samples with group label = ecoregion
EasternONLY_Filtered$GroupLabel <- EasternONLY_Filtered$Level1Ecoregion_Edited

# Create new dataset for species-wide distribution
EasternONLY_AllGroup <- EasternONLY %>%
  mutate(GroupLabel = "All Eastern Towhee") %>%
  select(colnames(EasternONLY_Filtered))  # ensure column match

# Combine both datasets
Eastern_PlotData <- bind_rows(EasternONLY_Filtered, EasternONLY_AllGroup)

# Define plotting order: All first, then ecoregions
group_levels <- c("All Eastern Towhee", levels(EasternONLY_Filtered$Level1Ecoregion_Edited))
Eastern_PlotData$GroupLabel <- factor(Eastern_PlotData$GroupLabel, levels = group_levels)

################################################################################
# Plotting Violin Plots
################################################################################
violin_plots_ecoregion_eastern <- list()

for (feature in song_features) {
  p <- ggplot(Eastern_PlotData, aes_string(x = "GroupLabel", y = feature, fill = "GroupLabel")) +
    geom_violin(trim = FALSE, color = "black", alpha = 1) +
    geom_boxplot(width = 0.2, outlier.shape = 16, outlier.size = 1.5, fatten = 1, color = "black") +
    stat_summary(fun = mean, geom = "crossbar", width = 0.2, fatten = 0.7, linetype = "dotted", color = "black") +
    scale_fill_manual(values = c("All Eastern Towhee" = "gray50", colors_present)) +
    labs(x = "Group (Species-wide + Ecoregion)", y = feature, title = feature) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 6),
      legend.position = "none",
      plot.title = element_text(size = 10, face = "bold")
    )
  
  violin_plots_ecoregion_spotted[[feature]] <- p
}

################################################################################
# Combine and display all plots
################################################################################
grid_plot <- wrap_plots(violin_plots_ecoregion_spotted, ncol = 2)
grid_plot

# PDF 9x8 portrait
table(Eastern_PlotData$GroupLabel)


###### WILCOXON
# Store p-values for each feature and ecoregion comparison
stat_results_eastern_ecoregion <- data.frame()

for (feature in song_features) {
  for (ecoregion in levels(EasternONLY_Filtered$Level1Ecoregion_Edited)) {
    
    # Extract values for each group
    all_values <- Eastern_PlotData %>%
      filter(GroupLabel == "All Eastern Towhee") %>%
      pull(!!sym(feature))
    
    eco_values <- Eastern_PlotData %>%
      filter(GroupLabel == ecoregion) %>%
      pull(!!sym(feature))
    
    # Run Wilcoxon rank-sum test
    if (length(eco_values) >= 3 && length(all_values) >= 3) {
      test_result <- wilcox.test(eco_values, all_values)
      
      stat_results_eastern_ecoregion <- rbind(stat_results_eastern_ecoregion, data.frame(
        Feature = feature,
        Ecoregion = ecoregion,
        W_Statistic = test_result$statistic,
        P_Value = test_result$p.value
      ))
    }
  }
}

# Adjust p-values for multiple testing (optional)
stat_results_eastern_ecoregion$Adj_P <- p.adjust(stat_results_eastern_ecoregion$P_Value, method = "fdr")
stat_results_eastern_ecoregion %>% filter(Adj_P < 0.05)

#write.csv(stat_results_eastern_ecoregion, "~/Ecoregion_WilcoxonResults_EasternONLY.csv", row.names = FALSE)
