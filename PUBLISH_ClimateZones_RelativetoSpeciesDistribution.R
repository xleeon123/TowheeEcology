################################################################################
################################################################################
######################## Analysis of Climate Zones #############################
################################################################################
################################################################################

# Plotting species distribution and climatezone specific distribution
library(ggplot2)
library(dplyr)
library(patchwork)

################################################################################
All_TowheeEcology_Data <- read.csv("~/PUBLISH_ALL_FINAL_TowheeResultsData_CLEANED.csv")

# Log transform
All_TowheeEcology_Data[,c(7:14)] <- log(All_TowheeEcology_Data[,c(7:14)])

# Assign specific colors to climate regions
climatezone_colors <- c(
  "ET" = "#b2b2b2",
  "EF" = "#666666",
  "Cfb: Temperate Oceanic Climate" = "#4a4181",
  "Dsd" = "#8c6396",
  "Am: Tropical Monsoon" = "#a091ac",
  "Af" = "#cdc9d6",
  "Csc" = "#e9e9ea",
  "Dfc: Subarctic with Cool Summers and Year-round Precipitation" = "#afcde9",
  "Cwc: Cold Subtropical Highland / Subpolar Oceanic" = "#4da2d2",
  "Dsc: Continental Subarctic - Cold Dry Summer" = "#225595",
  "Dfd" = "#00324f",
  "Dfb: Humid Continental Mild Summer, Wet All Year" = "#007d7d",
  "Dfa: Humid Continental Hot Summers with Year-round Precipitation" = "#506d6c",
  "Cfc" = "#32c800",
  "Dsb: Humid Continental Climate - Dry Cool Summer" = "#497f00",
  "Csb: Warm-Summer Mediterranean Climate" = "#9cae22",
  "Cfa: Humid Subtropical Climate" = "#a5bf92",
  "Csa: Hot-Summer Mediterranean Climate" = "#e3eeab",
  "BSk: Cold Semi-Arid Climate" = "#f6f2d2",
  "BWk: Cold Desert Climate" = "#f5df8b",
  "Dwa" = "#dab217",
  "BSh: Hot Semi-Arid Climate" = "#d78133",
  "Dsa: Humid Continental Climate - Dry Warm Summer" = "#f28f7d",
  "BWh: Hot Desert Climate" = "#b15c4e",
  "Aw: Tropical Savanna (Wet and Dry Climate)" = "#931d2f",
  "Dwb" = "#5d2b0e",
  "Cwb: Subtropical Highland Climate or Temperate Oceanic Climate with Dry Winters" = "#b1638c",
  "Dwc" = "#c2509e",
  "Cwa: Warm Oceanic Climate / Humid Subtropical Climate" = "#ffdde1"
)

################################################################################
############################## SPOTTED TOWHEE ##################################
################################################################################

SpottedONLY <- All_TowheeEcology_Data %>%
  filter(Species == "Pipilo maculatus")

SpottedONLY_Filtered <- SpottedONLY %>%
  group_by(KG_ClimateZones_Edited) %>%
  filter(n() >= 10) %>%
  ungroup()

# Get song feature names (assumed to be columns 7 to 14)
song_features <- colnames(SpottedONLY_Filtered[7:14])

# Filter color palette to match climatezones in data
climatezones_in_data <- unique(SpottedONLY_Filtered$KG_ClimateZones_Edited)
colors_present <- climatezone_colors[names(climatezone_colors) %in% climatezones_in_data]

# Set factor levels for consistent order
SpottedONLY_Filtered$KG_ClimateZones_Edited <- factor(SpottedONLY_Filtered$KG_ClimateZones_Edited, levels = names(colors_present))

################################################################################
# Add "All Spotted Towhee" group to dataset
################################################################################
# Tag original samples with group label = climatezone
SpottedONLY_Filtered$GroupLabel <- SpottedONLY_Filtered$KG_ClimateZones_Edited

# Create new dataset for species-wide distribution
SpottedONLY_AllGroup <- SpottedONLY %>%
  mutate(GroupLabel = "All Spotted Towhee") %>%
  select(colnames(SpottedONLY_Filtered))  # ensure column match

# Combine both datasets
Spotted_PlotData <- bind_rows(SpottedONLY_Filtered, SpottedONLY_AllGroup)

# Define plotting order: All first, then climatezones
group_levels <- c("All Spotted Towhee", levels(SpottedONLY_Filtered$KG_ClimateZones_Edited))
Spotted_PlotData$GroupLabel <- factor(Spotted_PlotData$GroupLabel, levels = group_levels)

################################################################################
# Plotting Violin Plots
################################################################################
violin_plots_climatezone_spotted <- list()

for (feature in song_features) {
  p <- ggplot(Spotted_PlotData, aes_string(x = "GroupLabel", y = feature, fill = "GroupLabel")) +
    geom_violin(trim = FALSE, color = "black", alpha = 1) +
    geom_boxplot(width = 0.2, outlier.shape = 16, outlier.size = 1.5, fatten = 1, color = "black") +
    stat_summary(fun = mean, geom = "crossbar", width = 0.2, fatten = 0.7, linetype = "dotted", color = "black") +
    scale_fill_manual(values = c("All Spotted Towhee" = "gray50", colors_present)) +
    labs(x = "Group (Species-wide + Climate Zone)", y = feature, title = feature) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 6),
      legend.position = "none",
      plot.title = element_text(size = 10, face = "bold")
    )
  
  violin_plots_climatezone_spotted[[feature]] <- p
}

################################################################################
# Combine and display all plots
################################################################################
grid_plot <- wrap_plots(violin_plots_climatezone_spotted, ncol = 2)
grid_plot
# PDF 9x10 portrait


table(Spotted_PlotData$GroupLabel)


# Store p-values for each feature and climatezone comparison
stat_results_spotted_climatezones <- data.frame()

for (feature in song_features) {
  for (climatezone in levels(SpottedONLY_Filtered$KG_ClimateZones_Edited)) {
    
    # Extract values for each group
    all_values <- Spotted_PlotData %>%
      filter(GroupLabel == "All Spotted Towhee") %>%
      pull(!!sym(feature))
    
    eco_values <- Spotted_PlotData %>%
      filter(GroupLabel == climatezone) %>%
      pull(!!sym(feature))
    
    # Run Wilcoxon rank-sum test
    if (length(eco_values) >= 3 && length(all_values) >= 3) {
      test_result <- wilcox.test(eco_values, all_values)
      
      stat_results_spotted_climatezones <- rbind(stat_results_spotted_climatezones, data.frame(
        Feature = feature,
        ClimateZone = climatezone,
        W_Statistic = test_result$statistic,
        P_Value = test_result$p.value
      ))
    }
  }
}

# Adjust p-values for multiple testing (optional)
stat_results_spotted_climatezones$Adj_P <- p.adjust(stat_results_spotted_climatezones$P_Value, method = "fdr")
stat_results_spotted_climatezones %>% filter(Adj_P < 0.05)

#write.csv(stat_results_spotted_climatezones, "~/ClimateZone_WilcoxonResults_SpottedONLY.csv", row.names = FALSE)




################################################################################
################################################################################
############################## EASTERN TOWHEE ##################################
################################################################################
################################################################################

EasternONLY <- All_TowheeEcology_Data %>%
  filter(Species == "Pipilo erythrophthalmus")

EasternONLY_Filtered <- EasternONLY %>%
  group_by(KG_ClimateZones_Edited) %>%
  filter(n() >= 10) %>%
  ungroup()

# Get song feature names (assumed to be columns 7 to 14)
song_features <- colnames(EasternONLY_Filtered[7:14])

# Filter color palette to match climatezones in data
climatezones_in_data <- unique(EasternONLY_Filtered$KG_ClimateZones_Edited)
colors_present <- climatezone_colors[names(climatezone_colors) %in% climatezones_in_data]

# Set factor levels for consistent order
EasternONLY_Filtered$KG_ClimateZones_Edited <- factor(EasternONLY_Filtered$KG_ClimateZones_Edited, levels = names(colors_present))

################################################################################
# Add "All Eastern Towhee" Group to Dataset
################################################################################
# Tag original samples with group label = climatezone
EasternONLY_Filtered$GroupLabel <- EasternONLY_Filtered$KG_ClimateZones_Edited

# Create new dataset for species-wide distribution
EasternONLY_AllGroup <- EasternONLY %>%
  mutate(GroupLabel = "All Eastern Towhee") %>%
  select(colnames(EasternONLY_Filtered))  # ensure column match

# Combine both datasets
Eastern_PlotData <- bind_rows(EasternONLY_Filtered, EasternONLY_AllGroup)

# Define plotting order: All first, then climatezones
group_levels <- c("All Eastern Towhee", levels(EasternONLY_Filtered$KG_ClimateZones_Edited))
Eastern_PlotData$GroupLabel <- factor(Eastern_PlotData$GroupLabel, levels = group_levels)

################################################################################
# Plotting Violin Plots
################################################################################
violin_plots_climatezone_eastern <- list()

for (feature in song_features) {
  p <- ggplot(Eastern_PlotData, aes_string(x = "GroupLabel", y = feature, fill = "GroupLabel")) +
    geom_violin(trim = FALSE, color = "black", alpha = 1) +
    geom_boxplot(width = 0.2, outlier.shape = 16, outlier.size = 1.5, fatten = 1, color = "black") +
    stat_summary(fun = mean, geom = "crossbar", width = 0.2, fatten = 0.7, linetype = "dotted", color = "black") +
    scale_fill_manual(values = c("All Eastern Towhee" = "gray50", colors_present)) +
    labs(x = "Group (Species-wide + Climate Zone)", y = feature, title = feature) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 6),
      legend.position = "none",
      plot.title = element_text(size = 10, face = "bold")
    )
  
  violin_plots_climatezone_eastern[[feature]] <- p
}

################################################################################
# Combine and display all plots
################################################################################
grid_plot <- wrap_plots(violin_plots_climatezone_eastern, ncol = 2)
grid_plot

# PDF 9x8 portrait

table(Eastern_PlotData$GroupLabel)


###### WILCOXON
# Store p-values for each feature and climatezone comparison
stat_results_eastern_climatezones <- data.frame()

for (feature in song_features) {
  for (climatezone in levels(EasternONLY_Filtered$KG_ClimateZones_Edited)) {
    
    # Extract values for each group
    all_values <- Eastern_PlotData %>%
      filter(GroupLabel == "All Eastern Towhee") %>%
      pull(!!sym(feature))
    
    eco_values <- Eastern_PlotData %>%
      filter(GroupLabel == climatezone) %>%
      pull(!!sym(feature))
    
    # Run Wilcoxon rank-sum test
    if (length(eco_values) >= 3 && length(all_values) >= 3) {
      test_result <- wilcox.test(eco_values, all_values)
      
      stat_results_eastern_climatezones <- rbind(stat_results_eastern_climatezones, data.frame(
        Feature = feature,
        ClimateZone = climatezone,
        W_Statistic = test_result$statistic,
        P_Value = test_result$p.value
      ))
    }
  }
}

# Adjust p-values for multiple testing (optional)
stat_results_eastern_climatezones$Adj_P <- p.adjust(stat_results_eastern_climatezones$P_Value, method = "fdr")
stat_results_eastern_climatezones %>% filter(Adj_P < 0.05)

#write.csv(stat_results_eastern_climatezones, "~/ClimateZone_WilcoxonResults_EasternONLY.csv", row.names = FALSE)
