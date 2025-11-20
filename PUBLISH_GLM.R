################################################################################
################################################################################
######################## Generalized Linear Models #############################
################################################################################
################################################################################

################################################################################
############################ Species Combined ##################################
################################################################################
# Read in environmental data
All_TowheeEcology_Data <- read.csv("~/PUBLISH_ALL_FINAL_TowheeResultsData_CLEANED.csv")
All_TowheeEcology_Data <- All_TowheeEcology_Data[,c(1,3,5:14,17:19,25,27)]

# Define the frequency columns 
frequency_columns <- 5:12

# Initialize an empty data frame to store the results
glm_results <- data.frame()

# Loop through each frequency variable (columns 7 to 14)
for (freq_col in frequency_columns) {
  
  # Get the name of the frequency variable (e.g., min_sylls_freq_Hz, etc.)
  freq_var <- colnames(All_TowheeEcology_Data)[freq_col]
  
  # Fit the GLM model for the current frequency variable
  glm_formula <- as.formula(paste(freq_var, "~ scale(TreeCover) + scale(NL_StableLights_Avg) + scale(PopulationDensity) + scale(Distance_to_Road_km) + factor(Species) + scale(Elevation_m) + scale(Longitude) + scale(Latitude)"))
  glm_model <- glm(glm_formula, 
                   data = All_TowheeEcology_Data, 
                   family = gaussian(link = "log"))
  
  # Get the model summary
  model_summary <- summary(glm_model)
  
  # Extract the coefficients (Estimate, Std. Error, z-value, p-value) for the environmental factors
  coef_table <- model_summary$coefficients
  
  # Store the results in the results data frame
  glm_results <- rbind(glm_results, data.frame(
    Frequency_Variable = freq_var,
    z_value_TreeCover = coef_table[2, 3],
    p_value_TreeCover = coef_table[2, 4],
    z_value_NL_StableLights_Avg = coef_table[3, 3],
    p_value_NL_StableLights_Avg = coef_table[3, 4],
    z_value_PopulationDensity = coef_table[4, 3],
    p_value_PopulationDensity = coef_table[4, 4],
    z_value_DistanceToRoad = coef_table[5, 3],
    p_value_DistanceToRoad = coef_table[5, 4],
    z_value_Species = coef_table[6, 3],
    p_value_Species = coef_table[6, 4],
    z_value_Elevation = coef_table[7, 3],
    p_value_Elevation = coef_table[7, 4],
    z_value_Longitude = coef_table[8, 3],
    p_value_Longitude = coef_table[8, 4],
    z_value_Latitude = coef_table[9, 3],
    p_value_Latitude = coef_table[9, 4]
  ))
}

# View the results
print(glm_results)


# How many samples went into glm?
length(model_summary[["deviance.resid"]]) #2848 total

#write.csv(glm_results, "~/GLMResults_10-20-25.csv", row.names = FALSE)


# #TO CHECK # OF SAMPLES IN GLM PER SPECIES
# # List all the predictor variables from your GLM
# predictors <- c("TreeCover", "NL_StableLights_Avg", "PopulationDensity",
#                 "Distance_to_Road_km",
#                 "Species", "Elevation_m", "Longitude", "Latitude")
# 
# # Create a cleaned dataset with no NAs in these columns
# glm_data_clean <- na.omit(All_TowheeEcology_Data[, predictors])
# 
# # Check how many of each species were included
# table(glm_data_clean$Species)
# 
# # (Optional) Total number of samples used in GLM
# nrow(glm_data_clean)

################################################################################
############################## Spotted ONLY ####################################
################################################################################
# Read in environmental data
All_TowheeEcology_Data <- read.csv("~/PUBLISH_ALL_FINAL_TowheeResultsData_CLEANED.csv")
All_TowheeEcology_Data <- All_TowheeEcology_Data[,c(1,3,5:14,17:19,25,27)]

SpottedONLY <- All_TowheeEcology_Data %>%
  filter(Species == "Pipilo maculatus") 

# Define the frequency columns 
frequency_columns <- 5:12

# Initialize an empty data frame to store the results
glm_results_SpottedONLY <- data.frame()

# Loop through each frequency variable (columns 7 to 14)
for (freq_col in frequency_columns) {
  
  # Get the name of the frequency variable (e.g., min_sylls_freq_Hz, etc.)
  freq_var <- colnames(SpottedONLY)[freq_col]
  
  # Fit the GLM model for the current frequency variable
  glm_formula <- as.formula(paste(freq_var, "~ scale(TreeCover) + scale(NL_StableLights_Avg) + scale(PopulationDensity) + scale(Distance_to_Road_km) + scale(Elevation_m) + scale(Longitude) + scale(Latitude)"))
  glm_model <- glm(glm_formula, 
                   data = SpottedONLY, 
                   family = gaussian(link = "log"))
  
  # Get the model summary
  model_summary_SpottedONLY <- summary(glm_model)
  
  # Extract the coefficients (Estimate, Std. Error, z-value, p-value) for the environmental factors
  coef_table <- model_summary_SpottedONLY$coefficients
  
  # Store the results in the results data frame
  glm_results_SpottedONLY <- rbind(glm_results_SpottedONLY, data.frame(
    Frequency_Variable = freq_var,
    z_value_TreeCover = coef_table[2, 3],
    p_value_TreeCover = coef_table[2, 4],
    z_value_NL_StableLights_Avg = coef_table[3, 3],
    p_value_NL_StableLights_Avg = coef_table[3, 4],
    z_value_PopulationDensity = coef_table[4, 3],
    p_value_PopulationDensity = coef_table[4, 4],
    z_value_DistanceToRoad = coef_table[5, 3],
    p_value_DistanceToRoad = coef_table[5, 4],
    z_value_Elevation = coef_table[6, 3],
    p_value_Elevation = coef_table[6, 4],
    z_value_Longitude = coef_table[7, 3],
    p_value_Longitude = coef_table[7, 4],
    z_value_Latitude = coef_table[8, 3],
    p_value_Latitude = coef_table[8, 4]
  ))
}

# View the results
print(glm_results_SpottedONLY)


# How many samples went into glm?
length(model_summary_SpottedONLY[["deviance.resid"]]) #1067 total 

#write.csv(glm_results_SpottedONLY, "~/GLM_Results_SpottedONLY_10-20-25.csv", row.names = FALSE)

################################################################################
############################## Eastern ONLY ####################################
################################################################################
# Read in environmental data
All_TowheeEcology_Data <- read.csv("~/PUBLISH_ALL_FINAL_TowheeResultsData_CLEANED.csv")
All_TowheeEcology_Data <- All_TowheeEcology_Data[,c(1,3,5:14,17:19,25,27)]

EasternONLY <- All_TowheeEcology_Data %>%
  filter(Species == "Pipilo erythrophthalmus") 

# Define the frequency columns 
frequency_columns <- 5:12

# Initialize an empty data frame to store the results
glm_results_EasternONLY <- data.frame()

# Loop through each frequency variable (columns 7 to 14)
for (freq_col in frequency_columns) {
  
  # Get the name of the frequency variable (e.g., min_sylls_freq_Hz, etc.)
  freq_var <- colnames(EasternONLY)[freq_col]
  
  # Fit the GLM model for the current frequency variable
  glm_formula <- as.formula(paste(freq_var, "~ scale(TreeCover) + scale(NL_StableLights_Avg) + scale(PopulationDensity) + scale(Distance_to_Road_km) + scale(Elevation_m) + scale(Longitude) + scale(Latitude)"))
  glm_model <- glm(glm_formula, 
                   data = EasternONLY, 
                   family = gaussian(link = "log"))
  
  # Get the model summary
  model_summary_EasternONLY <- summary(glm_model)
  
  # Extract the coefficients (Estimate, Std. Error, z-value, p-value) for the environmental factors
  coef_table <- model_summary_EasternONLY$coefficients
  
  # Store the results in the results data frame
  glm_results_EasternONLY <- rbind(glm_results_EasternONLY, data.frame(
    Frequency_Variable = freq_var,
    z_value_TreeCover = coef_table[2, 3],
    p_value_TreeCover = coef_table[2, 4],
    z_value_NL_StableLights_Avg = coef_table[3, 3],
    p_value_NL_StableLights_Avg = coef_table[3, 4],
    z_value_PopulationDensity = coef_table[4, 3],
    p_value_PopulationDensity = coef_table[4, 4],
    z_value_DistanceToRoad = coef_table[5, 3],
    p_value_DistanceToRoad = coef_table[5, 4],
    z_value_Elevation = coef_table[6, 3],
    p_value_Elevation = coef_table[6, 4],
    z_value_Longitude = coef_table[7, 3],
    p_value_Longitude = coef_table[7, 4],
    z_value_Latitude = coef_table[8, 3],
    p_value_Latitude = coef_table[8, 4]
  ))
}

# View the results
print(glm_results_EasternONLY)


# How many samples went into glm?
length(model_summary_EasternONLY[["deviance.resid"]]) #1781 total 

#write.csv(glm_results_EasternONLY, "~/GLM_Results_EasternONLY_10-20-25.csv", row.names = FALSE)








