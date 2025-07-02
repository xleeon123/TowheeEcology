################################################################################
################################################################################
######################## Generalized Linear Models #############################
################################################################################
################################################################################

################################################################################
############################ Species Combined ##################################
################################################################################
# Read in environmental data
All_TowheeEcology_Data <- read.csv("~/PUBLISH_FINAL_SongData_wEcologyClimateUrbanizationElevation_6.6.25_XL.csv")
All_TowheeEcology_Data <- All_TowheeEcology_Data[,c(1,3,5:14,15,18:22,28)]

# Define the frequency columns 
frequency_columns <- 5:12

# Initialize an empty data frame to store the results
glm_results <- data.frame()

# Loop through each frequency variable (columns 7 to 14)
for (freq_col in frequency_columns) {
  
  # Get the name of the frequency variable (e.g., min_sylls_freq_Hz, etc.)
  freq_var <- colnames(All_TowheeEcology_Data)[freq_col]
  
  # Fit the GLM model for the current frequency variable
  glm_formula <- as.formula(paste(freq_var, "~ scale(TreeCover) + scale(NL_StableLights_Avg) + scale(PopulationDensity) + scale(DistanceToRoad_km) + scale(Temperature) + scale(Humidity) + factor(Species) + scale(Elevation_m) + scale(Longitude) + scale(Latitude)"))
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
    z_value_Temperature = coef_table[6, 3],
    p_value_Temperature = coef_table[6, 4],
    z_value_Humidity = coef_table[7, 3],
    p_value_Humidity = coef_table[7, 4],
    z_value_Species = coef_table[8, 3],
    p_value_Species = coef_table[8, 4],
    z_value_Elevation = coef_table[9, 3],
    p_value_Elevation = coef_table[9, 4],
    z_value_Longitude = coef_table[10, 3],
    p_value_Longitude = coef_table[10, 4],
    z_value_Latitude = coef_table[11, 3],
    p_value_Latitude = coef_table[11, 4]
  ))
}

# View the results
print(glm_results)


# How many samples went into glm?
length(model_summary[["deviance.resid"]]) #2463 total 

#write.csv(glm_results, "~/GLMResults.csv", row.names = FALSE)

################################################################################
############################## Spotted ONLY ####################################
################################################################################
# Read in environmental data
All_TowheeEcology_Data <- read.csv("~/PUBLISH_FINAL_SongData_wEcologyClimateUrbanizationElevation_6.6.25_XL.csv")
All_TowheeEcology_Data <- All_TowheeEcology_Data[,c(1,3,5:14,15,18:22,28)]

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
  glm_formula <- as.formula(paste(freq_var, "~ scale(TreeCover) + scale(NL_StableLights_Avg) + scale(PopulationDensity) + scale(DistanceToRoad_km) + scale(Temperature) + scale(Humidity) + scale(Elevation_Km) + scale(Longitude) + scale(Latitude)"))
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
    z_value_Temperature = coef_table[6, 3],
    p_value_Temperature = coef_table[6, 4],
    z_value_Humidity = coef_table[7, 3],
    p_value_Humidity = coef_table[7, 4],
    z_value_Elevation = coef_table[8, 3],
    p_value_Elevation = coef_table[8, 4],
    z_value_Longitude = coef_table[9, 3],
    p_value_Longitude = coef_table[9, 4],
    z_value_Latitude = coef_table[10, 3],
    p_value_Latitude = coef_table[10, 4]
  ))
}

# View the results
print(glm_results_SpottedONLY)


# How many samples went into glm?
length(model_summary_SpottedONLY[["deviance.resid"]]) #898 total 

#write.csv(GLM_Results_SpottedONLY, "~/glm_results_SpottedONLY.csv", row.names = FALSE)

################################################################################
############################## Eastern ONLY ####################################
################################################################################
# Read in environmental data
All_TowheeEcology_Data <- read.csv("~/PUBLISH_FINAL_SongData_wEcologyClimateUrbanizationElevation_6.6.25_XL.csv")
All_TowheeEcology_Data <- All_TowheeEcology_Data[,c(1,3,5:14,15,18:22,28)]

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
  glm_formula <- as.formula(paste(freq_var, "~ scale(TreeCover) + scale(NL_StableLights_Avg) + scale(PopulationDensity) + scale(DistanceToRoad_km) + scale(Temperature) + scale(Humidity) + scale(Elevation_Km) + scale(Longitude) + scale(Latitude)"))
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
    z_value_Temperature = coef_table[6, 3],
    p_value_Temperature = coef_table[6, 4],
    z_value_Humidity = coef_table[7, 3],
    p_value_Humidity = coef_table[7, 4],
    z_value_Elevation = coef_table[8, 3],
    p_value_Elevation = coef_table[8, 4],
    z_value_Longitude = coef_table[9, 3],
    p_value_Longitude = coef_table[9, 4],
    z_value_Latitude = coef_table[10, 3],
    p_value_Latitude = coef_table[10, 4]
  ))
}

# View the results
print(glm_results_EasternONLY)


# How many samples went into glm?
length(model_summary_EasternONLY[["deviance.resid"]]) #1565 total 

#write.csv(GLM_Results_EasternONLY, "~/glm_results_EasternONLY.csv", row.names = FALSE)