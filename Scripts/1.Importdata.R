# 1. Import_data from lernzmp
# Explanation of this script ------------------------------------------------

# 
# 
#
#


# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

#Set working directory
setwd("~/Climate_kiwis/Data")

# Define the list of packages
packages <- c("trend","pak","tidyverse", "dplyr", "ggplot2","readxl", "writexl","readr")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})

# Install from GitHub
pak::pak("limnotrack/AEME")

# Import the data sets ---------------------------------------------------------
# Load metadata
metadata <- read.csv("Data_raw/lernzmp/LERNZmp_lake_metadata.csv")

metdata_data <- metadata %>%
  filter(Data != "No data")


# List all .rds files in the directory
rds_files <- list.files("Data_raw/lernzmp/", pattern = "\\.rds$", full.names = TRUE)

# Load AEME files dynamically
aeme_list <- lapply(rds_files, readRDS)

# Extract lake IDs from file names (assuming names are like "LID12345.rds")
lid_files <- gsub("Data_raw/lernzmp/|\\.rds$", "", rds_files)

# Model setup and directory path
model <- c("glm_aed", "gotm_wet")  # Models to build
path <- "aeme"  # Directory for model configuration

# Initialize an empty list to store detailed lake data
lake_data_list <- list()

for (i in seq_along(aeme_list)) {
  lid <- lid_files[i]
  aeme <- aeme_list[[i]]
  
  # Build and run the AEME object
  aeme <- build_aeme(aeme = aeme, model = model, path = path,
                     use_aeme = TRUE, use_bgc = TRUE)
  aeme <- run_aeme(aeme = aeme, model = model, path = path, parallel = TRUE)
  
  # Extract input data
  inp <- input(aeme)
  
  # Extract hypsograph data
  hyps <- inp$hypsograph |> dplyr::filter(depth <= 0)
  
  # Extract metadata for the lake
  lake_metadata <- metadata |> dplyr::filter(aeme_file == lid)
  
  # Prepare a list for all extracted data for this lake
  lake_details <- list(
    LID = lid,
    lake_name = lake_metadata$Name,
    region = lake_metadata$Region,
    geomorphic_type = lake_metadata$Geomorphic.type,
    max_depth = lake_metadata$Depth,
    depth_measurement = lake_metadata$Depth.measurement,
    Area = lake_metadata$Area,
    data_quality = lake_metadata$Data,
    init_depth = inp$init_depth,
    light_attenuation_coefficient = inp$Kw,
    use_longwave_radiation = inp$use_lw,
    hypsograph = hyps,  # Detailed hypsograph data
    init_profile = inp$init_profile,  # Initial conditions (depth, temperature, salinity)
    meteo = inp$meteo  # Meteorological data
  )
  
  # Add lake details to the list
  lake_data_list[[lid]] <- lake_details
}

# Combine all extracted data into a single structured object
all_lake_data <- do.call(rbind, lapply(lake_data_list, function(x) {
  data.frame(
    LID = x$LID,
    lake_name = x$lake_name,
    region = x$region,
    geomorphic_type = x$geomorphic_type,
    max_depth = x$max_depth,
    depth_measurement = x$depth_measurement,
    data_quality = x$data_quality,
    init_depth = x$init_depth,
    light_attenuation_coefficient = x$light_attenuation_coefficient,
    use_longwave_radiation = x$use_longwave_radiation,
    hypsograph = I(list(x$hypsograph)),  # Store hypsograph as a nested dataframe
    init_profile = I(list(x$init_profile)),  # Store initial profile as a nested dataframe
    meteo = I(list(x$meteo))  # Store meteorological data as a nested dataframe
  )
}))

# Save the structured data as an RDS file
saveRDS(all_lake_data, "Data_mod/all_lake_data.rds")

# Water quality data -----------------------------------------------------------

# Initialize an empty data frame to store all water quality data
water_quality_data <- data.frame()

# Loop through each AEME object in the aeme_list
for (i in seq_along(aeme_list)) {
  # Extract the current AEME object
  aeme <- aeme_list[[i]]
  
  # Extract observations related to lake water quality
  lake_observations <- aeme@observations[["lake"]]
  
  # Get metadata for the current lake (if available)
  lid <- lid_files[i]
  lake_metadata <- metadata |> dplyr::filter(aeme_file == lid)
  
  # Process water quality data
  processed_data <- lake_observations |> 
    dplyr::select(Date, depth_mid, var_aeme, value) |>  # Select relevant columns
    mutate(
      lake_name = lake_metadata$Name,
      region = lake_metadata$Region,
      geomorphic_type = lake_metadata$Geomorphic.type,
      max_depth = lake_metadata$Depth,
      depth_measurement = lake_metadata$Depth.measurement,
      data_quality = lake_metadata$Data
    )
  
  # Append processed data to the combined dataframe
  water_quality_data <- rbind(water_quality_data, processed_data)
}

# Summarize or filter the data (e.g., mean values by variable and lake)
water_quality_summary <- water_quality_data |> 
  group_by(lake_name, var_aeme) |> 
  summarize(
    mean_value = mean(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    min_value = min(value, na.rm = TRUE),
    .groups = "drop"
  )

# Save the combined water quality data to a CSV file
write.csv(water_quality_data, "Data_mod/water_quality_data.csv", row.names = FALSE)
write.csv(water_quality_summary, "Data_mod/water_quality_summary.csv", row.names = FALSE)

# Load the saved data for further use
water_quality_data <- read_csv("Data_mod/water_quality_data.csv")
water_quality_summary <- read_csv("Data_mod/water_quality_summary.csv")
