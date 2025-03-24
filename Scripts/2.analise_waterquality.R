# 2. Analyse waterquality from lernzmp
# Explanation of this script ------------------------------------------------

# 
# 
#
#


# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

#Set working directory
setwd("~/Climate_kiwis/Data/Climate_kiwis")

# Define the list of packages
packages <- c("trend","pak","tidyverse", "dplyr", "ggplot2","readxl", "writexl","readr")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})


# Import the data sets ---------------------------------------------------------
# Load metadata
metadata <- read.csv("Data_raw/lernzmp/LERNZmp_lake_metadata.csv")

water_quality_data <- read_csv("Data_mod/water_quality_data.csv")
water_quality_summary <- read_csv("Data_mod/water_quality_summary.csv")

# Clean data -------------------------------------------------------------------
# use only Temp data (HYD_temp)
# Filter for HYD_temp, valid values, and surface temperatures
surface_temp_data <- water_quality_data %>%
  filter(var_aeme == "HYD_temp", value <= 37.5, depth_mid %in% c(0, 1)) %>% 
  mutate(year_month = format(Date, "%Y-%m"))

# plot raw data
ggplot(surface_temp_data, aes(as.Date(paste0(year_month, "-01")),value, col= lake_name)) +
  geom_point() +
  theme(legend.position = "none")

# Calculate the mean value for each month-year and lake
Temp_data_summary <- surface_temp_data %>%
  group_by(year_month, lake_name) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

Temp_data_summary <- surface_temp_data %>%
  group_by(year_month, lake_name) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    Date = first(Date),
    region = first(region),  # Use the first occurrence for consistent values
    geomorphic_type = first(geomorphic_type),
    .groups = "drop"
  )

# plot raw data summery
ggplot(Temp_data_summary, aes(as.Date(paste0(year_month, "-01")),mean_value, col= lake_name)) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed",col="black", se = FALSE) +
  theme(legend.position = "none")

# Calculate the number of data points and the time range for each lake
lake_stats <- Temp_data_summary %>%
  group_by(lake_name) %>%
  summarise(
    data_points = n(),                            # Count the number of data points
    time_range_start = min(Date, na.rm = TRUE),   # Find the earliest date
    time_range_end = max(Date, na.rm = TRUE)) %>% # Find the latest date
  ungroup()

# Calculate the time range in years and filter lakes with more than 15 years of data
lake_stats_filtered <- lake_stats %>%
  mutate(
    time_range_years = as.numeric(difftime(time_range_end, time_range_start, units = "days")) / 365.25,
    total_months = as.numeric(difftime(time_range_end, time_range_start, units = "days")) / 30.4375,
    expected_data_points = ceiling(total_months)) %>%
  filter(time_range_years >= 15)

# Filter Temp_data to include only the lakes with 15+ years of data
Temp_data_filtered <- Temp_data_summary %>%
  filter(lake_name %in% lake_stats_filtered$lake_name)%>%
  left_join(lake_stats_filtered, by = "lake_name")

# Add data points to the lake name
Temp_data_filtered <- Temp_data_filtered %>%
  mutate(lake_label = paste0(lake_name, " (n = ", data_points, ")"))

# Filter out lakes with irregular datasets
Temp_data_filtered <- Temp_data_filtered %>%
  filter(!lake_name %in% c("Horowhenua", "Lake Dudding", "Mangahia", "Ohinewai"))

# Plot with updated labels
ggplot(Temp_data_filtered, aes(as.Date(paste0(year_month, "-01")), mean_value, col = region)) +
  geom_line() +
  geom_smooth(method = "lm", linetype = "dashed", color = "red", se = FALSE) +
  facet_wrap(region~lake_label, scales = "free_y") +
  theme_bw()

# calculate slopes -------------------------------------------------------------
# Calculate slopes for each lake
lm_slopes_data <- Temp_data_filtered %>%
  group_by(lake_name) %>%
  summarise(
    slope = coef(lm(mean_value ~ as.Date(paste0(year_month, "-01"))))[2], 
    intercept = coef(lm(mean_value ~ as.Date(paste0(year_month, "-01"))))[1],
    r_squared = summary(lm(mean_value ~ as.Date(paste0(year_month, "-01"))))$r.squared,
    .groups = "drop")

# Calculate the mean slope and intercept across all lakes
lm_mean_values <- lm_slopes_data %>%
  summarise(
    mean_slope = mean(slope),
    mean_intercept = mean(intercept),
    mean_r_squared = mean(r_squared))


# Calculate slope trend --------------------------------------------------------
# Compute Sen's slope and p-value for each lake_name 
sen_slopes_data <- Temp_data_filtered %>%
  group_by(lake_name) %>%
  summarise(
    sen_slope = sens.slope(mean_value)$estimates,
    sen_signif = sens.slope(mean_value)$p.value,
    median_temp = median(mean_value),
    intercept = median_temp - (sen_slope*median_temp),
    .groups = "drop")

sen_mean_values <- sen_slopes_data %>%
  summarise(
    mean_slope = mean(sen_slope),
    mean_signif = mean(sen_signif))

# Adding a column for the Sen's slope for each lake
Temp_data_filtered_sen <- Temp_data_filtered %>%
  left_join(sen_slopes_data, by = "lake_name")

# Plot the data and add Sen's slope line with intercept
ggplot(Temp_data_filtered_sen, aes(as.Date(paste0(year_month, "-01")), mean_value, col = region)) +
  geom_line() + 
  geom_abline(aes(slope = sen_slope, intercept = intercept), 
              linetype = "dashed", color = "red") +  
  facet_wrap(region~lake_label, scales = "free_y") +
  theme_bw()

# Calculate trend lines for Sen's slope
trend_lines <- Temp_data_filtered_sen %>%
  group_by(lake_name) %>%
  mutate(trend = intercept + sen_slope * as.numeric(as.Date(paste0(year_month, "-01"))))

# Plot the data and add Sen's slope trend lines
ggplot(Temp_data_filtered_sen, aes(as.Date(paste0(year_month, "-01")), mean_value, col = region)) +
  geom_line() +  # Original data lines
  geom_line(data = trend_lines, aes(y = trend), linetype = "dashed", color = "red") +  # Sen's slope trend lines
  facet_wrap(region~lake_label, scales = "free_y") +
  theme_bw()


# combine LM and SEN in one DF
names(Temp_data_filtered_sen)
names(lm_slopes_data)

combined_data <- Temp_data_filtered_sen %>%
  distinct(lake_name, .keep_all = TRUE) %>% 
  select(lake_name, region, geomorphic_type,time_range_start, time_range_end, time_range_years,total_months, mean_value, median_temp,sen_slope, sen_signif) %>%
  left_join(lm_slopes_data %>%rename(lm_slope = slope,lm_intercept = intercept,lm_r_squared = r_squared),by = "lake_name")




