################################################################################
#                    BAYESIAN TIME SERIES ANALYSIS WITH NIMBLE
#                      PART 1.5: DATA PROCESSING & PREPARATION
################################################################################
#
#     IF YOU ALREADY HAVE YOUR DATA FILES ; SKIP TO PART 2.SCRIPT_MODEL
#
# Script: Data import, processing, and preparation for Bayesian state-space modeling
# Author: Jeremy Bourdon (jeremy.bourdon@agrocampus-ouest.fr)
# Date: September 2025
# 
# Description:
# This is the second part of the preprocessing pipeline for Bayesian time series
# analysis. This script handles the actual data processing tasks including:
# - Importing and merging multiple data files
# - Extracting time series values and uncertainty measures
# - Computing observational uncertainties for Bayesian modeling
# - Creating visualization plots for data exploration
# - Preparing final datasets for NIMBLE analysis
#
# Prerequisites:
# - Run "Part 1: Project Setup" script first
# - Place data files in the Table/ directory following the required structure
# - Ensure data files follow the expected naming conventions (e.g., "PSS*.txt")
#
# Input Data Requirements:
# - Multiple CSV/TXT files with time series data
# - Files should contain: year, median values, uncertainty bounds (q5, q95)
# - Column naming convention: regioncode_median, regioncode_q5, regioncode_q95
#
# Output:
# - Processed time series matrix (years ×  labels)
# - Uncertainty matrices (q5 and q95 bounds)
# - Observational error estimates (sigma_obs)
# - Exploratory data visualization plots
# - Summary statistics and data diagnostics
#
################################################################################

# ------------------------------
# STUDY SYSTEM DEFINITION
# ------------------------------

#' Define population  labels
#' 
#' This analysis covers x distinct populations across 
#' the specific region.

regions <- c("LB", "NF", "QC", "GF", "SF", "US", "FR", "EW", "IR", "NI_FO", "NI_FB", 
               "SC_WE", "SC_EA", "IC_SW", "IC_NE", "SW", "NO_SE", "NO_SW", "NO_MI", 
               "NO_NO", "FI", "RU_KB", "RU_KW", "RU_AK", "RU_RP")

cat("Analysis configured for", length(regions), "\n")

labels <- regions

# ------------------------------
# DATA IMPORT AND PROCESSING
# ------------------------------

# -------------
# DATA IMPORT
# -------------

############################################################
# DATA IMPORT SECTION
#
# Purpose:
#   This section automatically finds and loads all data files
#   that match a specific naming pattern from the Table/ directory.
#   All matching files are then merged together based on a common
#   column (typically "year" for time series data).
#
# How it works:
#   1. Define a filename pattern to search for
#   2. Find all files matching this pattern
#   3. Load and merge all files into a single dataset
#   4. Handle missing data and inconsistencies
#
# File naming convention:
#   Files should start with "PSS" and end with ".txt"
#   Example: "PSS_data_2024.txt", "PSS_regional_estimates.txt"
#   
# Expected file structure:
#   Each file should contain:
#   - A "year" column (for merging)
#   - Region-specific columns with median values, uncertainties
#   - Column names like: "LB_median", "LB_q5", "LB_q95", etc.
############################################################

# Define the filename pattern to search for : 
#    -> Here, we look for all text files starting with "PSS"
#    -> Adjust the pattern if your file names differ

pattern <- "PSS.*\\.txt$"

# List all files matching the pattern inside the folder
files <- list.files(path = path_data, pattern = pattern, full.names = TRUE)

# Load the data import function and execute the merging process
# This function will:
# - Read each file individually
# - Check for common columns (especially "year")
# - Merge all files into a single dataset
# - Handle missing data and column mismatches
# - Report any issues or inconsistencies
# - file output from script: merged_table

source(file.path(path_scripts,"data_import.R"))

# -------------
# PROCESSING
# -------------

############################################################
# TIME SERIES EXTRACTION SECTION
#
# Purpose:
#   This section extracts the main time series values from the
#   merged dataset and organizes them according to the defined
#   regional order. This creates the primary data matrix for
#   Bayesian state-space modeling.
#
# Process:
#   1. Identify columns containing median/central estimates
#   2. Extract these columns and arrange by region order
#   3. Handle missing data and data quality issues
#   4. Create properly formatted matrices for analysis
#
# Column naming convention:
#   Columns ending with "_median" contain the central estimates
#   for each region. For example:
#   - "LB_median"  = median estimate for Labrador region
#   - "NF_median"  = median estimate for Newfoundland region
#   - etc.
############################################################

# Identify all columns ending with "_median" (or something else)
#    -> These represent the median values for each region
median_cols <- grep("_median$", colnames(merged_table), value = TRUE)

# Load the data processing function
# This function will:
# - Extract median values for each region
# - Arrange columns according to the defined region order
# - Handle missing labels or data gaps
# - Create clean time series matrix
# - Apply any necessary data transformations
# - file output from script: time_series (matrix used for model)

source(file.path(path_scripts,"data_process.R"))
time_series <- time_series
# Create dataset with year for plotting
ts_with_year <- cbind(year = merged_table$year, time_series)

############################################################
# UNCERTAINTY EXTRACTION SECTION
#
# Purpose:
#   This section extracts uncertainty bounds (confidence intervals)
#   for each region from the merged dataset. These bounds are
#   essential for Bayesian modeling as they quantify observation
#   uncertainty and measurement error.
#
# Uncertainty representation:
#   Data should include both lower and upper bounds of uncertainty:
#   - q5 columns  = 5th percentile (lower bound)
#   - q95 columns = 95th percentile (upper bound)
#   Together, these form 90% credible intervals
#
# Column naming convention:
#   - "LB_q5"  = 5th percentile for Labrador
#   - "LB_q95" = 95th percentile for Labrador
#   - Similar pattern for all  labels
#
# Note: This section can be skipped if uncertainty data is not available,
#       but this will limit the sophistication of the Bayesian model.
############################################################

# Load uncertainty extraction function
# This function will:
# - Extract q5 and q95 columns for each region
# - Arrange them according to region order
# - Check for consistency between bounds (q5 < q95)
# - Handle missing uncertainty data
# - Create uncertainty matrices for Bayesian modeling
# - file output from script: uncertainty_bounds (list)

source(file.path(path_scripts,"uncertainty.R"))

############################################################
# OBSERVATIONAL UNCERTAINTY COMPUTATION SECTION
#
# Purpose:
#   This section estimates observational standard deviations
#   (sigma_obs) from the credible intervals. These uncertainty
#   estimates are crucial for Bayesian state-space models as
#   they represent measurement error and observation uncertainty.
#
# Statistical background:
#   If we assume that observations follow a normal distribution,
#   we can convert credible intervals to standard deviations using:
#   
#   sigma = (q95 - q5) / (z_0.95 - z_0.05)
#   
#   Where:
#   - q5, q95 = 5th and 95th percentiles (our uncertainty bounds)
#   - z_p = quantile of standard normal distribution
#   - z_0.05 ≈ -1.645, z_0.95 ≈ +1.645
#   - Therefore: sigma ≈ (q95 - q5) / 3.29
#
# Usage in Bayesian models:
#   The computed sigma_obs values will be used as observation
#   error parameters in the state-space model, allowing the
#   model to account for measurement uncertainty.
#
# Note: This computation assumes normally distributed errors.
#       Alternative distributions can be used if more appropriate.
############################################################

# Load sigma computation function
# This function will:
# - Calculate sigma_obs for each region and time point
# - Handle cases where q5 >= q95 (invalid intervals)
# - Check for reasonable uncertainty magnitudes
# - Create sigma_obs matrix for Bayesian modeling
# - Provide diagnostic summaries of uncertainty patterns
# - Expect output from script : Sigma_obs (uncertainty matrix for model)

source(file.path(path_scripts,"sigma.R"))

# ------------------------------
# DATA VISUALIZATION
# ------------------------------

############################################################
# DATA VISUALIZATION SECTION
#
# Purpose:
#   Create comprehensive plots to explore the time series data
#   before proceeding with Bayesian modeling. This includes:
#   - Time series plots for each region
#   - Data transformation plots (original vs logit scale)
#   - Missing data patterns
#   - Correlation structure between  labels
#
# Plot types generated:
#   Data on original scale vs transformed scale
#
# Output location:
#   All plots will be saved in the results/ directory
############################################################

# Load the plotting function
# This comprehensive function will create:
# - Time series overview plots
# - Scale comparison plots (original vs logit)

source(file.path(path_scripts,"data_pre_plot.R"))

# ------------------------------
# ANALYSIS SUMMARY
# ------------------------------

source(file.path(path_scripts,"summary_pre.R"))

# Export time series matrix with proper row and column names
if (exists("time_series")) {
  ts_csv_file <- file.path(path_processed, "time_series_matrix.csv")
  ts_for_export <- as.data.frame(time_series)
  colnames(ts_for_export) <-  labels  # Use region names as column headers
  rownames(ts_for_export) <- merged_table$year    # Use years as row names

  # Write CSV with row names (years) as first column
  write.csv(ts_for_export, file = ts_csv_file, row.names = TRUE)
  cat("✓ Time series matrix saved to:", basename(ts_csv_file), "\n")
} else {
  cat("• time_series matrix not available - skipping CSV export\n")
}
# Export sigma_obs matrix if available
if (exists("Sigma_obs")) {
  sigma_csv_file <- file.path(path_processed, "sigma_obs_matrix.csv")
  sigma_for_export <- as.data.frame(Sigma_obs)
  colnames(sigma_for_export) <-  labels  # Use region names as column headers  
  rownames(sigma_for_export) <- merged_table$year    # Use years as row names
  
  # Write CSV with row names (years) as first column
  write.csv(sigma_for_export, file = sigma_csv_file, row.names = TRUE)
  cat("✓ Observational uncertainties (sigma_obs) saved to:", basename(sigma_csv_file), "\n")
} else {
  cat("• Sigma_obs matrix not available - skipping CSV export\n")
}

################################################################################
# END OF PRE-PROCESSING PART
################################################################################
