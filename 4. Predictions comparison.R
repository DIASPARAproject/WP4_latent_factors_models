# ==============================================================================
#            PREDICTIONS ANALYSIS FOR TEMPORAL MODEL COMPARISON
# ==============================================================================
# Purpose: This script performs Kullback-Leibler divergence analysis to compare
# truncated vs complete model predictions for survival probability modeling.
#
# The KL divergence measures the information loss when using truncated model
# predictions instead of complete model predictions. This helps quantify
# the impact of data truncation on model performance and prediction accuracy.+
# Higher values indicate worse forecasting.
#
# Key concepts:
# - KL divergence: Mathematical measure of how one probability distribution 
#   differs from another
# - y_obs : Observed values
# - Temporal models: RE (Random Effects), RW (Random Walk), AR (Autoregressive)
# ==============================================================================

# ==============================================================================
# CONFIGURATION PARAMETERS
# ==============================================================================

# Time indices of the truncated years in the model (e.g., 49, 50, 51 = 2019-2021)
years_to_analyze   <- c(49, 50, 51)

# Corresponding calendar years (for display)
base_calendar_year <- 1971   # year 1 in the model = 1971

# ==============================================================================
# DATA PREPARATION AND KL DIVERGENCE INDICES
# ==============================================================================
# PURPOSE: This script prepares the raw MCMC (Markov Chain Monte Carlo) output 
# data for KL divergence analysis. It extracts and formats the x values 
# (expected survival probabilities) from the Bayesian model chains, converting 
# them from the raw MCMC matrix format into a structured, analysis-ready format.
#
# WHAT IT DOES:
# - Searches for x variables in the MCMC output using regex patterns
# - Extracts time-series data for specific time points (typically years 49, 50, 51)
# - Converts wide-format MCMC iterations into a clean data structure
# - Validates data integrity and handles missing values
#
# EXPECTED OUTPUT: x_summary_[model_type].csv - Contains extracted x values 
# in wide format where each row represents one MCMC iteration and each column 
# represents x[time, series]

mcmc_matrix    <- as.matrix(readRDS(file.path(path_results, "mcmc_samplesDFATRQ.rds")))
source(file.path(path_scripts, "KL_data_prep.R"))       # extracts x[t,s] samples

# ==============================================================================
# KL DIVERGENCE INDICES
# ==============================================================================
# PURPOSE: This is the core analytical script that calculates Kullback-Leibler 
# divergence between the complete and truncated model predictions. KL divergence 
# quantifies the information lost when using the truncated model instead of the 
# complete model. Higher values indicate greater prediction differences between models.
#
# MATHEMATICAL PROCESS:
# - For each time series and year combination, computes KL(Complete || Truncated)
# - Calculates mean differences to determine bias direction (over/underestimation)
# - Creates signed KL values where positive indicates overestimation, negative indicates underestimation
# - Processes multiple years (typically 2019, 2020, 2021) and series simultaneously
#
# EXPECTED OUTPUT: KL_[model_type].csv - Contains KL divergence values, bias 
# direction indicators, and series/year identifiers
# ==============================================================================

source(file.path(path_scripts, "KL_index_values.R"))   # computes KL vs y_obs
KL <- KL_results

# ==============================================================================
# CREATE COMPREHENSIVE MULTI-MODEL ANALYSIS
# ==============================================================================
# PURPOSE: Generates the final comprehensive visualization comparing KL divergences 
# across all three temporal models, multiple years, and all time series. This 
# creates a "heatmap" style plot that reveals patterns in how different modeling 
# approaches handle data truncation across various conditions.
#
# ANALYTICAL INSIGHTS:
# - Cross-model comparison: Shows which temporal model is most robust to truncation
# - Temporal patterns: Reveals if certain years are more affected by truncation
# - Series-specific effects: Identifies which time series are most sensitive to data limitation
# - Bias patterns: Visualizes systematic over/underestimation tendencies
#
# EXPECTED OUTPUT: 
# - High-resolution heatmap visualization showing KL divergences
# - Comprehensive multi-panel plot comparing RE, RW, and AR1 models
# - Statistical summary tables of cross-model performance differences
# ==============================================================================

source(file.path(path_scripts,"Plot_KL_3_years_latent_processes.R"))
