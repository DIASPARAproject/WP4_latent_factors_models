# ==============================================================================
#            PREDICTIONS ANALYSIS FOR TEMPORAL MODEL COMPARISON
# ==============================================================================
# Purpose: This script performs Kullback-Leibler divergence analysis to compare
# truncated vs complete model predictions for survival probability modeling.
#
# The KL divergence measures the information loss when using truncated model
# predictions instead of complete model predictions. This helps quantify
# the impact of data truncation on model performance and prediction accuracy.
#
# Key concepts:
# - KL divergence: Mathematical measure of how one probability distribution 
#   differs from another
# - y_obs : Observed values from MCMC (Markov Chain Monte Carlo) sampling
# - Temporal models: RE (Random Effects), RW (Random Walk), AR (Autoregressive)
# ==============================================================================

# ==============================================================================
# CONFIGURATION PARAMETERS for KULLBACK-LEIBLER
# ==============================================================================
# USER CONFIGURATION - MODIFY THESE VALUES
years_to_analyze <- c(49, 50, 51)   # Model years to analyze (corresponding to 2019, 2020, 2021 for example) 
latent_process <- "AR1"             # Specify the latent process of the model: "RE", "RW", or "AR"
analyze_truncated <- FALSE          # Set to TRUE to analyze truncated model (with NA values for n years), FALSE for complete model

# ==============================================================================
# DATA PREPARATION
# ==============================================================================
# PURPOSE: This script prepares the raw MCMC (Markov Chain Monte Carlo) output 
# data for KL divergence analysis. It extracts and formats the E_x values 
# (expected survival probabilities) from the Bayesian model chains, converting 
# them from the raw MCMC matrix format into a structured, analysis-ready format.
#
# WHAT IT DOES:
# - Searches for E_x variables in the MCMC output using regex patterns
# - Extracts time-series data for specific time points (typically years 49, 50, 51)
# - Converts wide-format MCMC iterations into a clean data structure
# - Validates data integrity and handles missing values
#
# EXPECTED OUTPUT: Ex_summary_[model_type].csv - Contains extracted E_x values 
# in wide format where each row represents one MCMC iteration and each column 
# represents E_x[time, series]
# ==============================================================================

source(file.path(path_scripts,"KL_data_prep.R"))

# ==============================================================================
# CALCULATE KL DIVERGENCE INDICES
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

complete_model_data <- read_csv("results/Ex_summary_AR1.csv")
truncated_model_data    <- read_csv("results/Ex_summary_AR1T.csv")

source(file.path(path_scripts,"KL_index_values.R"))

# ==============================================================================
# GENERATE COMPARISON VISUALIZATIONS
# ==============================================================================
# PURPOSE: Creates detailed comparison plots showing how complete and truncated 
# models perform for individual time series. These visualizations overlay model 
# predictions with observed data, allowing visual assessment of model accuracy 
# and the impact of data truncation on prediction quality.
#
# VISUALIZATION ELEMENTS:
# - Observed data: Historical time series as gray reference lines
# - Complete model: Blue lines with confidence bands showing full-data predictions
# - Truncated model: Red dashed lines with confidence bands showing limited-data predictions
# - Uncertainty quantification: Ribbon plots showing prediction intervals
#
# EXPECTED OUTPUT: Stored plot objects in plot_list_compare variable
# Individual comparison plots for each selected time series label
# ==============================================================================

Ex_summary_tot <- read_csv("results/Ex_summary_tot_AR1.csv")
Ex_summary_totT    <- read_csv("results/Ex_summary_tot_AR1T.csv")

source(file.path(path_scripts,"Plot_KL_1label_comp.R"))

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

KL_RE <- read_csv("results/KL_RE.csv")
KL_RW <- read_csv("results/KL_RW.csv")
KL_AR <- read_csv("results/KL_AR1.csv")

source(file.path(path_scripts,"Plot_KL_3_years_latent_processes.R"))
