################################################################################
#                       MCMC DIAGNOSTICS AND RESULTS EXPLORATION
################################################################################
#
# Script: Comprehensive MCMC convergence assessment and posterior analysis
# Author: Jeremy Bourdon (jeremy.bourdon@agrocampus-ouest.fr)  
# Date: September 2025
#
# Description:
# This script provides a comprehensive framework for assessing MCMC convergence,
# effective sample sizes, and posterior distributions from Bayesian state-space
# models. It includes automated parameter detection, visualization functions,
# and standardized diagnostic reporting.
#
# Key Features:
# - Automated Gelman-Rubin diagnostics with intelligent parameter grouping
# - Flexible visualization system with parameter-specific plots
# - Effective sample size calculations and reporting
# - Modular functions for easy customization and reuse
#
################################################################################

# ------------------------------
# CORE DIAGNOSTIC FUNCTIONS
# ------------------------------

#' Classify parameters into meaningful groups based on naming patterns
#'
#' @param param_names Character vector of parameter names
#' @return Character vector of parameter types
classify_parameters <- function(param_names) {
  
  dplyr::case_when(
    # State-space model components
    stringr::str_detect(param_names, "^x\\[|^y_") ~ "latent_states",
    stringr::str_detect(param_names, "^factor\\[") ~ "factors",
    stringr::str_detect(param_names, "^lambda\\[") ~ "loadings",
    
    # Variance parameters
    stringr::str_detect(param_names, "^sd_factor|^sigma_factor") ~ "factor_sd",
    stringr::str_detect(param_names, "^sd_x|^sigma_x") ~ "process_sd", 
    stringr::str_detect(param_names, "^sd_|^sigma_") ~ "other_sd",
    
    # Mean parameters
    stringr::str_detect(param_names, "^mu_x|^mean_x") ~ "level_means",
    stringr::str_detect(param_names, "^mu_|^mean_") ~ "other_means",
    
    # AR parameters
    stringr::str_detect(param_names, "^phi\\[|^phi_|^rho") ~ "autoregressive",
    
    # Observation model
    stringr::str_detect(param_names, "^y_pred|^y_obs") ~ "predictions",
    
    # Default classification
    TRUE ~ "other"
  )
}
