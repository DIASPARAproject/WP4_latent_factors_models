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

#' Extract numeric indices from parameter names for grouping
#'
#' @param param_names Character vector of parameter names  
#' @return Character vector with extracted indices (for grouping)
extract_parameter_indices <- function(param_names) {
  
  # Extract indices from patterns like param[i,j] or param[i]
  indices <- stringr::str_extract(param_names, "\\[([0-9,\\s]+)\\]")
  
  # Return cleaned indices or original parameter name if no indices found
  ifelse(is.na(indices), param_names, indices)
}
