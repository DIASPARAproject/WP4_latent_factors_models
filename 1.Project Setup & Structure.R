################################################################################
#                    BAYESIAN TIME SERIES ANALYSIS WITH NIMBLE
#                          PART 1: PROJECT SETUP & STRUCTURE
################################################################################
#
# Script: Project initialization and structure setup for Bayesian state-space modeling
# Author: Jeremy Bourdon (jeremy.bourdon@agrocampus-ouest.fr)
# Date: September 2025
# 
# Description:
# This script sets up the project structure and environment for Bayesian 
# time series analysis using the NIMBLE package. It handles package management,
# directory creation, and function loading.
#
# Expected Data Structure:
# The analysis requires three main datasets that should be placed in the Table/ directory:
#
# 1. VALUES MATRIX (e.g., "data_values.csv" or "data_values.txt"):
#    - Rows: Years/Time points
#    - Columns: Different time series (species/regions/populations)
#    - Contains the observed values for each series at each time point
#    - Format: numeric matrix with NA allowed for missing observations
#
# 2. UNCERTAINTIES MATRIX (e.g., "data_uncertainties.csv"):
#    - Same dimensions as values matrix (years x series)
#    - Contains uncertainty estimates (standard errors, confidence intervals, etc.)
#    - Format: numeric matrix, positive values
#    - Should correspond exactly to the values matrix structure
#
# 3. SERIES NAMES (e.g., "series_names.csv" or as column headers):
#    - Vector containing names/labels for each time series
#    - Examples: species names, region names, population identifiers
#    - Length should match the number of columns in the data matrices
#    - Used for labeling plots and results
#
# Data can be in logit-scale or original scale depending on the analysis needs.
################################################################################

# Clear workspace for fresh analysis
rm(list = ls())
gc()  # garbage collection for memory management

# ------------------------------
# PACKAGE MANAGEMENT
# ------------------------------

# Load and attach required packages for the analysis
# Includes Bayesian modeling, data manipulation, and visualization tools
required_packages <- c(
  "nimble","nimbleHMC","coda","dplyr","tidyr","ggplot2","plotly","here","stringr","parallel","MASS","reshape2",
  "readr","FNN","gridExtra","stringr","patchwork","scales","tibble","NbClust","cluster","dendextend","RColorBrewer")

# Check for missing packages and install if necessary
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, dependencies = TRUE)
}

# Load all required packages
invisible(lapply(required_packages, library, character.only = TRUE, quietly = TRUE))


# Save default plotting parameters for restoration if needed
default_par <- par(no.readonly = TRUE)

# ------------------------------
# PROJECT STRUCTURE SETUP
# ------------------------------

#' Define project directory structure using relative paths
#' This approach ensures portability across different systems and users
#' 
#' Expected project structure:
#' project_root/
#' ├── script/          
#' │   ├── preprocessing.R    (this file or another .R)
#' ├── function/              (custom R functions)
#' │   ├── f.panel.cor.R      (for example)
#' │   ├── f.panel.dens.R
#' │   ├── f.density.bivar.R
#' │   └── ....
#' ├── Table/                 (raw data files)
#' │   └── data.txt
#' ├── results/               (summaries and plots files)
#' │   ├── plot gelman
#' │   ├── plot ess
#' │   └── plot ...
#' ├── data/                  (processed data for NIMBLE and Kullback-Leibler)
#' │   ├── data.rds
#' │   ├── const.rds
#' │   └── ....
#' └── model/                 (NIMBLE model definitions)
#      └── DFAbayesian1AR1.txt

# Set project root directory
project_root <- here::here()
cat("Project root directory:", project_root, "\n")

# Define subdirectory paths relative to the project root
# --> Each variable represents a folder where different 
#     types of files will be stored.
path_scripts <- file.path(project_root, "scripts")      # For R scripts
path_functions <- file.path(project_root, "function")   # For custom functions
path_data      <- file.path(project_root, "Table")      # For raw data tables (CSV, Excel, etc.)
path_processed <- file.path(project_root, "data")       # For cleaned or processed data
path_models    <- file.path(project_root, "model")      # For model files or scripts
path_results   <- file.path(project_root, "results")    # For outputs, figures, tables, reports

# Collect all critical directories in a vector
# --> These are the folders that must exist for the project to run correctly
required_dirs <- c(path_scripts, path_functions, path_data, path_processed, path_models, path_results)

# Check if each required directory exists
# --> If not, the script will create it automatically.
# --> A message is printed for each directory to inform the user.
for (dir_path in required_dirs) {
  
  if (!dir.exists(dir_path)) {
    # Case 1: Directory does NOT exist
    message("Directory missing. Creating: ", dir_path)
    
    # dir.create() creates the folder
    # recursive = TRUE ensures that any parent folders are also created if needed
    dir.create(dir_path, recursive = TRUE)
    
  } else {
    # Case 2: Directory already exists
    message("✓ Directory already exists: ", dir_path)
  }
}

# ------------------------------
# LOAD CUSTOM FUNCTIONS
# ------------------------------

#' Source custom plotting and analysis functions
#' These functions provide specialized correlation panels and density plots
#' for posterior distribution visualization

source(file.path(path_scripts,"function_setup.R"))
