# ==============================================================================
# MODEL CONFIGURATION AND FILE NAMING
# ==============================================================================
# Determine which model type to analyze and create appropriate file names

if (analyze_truncated) {
  # Configuration for truncated model analysis
  truncation_suffix <- "T"
  model_type <- "truncated"
  cat("✓ Analyzing TRUNCATED model\n")
} else {
  # Configuration for complete model analysis  
  truncation_suffix <- ""
  model_type <- "complete"
  cat("✓ Analyzing COMPLETE model\n")
}

# Generate standardized filename based on latent process and model type
filename <- paste0("Ex_summary_", latent_process, truncation_suffix, ".csv")
filepath <- file.path(path_processed, filename)
cat("Output file:", filename, "\n\n")

# ==============================================================================
# STEP 1: EXTRACT E_x VALUES FROM MCMC MATRIX
# ==============================================================================
# E_x values represent expected values from Bayesian model sampling
# Format: E_x[time_point, series_number] - e.g., E_x[49, 1], E_x[50, 2]

cat("Searching for E_x variables in MCMC output...\n")

# Define regex pattern to match E_x columns for specific time points (49, 50, 51)
# This pattern captures: E_x[49,1], E_x[50,2], etc.
Ex_pattern <- "^E_x\\[(49|50|51), [0-9]+\\]$"

# Find all matching column indices in the MCMC matrix
Ex_col_indices <- grep(Ex_pattern, colnames(mcmc_matrix))
Ex_col_names <- colnames(mcmc_matrix)[Ex_col_indices]

# Validation: Check if E_x columns were found
if (length(Ex_col_indices) > 0) {
  cat("✓ Found", length(Ex_col_indices), "E_x columns matching pattern\n")
  
  # Extract E_x data while preserving MCMC iteration structure
  # Rows = MCMC iterations, Columns = E_x[t,s] variables
  Ex_wide_data <- mcmc_matrix[, Ex_col_indices, drop = FALSE]
  
  # Convert to data frame for easier manipulation
  Ex_wide_df <- as.data.frame(Ex_wide_data, check.names = FALSE)
  
  # Display extraction summary
  cat("Data dimensions:", nrow(Ex_wide_df), "iterations ×", 
      ncol(Ex_wide_df), "E_x variables\n")
  cat("Sample column names:", paste(head(colnames(Ex_wide_df), 5), collapse = ", "), "\n")
  
} else {
  stop("ERROR: No E_x columns found. Check MCMC matrix column naming convention.")
}
# ==============================================================================
# STEP 2: SAVE EXTRACTED DATA
# ==============================================================================
# Save wide format data for later analysis or sharing

cat("Saving wide format data...\n")
write.csv(
  Ex_wide_df,
  file = file.path(path_results, filename),
  row.names = FALSE
)

cat("✓ Data saved to:", file.path(path_results, filename), "\n")
cat("ℹ️  Format: Each row = 1 MCMC iteration, Each column = E_x[t,s]\n")

# Display data structure preview
cat("\n📋 Data structure preview:\n")
str(Ex_wide_df[1:3, 1:5])  # Show first 3 rows and 5 columns

cat("\nSummary statistics (first 5 variables):\n")
print(summary(Ex_wide_df[, 1:min(5, ncol(Ex_wide_df))]))