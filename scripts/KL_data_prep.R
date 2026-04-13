# ==============================================================================
# KL_data_prep_trunc.R
# ==============================================================================
# PURPOSE: Extract posterior samples of x[t,s] from the truncated model MCMC
# for the years of interest (years_to_analyze).
#
# Unlike the original KL_data_prep.R which extracted E_x values,
# here we extract x[t,s] — the latent state — which is what the truncated
# model predicts for the masked years.
#
# INPUT:
#   - mcmc_matrix       : MCMC samples as matrix (iterations × parameters)
#   - years_to_analyze  : integer vector of time indices (e.g. c(49, 50, 51))
#   - latent_process    : string identifier for file naming ("RE", "RW", "AR1")
#   - path_results      : output directory path
#
# OUTPUT:
#   - truncated_model_data : data.frame (iterations × x[t,s] columns)
#   - CSV saved to results/
# ==============================================================================

cat("Extracting x[t,s] samples for truncated years...\n")

# Build regex pattern matching x[t, s] for all years_to_analyze
year_pattern <- paste(years_to_analyze, collapse = "|")
x_pattern    <- paste0("^x\\[(", year_pattern, "), [0-9]+\\]$")

# Find matching columns in MCMC matrix
x_col_indices <- grep(x_pattern, colnames(mcmc_matrix))
x_col_names   <- colnames(mcmc_matrix)[x_col_indices]

# Validate
if (length(x_col_indices) == 0) {
  stop("ERROR: No x[t,s] columns found for years_to_analyze. ",
       "Check that 'x' is monitored in MCMC and years_to_analyze is correct.")
}

cat("✓ Found", length(x_col_indices), "x[t,s] columns\n")
cat("  Sample column names:", paste(head(x_col_names, 5), collapse = ", "), "\n")

# Extract as data.frame (each row = 1 MCMC iteration)
truncated_model_data <- as.data.frame(mcmc_matrix[, x_col_indices, drop = FALSE],
                                      check.names = FALSE)

cat("Data dimensions:", nrow(truncated_model_data), "iterations ×",
    ncol(truncated_model_data), "x[t,s] variables\n")

# Save to disk
filename_trunc <- paste0("x_trunc_", latent_process, ".csv")
write.csv(
  truncated_model_data,
  file      = file.path(path_results, filename_trunc),
  row.names = FALSE
)

cat("✓ Data saved to:", filename_trunc, "\n")