cat("Extracting uncertainty intervals (5th and 95th percentiles)...\n")

# Identify all columns that contain quantiles
#    -> Columns should end with "_q5" or "_q95"
q_cols <- grep("_q[0-9]+$", colnames(merged_table), value = TRUE)

# Check if quantile columns exist
if (length(q_cols) == 0) {
  stop("No quantile columns were found in the dataset.
       Expected column names like 'region_q5' or 'region_q95'.")
}

cat("Found", length(q_cols), "quantile columns in the dataset\n")

# Extract region codes from column names
#    -> Remove the suffix (_q5, _q95) to get the region identifiers
region_codes_q <- sub("_q[0-9]+$", "", q_cols)

# Order quantile columns according to the "labels" sequence
#    -> Ensures that all labels are aligned consistently
q_cols_sorted <- q_cols[order(match(region_codes_q, labels))]

# Remove any NA values in case some labels were missing
q_cols_sorted <- q_cols_sorted[!is.na(q_cols_sorted)]

# Select only the ordered quantile columns from the dataset
q_data <- merged_table[, q_cols_sorted, drop = FALSE]

# Separate lower (5th percentile) and upper (95th percentile) columns
q5_cols  <- grep("_q5$", colnames(q_data), value = TRUE)
q95_cols <- grep("_q95$", colnames(q_data), value = TRUE)

# Convert the data frames into matrices for numerical modeling
q5  <- as.matrix(q_data[, q5_cols, drop = FALSE])
q95 <- as.matrix(q_data[, q95_cols, drop = FALSE])

# Validate dimensions (should match for q5 and q95)
if (ncol(q5) != ncol(q95)) {
  warning("Number of q5 and q95 columns differ: ",
          ncol(q5), " vs ", ncol(q95))
}

# Final confirmation message
cat("Extracted uncertainty bounds for", ncol(q5),
    "labels with", nrow(q5), "time points\n")

# Store results in a list (optional, for convenience)
uncertainty_bounds <- list(q5 = q5, q95 = q95)