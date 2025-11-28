# Check if any median columns exist
if (length(median_cols) == 0) {
  stop("No columns ending with '_median' were found in the dataset. 
       Please check your input data.")
}

cat("Found", length(median_cols), "median columns in the dataset\n")

# Extract region codes from column names (specific to some datasets)
#    -> Remove the "_median" suffix to get region identifiers
region_codes <- sub("_median$", "", median_cols)

# Check that all specified labels are present in the data
missing_labels <- setdiff(labels, region_codes)
if (length(missing_labels) > 0) {
  warning("Some labels were not found in the dataset: ",
          paste(missing_labels, collapse = ", "))
}

# Order columns according to the specified sequence in "labels"
#    -> Ensures consistent regional ordering across model runs
median_cols_sorted <- median_cols[order(match(region_codes, labels))]

# Remove any NA values in case some labels are missing
median_cols_sorted <- median_cols_sorted[!is.na(median_cols_sorted)]

# Extract only the ordered median columns
time_series <- merged_table[, median_cols_sorted, drop = FALSE]

cat("Prepared time series with", ncol(time_series), "labels and",
    nrow(time_series), "time points\n")
