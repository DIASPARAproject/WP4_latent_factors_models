# Check if any files were found
if (length(files) == 0) {
  stop("No files found matching pattern: ", pattern, 
       " in path: ", path_data, 
       "\nPlease check your data folder or file names.")
}

cat("Loading", length(files), "CSV/TXT files...\n")

# Load each file into a list
#    -> read.csv() is used here, works also for .txt if comma/semicolon-separated
#    -> stringsAsFactors = FALSE prevents automatic factor conversion
data_list <- lapply(files, function(file) {
  cat("  - Loading:", basename(file), "\n")
  read.csv(file, stringsAsFactors = FALSE)
})

# Merge all datasets together
#    -> We assume all files contain a common column "year"
#    -> full_join() keeps all years, even if missing in some files
merged_table <- Reduce(function(x, y) {
  full_join(x, y, by = "year", suffix = c("", ""))
}, data_list)

# Final check and message
cat("uccessfully merged", length(files), "files with", nrow(merged_table), "rows.\n")

# Validate data structure
required_cols <- c("year")  # At minimum, need year column
missing_cols <- setdiff(required_cols, colnames(merged_table))
if (length(missing_cols) > 0) {
  stop("Missing required columns in data: ", paste(missing_cols, collapse = ", "))
}

cat("Raw data dimensions:", nrow(merged_table), "rows ×", ncol(merged_table), "columns\n")
cat("Time series span:", min(merged_table$year), "to", max(merged_table$year), "\n")
