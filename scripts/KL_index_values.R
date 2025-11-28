# ==============================================================================
# STEP 3: KULLBACK-LEIBLER DIVERGENCE CALCULATION
# ==============================================================================
# Calculate KL divergence between complete and truncated model predictions
# KL(P||Q) measures information lost when using Q to approximate P

cat("\nStarting KL divergence calculations...\n")

# Initialize storage for results across all years
KL_results_list <- list()

# Process each year of interest
for (year in years_to_analyze) {
  cat("Processing year:", year, "\n")
  
  # Initialize data frame for current year
  df_year <- data.frame(
    series = integer(),
    KL_divergence = numeric(),
    difference = numeric(),
    sign_indicator = integer(),
    stringsAsFactors = FALSE
  )
  
  # Process each series within the current year
  for (s in n_series) {
    
    # Construct column name following E_x[time, series] format
    colname <- paste0("E_x[", year, ", ", s, "]")
    
    # Find column indices in both model datasets
    col_idx_complete <- match(colname, colnames(complete_model_data))
    col_idx_trunc <- match(colname, colnames(truncated_model_data))
    
    # Validate that both columns exist
    if (!is.na(col_idx_complete) && !is.na(col_idx_trunc)) {
      
      # Extract data vectors for KL calculation
      complete_values <- complete_model_data[[col_idx_complete]]
      truncated_values <- truncated_model_data[[col_idx_trunc]]
      
      # Calculate KL divergence: KL(complete || truncated)
      # This measures information lost when using truncated model instead of complete
      KL_value <- tryCatch({
        mean(KL.divergence(complete_values, truncated_values))
      }, error = function(e) {
        warning(paste("KL calculation failed for", colname, ":", e$message))
        NA
      })
      
      # Calculate mean difference (complete - truncated)
      mean_difference <- mean(complete_values, na.rm = TRUE) - 
        mean(truncated_values, na.rm = TRUE)
      
      # Determine sign: -1 = underestimation, +1 = overestimation, 0 = no difference
      sign_indicator <- sign(mean_difference)
      
      # Add results to year data frame
      df_year <- rbind(df_year, data.frame(
        series = s,
        KL_divergence = KL_value,
        difference = mean_difference,
        sign_indicator = sign_indicator,
        stringsAsFactors = FALSE
      ))
      
    } else {
      warning(paste("⚠️  Missing column for year", year, "and series", s))
    }
  }
  
  # Store results for current year (convert to calendar year)
  real_year <- year + base_calendar_year - 1
  KL_results_list[[as.character(real_year)]] <- df_year
}

# ==============================================================================
# STEP 4: COMPILE AND FORMAT RESULTS
# ==============================================================================
# Combine all years into a single dataset

cat("Compiling final results...\n")

# Combine all yearly results
KL_all <- bind_rows(KL_results_list, .id = "year")

# Create signed KL divergence (incorporates direction of bias)
# Positive: truncated model overestimates compared to complete model
# Negative: truncated model underestimates compared to complete model
KL_results <- data.frame(
  Value = KL_all$KL_divergence * KL_all$sign_indicator,
  Year = KL_all$year,
  Series = KL_all$series,
  Labels = rep(labels, length.out = nrow(KL_all)),
  stringsAsFactors = FALSE
)

# ==============================================================================
# STEP 5: SAVE RESULTS
# ==============================================================================
# Export KL divergence results for further analysis

filename_KL <- paste0("KL_", latent_process, ".csv")

cat("Saving KL divergence results...\n")
write.csv(
  KL_results,
  file = file.path(path_results, filename_KL),
  row.names = FALSE
)

cat("KL analysis completed! Results saved to:", filename_KL, "\n")
