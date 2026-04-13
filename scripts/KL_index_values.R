# ==============================================================================
# KL_index_values_vs_obs.R
# ==============================================================================
# PURPOSE: Calculate KL divergence between truncated model predictions and
# observed data (y_obs_full) for the truncated years.
#
# COMPARISON:
#   P = posterior distribution of x[t,s] from truncated model (MCMC samples)
#   Q = normal approximation centered on y_obs_full[t,s] (ground truth)
#   KL(P||Q) measures how far the model's forecast is from the truth.
#
# SIGN CONVENTION:
#   Positive KL : model overestimates  (mean prediction > observed)
#   Negative KL : model underestimates (mean prediction < observed)
#
# INPUT:
#   - truncated_model_data : data.frame from KL_data_prep_trunc.R
#   - y_obs_full           : full observed data matrix [n_total x nb_series]
#   - years_to_analyze     : integer vector of time indices (e.g. c(49, 50, 51))
#   - base_calendar_year   : calendar year corresponding to model time index 1
#   - n_series             : integer vector 1:nb_series
#   - labels               : character vector of series names
#   - latent_process       : string identifier ("RE", "RW", "AR1")
#   - path_results         : output directory path
#
# OUTPUT:
#   - KL_results : data.frame(Value, Year, Series, Labels)
#   - CSV saved to results/KL_[latent_process].csv
# ==============================================================================

cat("\nStarting KL divergence calculations (truncated model vs y_obs)...\n")

# Initialize storage for results across all years
KL_results_list <- list()

# Process each year of interest
for (year in years_to_analyze) {
  cat("Processing year:", year,
      "(", year + base_calendar_year - 1, ")\n")
  
  # Initialize data frame for current year
  df_year <- data.frame(
    series         = integer(),
    KL_divergence  = numeric(),
    difference     = numeric(),
    sign_indicator = integer(),
    stringsAsFactors = FALSE
  )
  
  # Process each series within the current year
  for (s in n_series) {
    
    # Construct column name following x[time, series] format
    colname <- paste0("x[", year, ", ", s, "]")
    
    # Find column index in truncated model samples
    col_idx_trunc <- match(colname, colnames(truncated_model_data))
    
    # Retrieve the observed value for this year/series (ground truth)
    obs_value <- y_obs_full[year, s]
    
    # Validate that column exists and observation is not missing
    if (!is.na(col_idx_trunc) && !is.na(obs_value)) {
      
      # Extract posterior samples for x[t,s] from truncated model
      truncated_values <- truncated_model_data[[col_idx_trunc]]
      
      # Approximate Q as a normal distribution centered on the observed value
      # with the same spread as the posterior (non-parametric comparison)
      q_samples <- rnorm(length(truncated_values),
                         mean = obs_value,
                         sd   = max(sd(truncated_values), 1e-6))
      
      # Calculate KL divergence: KL(truncated posterior || observed-centered normal)
      # This measures how far the model predictions are from the truth
      KL_value <- tryCatch({
        mean(FNN::KL.divergence(truncated_values, q_samples))
      }, error = function(e) {
        warning(paste("KL calculation failed for", colname, ":", e$message))
        NA
      })
      
      # Calculate mean difference (truncated prediction - observed)
      # Positive = model overestimates, Negative = model underestimates
      mean_difference <- mean(truncated_values, na.rm = TRUE) - obs_value
      
      # Determine sign: -1 = underestimation, +1 = overestimation, 0 = no difference
      sign_indicator <- sign(mean_difference)
      
      # Add results to year data frame
      df_year <- rbind(df_year, data.frame(
        series         = s,
        KL_divergence  = KL_value,
        difference     = mean_difference,
        sign_indicator = sign_indicator,
        stringsAsFactors = FALSE
      ))
      
    } else {
      warning(paste("Missing column or observed value for year", year,
                    "and series", s))
    }
  }
  
  # Store results for current year (convert to calendar year)
  real_year <- year + base_calendar_year - 1
  KL_results_list[[as.character(real_year)]] <- df_year
}

# ==============================================================================
# COMPILE AND FORMAT RESULTS
# ==============================================================================

cat("Compiling final results...\n")

# Combine all yearly results
KL_all <- bind_rows(KL_results_list, .id = "year")

# Create signed KL divergence (incorporates direction of bias)
# Positive: truncated model overestimates compared to observed data
# Negative: truncated model underestimates compared to observed data
KL_results <- data.frame(
  Value  = KL_all$KL_divergence * KL_all$sign_indicator,
  Year   = KL_all$year,
  Series = KL_all$series,
  Labels = rep(labels, length.out = nrow(KL_all)),
  stringsAsFactors = FALSE
)

# ==============================================================================
# SAVE RESULTS
# ==============================================================================

filename_KL <- paste0("KL_", latent_process, ".csv")

cat("Saving KL divergence results...\n")
write.csv(
  KL_results,
  file      = file.path(path_results, filename_KL),
  row.names = FALSE
)

cat("KL analysis completed! Results saved to:", filename_KL, "\n")