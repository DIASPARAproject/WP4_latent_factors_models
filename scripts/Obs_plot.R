# ==============================================================================
# OPTIMIZED EXTRACTION OF y_obs VALUES FROM MCMC OUTPUT
# ==============================================================================
# This script extracts the expected values y_obs[t,i] from MCMC samples and 
# creates summary statistics (mean, 95% credible intervals) for plotting
# against observed data.

# ==============================================================================
# STEP 1: EXTRACT AND SUMMARIZE y_obs VALUES
# ==============================================================================
if (analyze_truncated) {
  # Truncated model analysis
  truncation_suffix <- "T"
  model_type <- "truncated"
  cat("Analyzing TRUNCATED model\n")
} else {
  # Complete model analysis
  truncation_suffix <- ""
  model_type <- "complete"
  cat("Analyzing COMPLETE model\n")
}

# Pre-allocate list to store results for each series
# This is more efficient than growing the list dynamically
y_obs_summary_list <- vector("list", n_series)

# Loop through each series to extract corresponding y_obs columns
for (i in 1:n_series) {
  
  # Create regex pattern to match y_obs[t, i] columns for series i
  # Pattern matches: y_obs[1, i], y_obs[2, i], ..., y_obs[n, i]
  pattern <- paste0("^y_obs\\[[0-9]+, ", i, "\\]$")
  
  # Find column indices that match the pattern
  y_obs_cols <- grep(pattern, colnames(mcmc_matrix))
  
  # Check if columns were found (defensive programming)
  if (length(y_obs_cols) > 0) {
    
    # Extract MCMC samples for this series
    # drop=FALSE ensures we keep matrix structure even for single column
    y_obs_data <- mcmc_matrix[, y_obs_cols, drop = FALSE]
    
    # Compute summary statistics across MCMC iterations
    # colMeans() is faster than apply(, 2, mean) for large matrices
    y_obs_summary_list[[i]] <- data.frame(
      serie = labels[i],              # Series identifier
      year = years,                             # Time axis (years)
      mean = colMeans(y_obs_data),                 # Posterior mean
      lower = apply(y_obs_data, 2, quantile, probs = 0.025),  # Lower 95% CI
      upper = apply(y_obs_data, 2, quantile, probs = 0.975),  # Upper 95% CI
      row.names = NULL                          # Remove row names for cleaner output
    )
  } else {
    # Handle case where no columns found (shouldn't happen with correct model)
    warning(paste("No y_obs columns found for series", i))
  }
}

# ==============================================================================
# STEP 2: COMBINE RESULTS INTO SINGLE DATA FRAME
# ==============================================================================
# Combine all series into a single data frame for easier manipulation
# do.call(rbind, list) is more efficient than multiple rbind() calls
y_obs_summary_table_tot <- do.call(rbind, y_obs_summary_list)

# Reorganize columns for better readability (optional)
# Solution 1: Use dplyr::select() to avoid conflicts
y_obs_summary_table_tot <- y_obs_summary_table_tot %>%
  dplyr::select(serie, year, mean, lower, upper)

# Display first few rows to check results
head(y_obs_summary_table_tot)

# Create filename based on latent process and truncation status
filename_tot <- paste0("Ex_summary_tot_", latent_process, truncation_suffix, ".csv")
filepath_tot <- file.path(path_processed, filename_tot)

# Save results to CSV file for later use or sharing
write.csv(
  y_obs_summary_table_tot,
  file = file.path(path_results, filename_tot),
  row.names = FALSE
)

# ==============================================================================
# STEP 3: CREATE OUTPUT DIRECTORY FOR PNG FILES
# ==============================================================================
# Create "Observation plots" directory in path_results
plots_directory <- file.path(path_results, "Observation plots")

# Create directory if it doesn't exist
if (!dir.exists(plots_directory)) {
  dir.create(plots_directory, recursive = TRUE)
  cat("Created directory:", plots_directory, "\n")
}

# ==============================================================================
# STEP 4: CREATE COMPARISON PLOTS (MODEL vs OBSERVED DATA)
# ==============================================================================
# Pre-allocate list to store ggplot objects
# This avoids dynamic list growth which can be slow
plot_list <- vector("list", n_series)

# Create individual plots for each labels
for (i in 1:n_series) {
  
  # Construct variable name for observed data
  # Assumes observed data follows naming convention: label
  label_name <- paste0(labels[i])
  
  # Filter model predictions for current series
  model_data <- y_obs_summary_table_tot %>% 
    filter(serie == labels[i])
  
  # Filter observed data for current labels
  raw_df <- ts_long %>% 
    filter(label == label_name)
  
  # Create layered plot with observed data and model fit
  plot_list[[i]] <- ggplot() +
    
    # Layer 1: Raw observed data as gray line
    geom_line(data = model_data, 
              aes(x = year, y = mean), 
              color = "darkblue",          # Dark blue
              size = 1.6) +                # Thick line
    
    # Layer 2: Credible interval as shaded ribbon
    geom_ribbon(data = model_data, 
                aes(x = year, ymin = lower, ymax = upper), 
                fill = "dodgerblue4",      # Dark blue
                alpha = 0.3) +             # Transparent
    
    # Layer 3: Posterior mean as solid line
    geom_line(data = raw_df, 
              aes(x = year, y = values), 
              color = "#FF3030",           # Dark gray
              alpha = 0.6,                # Semi-transparent
              size = 0.7) +

    
    # Apply clean theme with custom styling
    theme_minimal(base_size = 14) +
    theme(
      # Center and style the plot title
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", 
                                margin = margin(b = 15)),
      
      # Style axis labels and text
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      axis.ticks = element_line(color = "black", size = 0.5),
      
      # Customize grid lines
      panel.grid.major = element_line(color = "gray90", size = 0.5),
      panel.grid.minor = element_blank(),          # Remove minor grid
      
      # Set plot margins and remove legend
      plot.margin = margin(10, 10, 10, 10),
      legend.position = "none"
    ) +
    
    # Add informative axis labels and title
    labs(
      x = "Year", 
      y = "Survival probability (Logit)", 
      title = paste("Estimation for labels:", labels[i])
    )
}

# Add meaningful names to plot list elements for easier access
names(plot_list) <- labels

# ==============================================================================
# STEP 5: SAVE INDIVIDUAL PLOTS AS PNG FILES
# ==============================================================================
cat("Saving individual plots as PNG files...\n")

# Loop through each plot and save as PNG
for (i in 1:length(plot_list)) {
  
  # Create filename with series name and model type
  png_filename <- paste0("y_obs_plot_", labels[i], "_", model_type, ".png")
  png_filepath <- file.path(plots_directory, png_filename)
  
  # Save plot as high-quality PNG
  ggsave(
    filename = png_filepath,
    plot = plot_list[[i]],
    width = 10,           # Width in inches
    height = 6,           # Height in inches
    dpi = 300,            # High resolution for publication quality
    bg = "white"          # White background
  )
  
  cat("  ✓ Saved:", png_filename, "\n")
}

# ==============================================================================
# STEP 6: CREATE AND SAVE COMBINED PLOT GRID
# ==============================================================================
cat("Creating combined plot grid...\n")

# Configuration for combined plot
plots_per_page <- 4                           # Number of plots per page
n_pages <- ceiling(length(plot_list) / plots_per_page)  # Calculate needed pages

# Save each page as a separate PNG file
for (page in seq_len(n_pages)) {
  
  # Calculate which plots go on current page
  start_idx <- (page - 1) * plots_per_page + 1
  end_idx <- min(page * plots_per_page, length(plot_list))
  
  # Extract subset of plots for current page
  plots_subset <- plot_list[start_idx:end_idx]
  
  # Create filename for combined plot page
  combined_filename <- paste0("Ex_plots_combined_page", page, "_", model_type, ".png")
  combined_filepath <- file.path(plots_directory, combined_filename)
  
  # Create and save combined plot
  png(combined_filepath, width = 12, height = 10, units = "in", res = 300)
  
  # Arrange plots in 2-column grid
  do.call("grid.arrange", c(plots_subset, ncol = 2))
  
  # Close PNG device
  dev.off()
  
  cat("  ✓ Saved combined plot:", combined_filename, "\n")
}

cat("All plots saved successfully in:", plots_directory, "\n")

# ==============================================================================