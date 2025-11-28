# ------------------------------------------------------------------------------
#                                 TRACEPLOTS
# ------------------------------------------------------------------------------

cat("Creating enhanced traceplots...\n")

N <- 10   # Number of entries per factor / lambda / mu

# -------------------------------
# AUTOMATIC PARAMETER GROUP CREATION
# -------------------------------
param_groups <- list()

for(k in 1:K){
  # Lambda parameters for factor k
  param_groups[[paste0("lambda", k)]] <- paste0("lambda[", 2:N, ", ", k, "]")
  # Factor parameters for factor k
  param_groups[[paste0("factor", k)]] <- paste0("factor[", k, ", ", 2:N, "]")
}

# Mu parameters (common to all)
param_groups[["mu"]] <- paste0("mu_x[", 1:N, "]")

# -------------------------------
# CREATE TRACEPLOTS FOR ALL PARAMETER GROUPS
# -------------------------------

# Load required packages for enhanced plots
if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
  warning("RColorBrewer package recommended for better colors. Using default colors.")
}

cat("Generating traceplots for parameter groups...\n")

# Counter for successful plots
successful_plots <- 0

for (group_name in names(param_groups)) {
  cat("  Processing group:", group_name, "...")
  
  tryCatch({
    # Create combined traceplot for this group
    group_plot <- create_combined_traceplot(
      mcmc_data = mcmc,                   # Ampute chains here if Label-switching exists
      param_group = param_groups[[group_name]], 
      group_name = group_name,
      max_params = 25  # Adjust as needed
    )
    
    if (!is.null(group_plot)) {
      # Store in diagnostic_plots list
      diagnostic_plots[[paste0("traceplot_", group_name)]] <- group_plot
      successful_plots <- successful_plots + 1
      cat(" ✓\n")
    } else {
      cat(" ✗ (no valid parameters)\n")
    }
    
  }, error = function(e) {
    cat(" ✗ (error:", e$message, ")\n")
  })
}

cat("Successfully created", successful_plots, "traceplot groups\n")

# -------------------------------
# SAVE TRACEPLOTS IF REQUESTED
# -------------------------------

if (save_plots && successful_plots > 0) {
  cat("Saving traceplot files...\n")
  
  traceplot_names <- names(diagnostic_plots)[grepl("traceplot_", names(diagnostic_plots))]
  
  for (plot_name in traceplot_names) {
    tryCatch({
      # Use larger dimensions for traceplots
      plot_width <- ifelse(grepl("summary", plot_name), 15, 12)
      plot_height <- ifelse(grepl("summary", plot_name), 12, 8)
      
      ggsave(
        filename = file.path(output_directory, paste0(plot_name, ".png")),
        plot = diagnostic_plots[[plot_name]],
        width = plot_width,
        height = plot_height,
        dpi = 300,
        bg = "white"
      )
      
    }, error = function(e) {
      warning("Could not save ", plot_name, ": ", e$message)
    })
  }
  
  cat("✓ Traceplots saved to:", output_directory, "\n")
}

# -------------------------------
# DISPLAY SUMMARY INFORMATION
# -------------------------------

cat("\nTRACEPLOT SUMMARY:\n")
cat("  • Parameter groups processed:", length(param_groups), "\n")
cat("  • Successful traceplot groups:", successful_plots, "\n")
cat("  • Total traceplots in diagnostic_plots:", 
    length(names(diagnostic_plots)[grepl("traceplot_", names(diagnostic_plots))]), "\n")

# Optional: Display the summary traceplot
if ("traceplot_summary" %in% names(diagnostic_plots)) {
  cat("  • Summary traceplot available for immediate viewing\n")
  # Uncomment the next line to display the plot immediately
  # print(diagnostic_plots[["traceplot_summary"]])
}

cat("\nTraceplots integration completed successfully!\n")
