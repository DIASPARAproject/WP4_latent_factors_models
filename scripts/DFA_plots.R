cat("\nCREATING DFA-SPECIFIC VISUALIZATIONS\n")

# ---------------
# FACTOR AND LAMBDA VISUALIZATION FOR DFA MODEL
# ---------------
cat("Creating factor and lambda visualizations...\n")
# -------------------------------
# GENERATE FACTOR AND LAMBDA PLOTS
# -------------------------------

# -------------------------------
# 1. FACTOR EVOLUTION PLOTS
# -------------------------------

cat("  Creating factor evolution plots...\n")
for (k in 1:K) {
  factor_plot <- create_factor_evolution_plot(mcmc, k)
  
  if (!is.null(factor_plot)) {
    diagnostic_plots[[paste0("factor_evolution_", k)]] <- factor_plot
    cat("    ✓ Factor", k, "evolution plot created\n")
  } else {
    cat("    ✗ Could not create factor", k, "evolution plot\n")
  }
}

# Create combined factors evolution plot
cat("  Creating combined factors evolution plot...\n")
combined_factors <- create_combined_factors_plot(mcmc, factors_to_show = NULL)
diagnostic_plots[["factor_combined_evolution"]] <- combined_factors
cat("    ✓ Combined factors evolution plot created\n")


# -------------------------------
# 2. LAMBDA DISTRIBUTION PLOTS
# -------------------------------

cat("  Updating lambda distribution plots with label names...\n")
for (k in 1:K) {
  lambda_dist_plot <- create_lambda_distribution_with_labels(mcmc, k, labels)
  
  if (!is.null(lambda_dist_plot)) {
    diagnostic_plots[[paste0("lambda_distribution_labels_", k)]] <- lambda_dist_plot
    cat("    ✓ Lambda distribution with labels for factor", k, "created\n")
  }
}

cat("  Updating lambda distribution plots together...\n")
  lambda_grouped_plot <- create_lambda_combined_plot(mcmc, labels, k)
  
  if (!is.null(lambda_grouped_plot)) {
    diagnostic_plots[[paste0("lambda_distribution_labels_together")]] <- lambda_grouped_plot
    cat("    ✓ Lambda distribution with labels for factor created\n")
  }

# -------------------------------
# SAVE FACTOR/LAMBDA PLOTS
# -------------------------------

if (save_plots) {
  cat("  Saving factor and lambda plots...\n")
  
  factor_lambda_plots <- names(diagnostic_plots)[
    grepl("factor_|lambda_", names(diagnostic_plots))
  ]
  
  saved_count <- 0
  for (plot_name in factor_lambda_plots) {
    tryCatch({
      # Adjust dimensions based on plot type
      if (grepl("summary|heatmap|correlation", plot_name)) {
        plot_width <- 12
        plot_height <- 10
      } else {
        plot_width <- 10
        plot_height <- 6
      }
      
      ggsave(
        filename = file.path(output_directory, paste0(plot_name, ".png")),
        plot = diagnostic_plots[[plot_name]],
        width = plot_width,
        height = plot_height,
        dpi = 300,
        bg = "white"
      )
      saved_count <- saved_count + 1
      
    }, error = function(e) {
      warning("Could not save ", plot_name, ": ", e$message)
    })
  }
  
  cat("    ✓", saved_count, "factor/lambda plots saved\n")
}
