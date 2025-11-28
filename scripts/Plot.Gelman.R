# -------------------------------------------------------------------------
# Gelman-Rubin (PSRF) diagnostic plots for each parameter type
# -------------------------------------------------------------------------

cat("Creating diagnostic plots...\n")

# ==============================================================================
# CREATE OUTPUT DIRECTORY FOR GELMAN-RUBIN PNG FILES
# ==============================================================================
# Create "Gelman-Rubin" directory in path_results
gelman_plots_directory <- file.path(path_results, "Gelman-Rubin")

# Create directory if it doesn't exist
if (!dir.exists(gelman_plots_directory)) {
  dir.create(gelman_plots_directory, recursive = TRUE)
  cat("Created directory:", gelman_plots_directory, "\n")
}

# Initialize list to store plots for potential combined output
gelman_plot_list <- list()

# Get all unique parameter types (e.g., factors, loadings, variances, etc.)
param_types <- unique(gelman_results$Type)
colors <- DIAGNOSTIC_CONFIG$color_palette

cat("Processing Gelman-Rubin diagnostics for", length(param_types), "parameter types...\n")

# -------------------------------------------------------------------------
# Create Gelman-Rubin (PSRF) diagnostic plots for each parameter type
# -------------------------------------------------------------------------
for (i in seq_along(param_types)) {
  type <- param_types[i]
  # Assign a color to this parameter type (cycling through the palette)
  color_idx <- ((i - 1) %% length(colors)) + 1
  
  # ------------------------------------------------------------
  # 1. Filter the data: keep only rows belonging to the current parameter type
  # ------------------------------------------------------------
  filtered_data <- gelman_results %>%
    dplyr::filter(Type == type)
  
  # ------------------------------------------------------------
  # 2. Special case for latent factors: extract the index k from factors[k,t]
  # ------------------------------------------------------------
  if (type == "factors") {
    # Extract the k index from the parameter name (first position in [k,t])
    filtered_data <- filtered_data %>%
      dplyr::mutate(
        k_index = stringr::str_extract(Parameter, "\\[([0-9]+),") %>%
          stringr::str_extract("[0-9]+") %>%
          as.numeric()
      ) %>%
      dplyr::filter(!is.na(k_index))  # Drop rows where extraction failed
    
    # Identify all unique k values present
    unique_k <- sort(unique(filtered_data$k_index))
    
    # Create one diagnostic plot per factor k
    for (k_val in unique_k) {
      factor_data <- filtered_data %>%
        dplyr::filter(k_index == k_val)
      
      # Limit the number of displayed parameters to improve readability
      if (nrow(factor_data) > DIAGNOSTIC_CONFIG$max_display_params) {
        factor_data <- factor_data %>%
          dplyr::slice_head(n = DIAGNOSTIC_CONFIG$max_display_params)
        warning("Displaying only first ", DIAGNOSTIC_CONFIG$max_display_params,
                " parameters for factor k=", k_val)
      }
      
      # Decide whether to flip axes depending on how many parameters are shown
      flip_axes <- nrow(factor_data) > DIAGNOSTIC_CONFIG$flip_axis_threshold
      
      # Create the bar plot of PSRF values
      p <- ggplot(factor_data, aes(x = reorder(Parameter, -PSRF), y = PSRF)) +
        geom_col(fill = colors[color_idx], alpha = 0.7, color = "white", linewidth = 0.2) +
        # Add threshold line for acceptable PSRF values
        geom_hline(
          yintercept = DIAGNOSTIC_CONFIG$psrf_threshold, 
          color = "red", 
          linetype = "dashed", 
          linewidth = 0.8
        ) +
        labs(
          title = paste("Gelman-Rubin Diagnostics: Factor k =", k_val),
          subtitle = paste("Threshold:", DIAGNOSTIC_CONFIG$psrf_threshold, 
                           "| Parameters:", nrow(factor_data)),
          x = "Parameter",
          y = "Potential Scale Reduction Factor (PSRF)"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(color = "grey60"),
          axis.text.x = element_text(hjust = 1, size = 9),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()
        )
      
      # Flip axes if too many parameters (makes labels more readable)
      if (flip_axes) {
        p <- p + coord_flip() +
          theme(axis.text.y = element_text(size = 8))
      }
      
      # Save plot into the diagnostics list with a unique name
      plot_name <- paste0("gelman_factors_k", k_val)
      diagnostic_plots[[plot_name]] <- p
      gelman_plot_list[[plot_name]] <- p
      
      # Save individual plot as PNG
      png_filename <- paste0("Gelman_factors_k", k_val, "_",".png")
      png_filepath <- file.path(gelman_plots_directory, png_filename)
      
      ggsave(
        filename = png_filepath,
        plot = p,
        width = 12,           # Wider for better parameter readability
        height = 8,           # Height in inches
        dpi = 300,            # High resolution
        bg = "white"          # White background
      )
      
      cat("  ✓ Saved Gelman factors k=", k_val, ":", png_filename, "\n")
    }
    
  } 
  # ------------------------------------------------------------
  # 2bis. Special case for loadings: extract k from lambda[i,k]
  # ------------------------------------------------------------
  else if (type == "loadings") {
    # Extract the k index (second position in [i,k])
    filtered_data <- filtered_data %>%
      dplyr::mutate(
        k_index = stringr::str_extract(Parameter, ",\\s*([0-9]+)\\]") %>%
          stringr::str_extract("[0-9]+") %>%
          as.numeric()
      ) %>%
      dplyr::filter(!is.na(k_index))
    
    # Identify all unique k values
    unique_k <- sort(unique(filtered_data$k_index))
    
    # Create one diagnostic plot per loading k
    for (k_val in unique_k) {
      lambda_data <- filtered_data %>%
        dplyr::filter(k_index == k_val)
      
      # Limit number of parameters displayed
      if (nrow(lambda_data) > DIAGNOSTIC_CONFIG$max_display_params) {
        lambda_data <- lambda_data %>%
          dplyr::slice_head(n = DIAGNOSTIC_CONFIG$max_display_params)
        warning("Displaying only first ", DIAGNOSTIC_CONFIG$max_display_params,
                " parameters for loading k=", k_val)
      }
      
      # Decide orientation
      flip_axes <- nrow(lambda_data) > DIAGNOSTIC_CONFIG$flip_axis_threshold
      
      # Create bar plot
      p <- ggplot(lambda_data, aes(x = reorder(Parameter, -PSRF), y = PSRF)) +
        geom_col(fill = colors[color_idx], alpha = 0.7, color = "white", linewidth = 0.2) +
        geom_hline(
          yintercept = DIAGNOSTIC_CONFIG$psrf_threshold,
          color = "red",
          linetype = "dashed",
          linewidth = 0.8
        ) +
        labs(
          title = paste("Gelman-Rubin Diagnostics: Loading k =", k_val),
          subtitle = paste("Threshold:", DIAGNOSTIC_CONFIG$psrf_threshold,
                           "| Parameters:", nrow(lambda_data)),
          x = "Parameter",
          y = "Potential Scale Reduction Factor (PSRF)"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(color = "grey60"),
          axis.text.x = element_text(hjust = 1, size = 9),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()
        )
      
      if (flip_axes) {
        p <- p + coord_flip() +
          theme(axis.text.y = element_text(size = 8))
      }
      
      # Save plot
      plot_name <- paste0("gelman_loadings_k", k_val)
      diagnostic_plots[[plot_name]] <- p
      gelman_plot_list[[plot_name]] <- p
      
      # Save individual plot as PNG
      png_filename <- paste0("Gelman_loadings_k", k_val,".png")
      png_filepath <- file.path(gelman_plots_directory, png_filename)
      
      ggsave(
        filename = png_filepath,
        plot = p,
        width = 12,           # Wider for better parameter readability
        height = 8,           # Height in inches
        dpi = 300,            # High resolution
        bg = "white"          # White background
      )
      
      cat("  ✓ Saved Gelman loadings k=", k_val, ":", png_filename, "\n")
    }
  } else {
    # ------------------------------------------------------------
    # 3. Default case for all other parameter types
    # ------------------------------------------------------------
    
    # Limit number of displayed parameters
    if (nrow(filtered_data) > DIAGNOSTIC_CONFIG$max_display_params) {
      filtered_data <- filtered_data %>%
        dplyr::slice_head(n = DIAGNOSTIC_CONFIG$max_display_params)
      warning("Displaying only first ", DIAGNOSTIC_CONFIG$max_display_params,
              " parameters for type: ", type)
    }
    
    # Decide orientation
    flip_axes <- nrow(filtered_data) > DIAGNOSTIC_CONFIG$flip_axis_threshold
    
    # Create bar plot
    p <- ggplot(filtered_data, aes(x = reorder(Parameter, -PSRF), y = PSRF)) +
      geom_col(fill = colors[color_idx], alpha = 0.7, color = "white", linewidth = 0.2) +
      geom_hline(
        yintercept = DIAGNOSTIC_CONFIG$psrf_threshold, 
        color = "red", 
        linetype = "dashed", 
        linewidth = 0.8
      ) +
      labs(
        title = paste("Gelman-Rubin Diagnostics:",
                      stringr::str_to_title(gsub("_", " ", type))),
        subtitle = paste("Threshold:", DIAGNOSTIC_CONFIG$psrf_threshold, 
                         "| Parameters:", nrow(filtered_data)),
        x = "Parameter",
        y = "Potential Scale Reduction Factor (PSRF)"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey60"),
        axis.text.x = element_text(hjust = 1, size = 9),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      )
    
    if (flip_axes) {
      p <- p + coord_flip() +
        theme(axis.text.y = element_text(size = 8))
    }
    
    # Save plot
    plot_name <- paste0("gelman_", type)
    diagnostic_plots[[plot_name]] <- p
    gelman_plot_list[[plot_name]] <- p
    
    # Save individual plot as PNG
    png_filename <- paste0("Gelman_", type, "_",".png")
    png_filepath <- file.path(gelman_plots_directory, png_filename)
    
    ggsave(
      filename = png_filepath,
      plot = p,
      width = 12,           # Wider for better parameter readability
      height = 8,           # Height in inches
      dpi = 300,            # High resolution
      bg = "white"          # White background
    )
    
    cat("  ✓ Saved Gelman", type, ":", png_filename, "\n")
  }
}

# ==============================================================================
# CREATE COMBINED GELMAN-RUBIN SUMMARY PLOTS (OPTIONAL)
# ==============================================================================
if (length(gelman_plot_list) > 1) {
  cat("Creating combined Gelman-Rubin plots...\n")
  
  # Configuration for combined plot
  plots_per_page <- 4                           # Number of plots per page
  n_pages <- ceiling(length(gelman_plot_list) / plots_per_page)
  
  # Save each page as a separate PNG file
  for (page in seq_len(n_pages)) {
    
    # Calculate which plots go on current page
    start_idx <- (page - 1) * plots_per_page + 1
    end_idx <- min(page * plots_per_page, length(gelman_plot_list))
    
    # Extract subset of plots for current page
    plots_subset <- gelman_plot_list[start_idx:end_idx]
    
    # Create filename for combined plot page
    combined_filename <- paste0("Gelman_combined_page", page,".png")
    combined_filepath <- file.path(gelman_plots_directory, combined_filename)
    
    # Create and save combined plot
    png(combined_filepath, width = 16, height = 12, units = "in", res = 300)
    
    # Arrange plots in 2-column grid
    do.call("grid.arrange", c(plots_subset, ncol = 2))
    
    # Close PNG device
    dev.off()
    
    cat("  ✓ Saved combined Gelman-Rubin plot:", combined_filename, "\n")
  }
}

cat("All Gelman-Rubin diagnostic plots saved successfully in:", gelman_plots_directory, "\n")

# ==============================================================================