# -------------------------------------------------------------------------
# Effective Sample Size (ESS) diagnostics — one plot per parameter type
# including factors[k,t] and loadings[i,k]
# -------------------------------------------------------------------------

# ==============================================================================
# CREATE OUTPUT DIRECTORY FOR ESS PNG FILES
# ==============================================================================
# Create "ESS" directory in path_results
ess_plots_directory <- file.path(path_results, "ESS")

# Create directory if it doesn't exist
if (!dir.exists(ess_plots_directory)) {
  dir.create(ess_plots_directory, recursive = TRUE)
  cat("Created directory:", ess_plots_directory, "\n")
}

# Initialize list to store plots for potential combined output
ess_plot_list <- list()

# 0) List all parameter types present in ESS results
param_types_ess <- ess_results %>% dplyr::distinct(Type) %>% dplyr::pull(Type)

cat("Processing ESS diagnostics for", length(param_types_ess), "parameter types...\n")

for (type in param_types_ess) {
  
  # ------------------------------------------------------------
  # 1) Keep only rows for this parameter type and drop unusable ESS
  # ------------------------------------------------------------
  ess_dt <- ess_results %>%
    dplyr::filter(Type == type) %>%
    dplyr::filter(!is.na(ESS), is.finite(ESS), ESS > 0)
  
  # Skip if nothing valid
  if (nrow(ess_dt) == 0) {
    message("No valid ESS rows for type: ", type, " — skipping plot.")
    next
  }
  
  # ------------------------------------------------------------
  # 2) Special case: factors[k,t] — separate by k
  # ------------------------------------------------------------
  if (type == "factors") {
    ess_dt <- ess_dt %>%
      dplyr::mutate(
        k_index = stringr::str_extract(Parameter, "\\[([0-9]+),") %>%
          stringr::str_extract("[0-9]+") %>%
          as.numeric()
      ) %>%
      dplyr::filter(!is.na(k_index))
    
    unique_k <- sort(unique(ess_dt$k_index))
    
    for (k_val in unique_k) {
      factor_data <- ess_dt %>% dplyr::filter(k_index == k_val) %>% dplyr::arrange(ESS)
      
      # Limit number of parameters displayed
      max_show <- DIAGNOSTIC_CONFIG$max_display_params
      if (nrow(factor_data) > max_show) {
        factor_data <- factor_data %>% dplyr::slice_head(n = max_show)
        warning("Displaying only first ", max_show, " parameters for factor k=", k_val)
      }
      
      # Plot ESS bar chart
      p <- ggplot(factor_data, aes(x = reorder(Parameter, ESS), y = ESS)) +
        geom_col(aes(fill = ESS_Adequate), alpha = 0.7, color = "white", linewidth = 0.2) +
        geom_hline(
          yintercept = DIAGNOSTIC_CONFIG$ess_min_threshold,
          color = "red", linetype = "dashed", linewidth = 0.8
        ) +
        scale_fill_manual(
          name = "Adequate ESS",
          values = c("FALSE" = "#E15759", "TRUE" = "#59A14F"),
          labels = c("FALSE" = paste0("< ", DIAGNOSTIC_CONFIG$ess_min_threshold),
                     "TRUE"  = paste0("≥ ", DIAGNOSTIC_CONFIG$ess_min_threshold))
        ) +
        labs(
          title = paste0("ESS Diagnostics — Factor k=", k_val),
          subtitle = paste(
            "Parameters shown:", nrow(factor_data),
            "| Min ESS:", format(min(factor_data$ESS), big.mark = " "),
            "| Adequacy rate:", sprintf("%.1f%%", 100 * mean(factor_data$ESS_Adequate))
          ),
          x = "Parameter",
          y = "Effective Sample Size"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(color = "grey60"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = "top"
        ) +
        coord_flip()
      
      # Store plot in diagnostic_plots list
      plot_name <- paste0("ess_factors_k", k_val)
      diagnostic_plots[[plot_name]] <- p
      ess_plot_list[[plot_name]] <- p
      
      # Save individual plot as PNG
      png_filename <- paste0("ESS_factors_k", k_val,".png")
      png_filepath <- file.path(ess_plots_directory, png_filename)
      
      ggsave(
        filename = png_filepath,
        plot = p,
        width = 12,           # Wider for better parameter readability
        height = 8,           # Height in inches
        dpi = 300,            # High resolution
        bg = "white"          # White background
      )
      
      cat("  ✓ Saved ESS factors k=", k_val, ":", png_filename, "\n")
    }
    
  } 
  # ------------------------------------------------------------
  # 2bis. Special case: loadings[i,k] — separate by k
  # ------------------------------------------------------------
  else if (type == "loadings") {
    ess_dt <- ess_dt %>%
      dplyr::mutate(
        k_index = stringr::str_extract(Parameter, ",\\s*([0-9]+)\\]") %>%
          stringr::str_extract("[0-9]+") %>%
          as.numeric()
      ) %>%
      dplyr::filter(!is.na(k_index))
    
    unique_k <- sort(unique(ess_dt$k_index))
    
    for (k_val in unique_k) {
      lambda_data <- ess_dt %>% dplyr::filter(k_index == k_val) %>% dplyr::arrange(ESS)
      
      max_show <- DIAGNOSTIC_CONFIG$max_display_params
      if (nrow(lambda_data) > max_show) {
        lambda_data <- lambda_data %>% dplyr::slice_head(n = max_show)
        warning("Displaying only first ", max_show, " parameters for loading k=", k_val)
      }
      
      p <- ggplot(lambda_data, aes(x = reorder(Parameter, ESS), y = ESS)) +
        geom_col(aes(fill = ESS_Adequate), alpha = 0.7, color = "white", linewidth = 0.2) +
        geom_hline(
          yintercept = DIAGNOSTIC_CONFIG$ess_min_threshold,
          color = "red", linetype = "dashed", linewidth = 0.8
        ) +
        scale_fill_manual(
          name = "Adequate ESS",
          values = c("FALSE" = "#E15759", "TRUE" = "#59A14F"),
          labels = c("FALSE" = paste0("< ", DIAGNOSTIC_CONFIG$ess_min_threshold),
                     "TRUE"  = paste0("≥ ", DIAGNOSTIC_CONFIG$ess_min_threshold))
        ) +
        labs(
          title = paste0("ESS Diagnostics — Loading k=", k_val),
          subtitle = paste(
            "Parameters shown:", nrow(lambda_data),
            "| Min ESS:", format(min(lambda_data$ESS), big.mark = " "),
            "| Adequacy rate:", sprintf("%.1f%%", 100 * mean(lambda_data$ESS_Adequate))
          ),
          x = "Parameter",
          y = "Effective Sample Size"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(color = "grey60"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = "top"
        ) +
        coord_flip()
      
      # Store plot in diagnostic_plots list
      plot_name <- paste0("ess_loadings_k", k_val)
      diagnostic_plots[[plot_name]] <- p
      ess_plot_list[[plot_name]] <- p
      
      # Save individual plot as PNG
      png_filename <- paste0("ESS_loadings_k", k_val,".png")
      png_filepath <- file.path(ess_plots_directory, png_filename)
      
      ggsave(
        filename = png_filepath,
        plot = p,
        width = 12,           # Wider for better parameter readability
        height = 8,           # Height in inches
        dpi = 300,            # High resolution
        bg = "white"          # White background
      )
      
      cat("  ✓ Saved ESS loadings k=", k_val, ":", png_filename, "\n")
    }
  } 
  # ------------------------------------------------------------
  # 3) Default case for other parameter types
  # ------------------------------------------------------------
  else {
    ess_dt <- ess_dt %>% dplyr::arrange(ESS)
    
    max_show <- DIAGNOSTIC_CONFIG$max_display_params
    if (nrow(ess_dt) > max_show) {
      ess_dt <- ess_dt %>% dplyr::slice_head(n = max_show)
      warning("Displaying only first ", max_show, " parameters for type: ", type)
    }
    
    p <- ggplot(ess_dt, aes(x = reorder(Parameter, ESS), y = ESS)) +
      geom_col(aes(fill = ESS_Adequate), alpha = 0.7, color = "white", linewidth = 0.2) +
      geom_hline(
        yintercept = DIAGNOSTIC_CONFIG$ess_min_threshold,
        color = "red", linetype = "dashed", linewidth = 0.8
      ) +
      scale_fill_manual(
        name = "Adequate ESS",
        values = c("FALSE" = "#E15759", "TRUE" = "#59A14F"),
        labels = c("FALSE" = paste0("< ", DIAGNOSTIC_CONFIG$ess_min_threshold),
                   "TRUE"  = paste0("≥ ", DIAGNOSTIC_CONFIG$ess_min_threshold))
      ) +
      labs(
        title = paste0("ESS Diagnostics — ", type),
        subtitle = paste(
          "Parameters shown:", nrow(ess_dt),
          "| Min ESS:", format(min(ess_dt$ESS), big.mark = " "),
          "| Adequacy rate:", sprintf("%.1f%%", 100 * mean(ess_dt$ESS_Adequate))
        ),
        x = "Parameter",
        y = "Effective Sample Size"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey60"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "top"
      ) +
      coord_flip()
    
    # Store plot in diagnostic_plots list
    plot_name <- paste0("ess_", type)
    diagnostic_plots[[plot_name]] <- p
    ess_plot_list[[plot_name]] <- p
    
    # Save individual plot as PNG
    png_filename <- paste0("ESS_", type,".png")
    png_filepath <- file.path(ess_plots_directory, png_filename)
    
    ggsave(
      filename = png_filepath,
      plot = p,
      width = 12,           # Wider for better parameter readability
      height = 8,           # Height in inches
      dpi = 300,            # High resolution
      bg = "white"          # White background
    )
    
    cat("  ✓ Saved ESS", type, ":", png_filename, "\n")
  }
}

# ==============================================================================
# CREATE COMBINED ESS SUMMARY PLOT (OPTIONAL)
# ==============================================================================
if (length(ess_plot_list) > 1) {
  cat("Creating combined ESS plots...\n")
  
  # Configuration for combined plot
  plots_per_page <- 4                           # Number of plots per page
  n_pages <- ceiling(length(ess_plot_list) / plots_per_page)
  
  # Save each page as a separate PNG file
  for (page in seq_len(n_pages)) {
    
    # Calculate which plots go on current page
    start_idx <- (page - 1) * plots_per_page + 1
    end_idx <- min(page * plots_per_page, length(ess_plot_list))
    
    # Extract subset of plots for current page
    plots_subset <- ess_plot_list[start_idx:end_idx]
    
    # Create filename for combined plot page
    combined_filename <- paste0("ESS_combined_page", page,".png")
    combined_filepath <- file.path(ess_plots_directory, combined_filename)
    
    # Create and save combined plot
    png(combined_filepath, width = 16, height = 12, units = "in", res = 300)
    
    # Arrange plots in 2-column grid
    do.call("grid.arrange", c(plots_subset, ncol = 2))
    
    # Close PNG device
    dev.off()
    
    cat("  ✓ Saved combined ESS plot:", combined_filename, "\n")
  }
}

cat("All ESS diagnostic plots saved successfully in:", ess_plots_directory, "\n")

# ==============================================================================