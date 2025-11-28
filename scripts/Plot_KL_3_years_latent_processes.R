# ==============================================================================
# STEP 7: KL DIVERGENCE HEATMAP VISUALIZATION
# ==============================================================================
# Create comprehensive heatmap showing KL divergences across models, years, and series

cat("Preparing comprehensive KL divergence analysis...\n")

# Combine results from different temporal models (assuming they exist)
# This section assumes you have KL_RE, KL_RW, KL_AR from different model runs
if (exists("KL_RE") && exists("KL_RW") && exists("KL_AR")) {
  
  # Add model identifiers
  KL_RE$Model <- "RE"
  KL_RW$Model <- "RW" 
  KL_AR$Model <- "AR"
  
  # Combine all models
  df_kl_combined <- bind_rows(KL_RE, KL_RW, KL_AR)
  
  # Clean and prepare data for visualization
  df_kl_viz <- df_kl_combined %>%
    rename(KL_divergence = Value,
           temporal_model = Model,
           year = Year,
           series = Series) %>%
    mutate(
      # Categorize deviations
      deviation_type = case_when(
        KL_divergence > 0 ~ "Overestimation",
        KL_divergence < 0 ~ "Underestimation", 
        TRUE ~ "Neutral"
      ),
      # Convert to factors for proper ordering
      temporal_model = factor(temporal_model, levels = c("RE", "RW", "AR")),
      year = factor(year, levels = sort(unique(year), decreasing = TRUE)),
      series = as.factor(series),
      # Calculate absolute values for sizing
      abs_KL = abs(KL_divergence)
    )
  
  # Create comprehensive heatmap
  kl_heatmap <- ggplot(df_kl_viz, aes(x = series, y = year)) +
    # Points sized by absolute KL divergence, colored by direction
    geom_point(aes(size = abs_KL, 
                   fill = KL_divergence, 
                   color = deviation_type),
               shape = 21, stroke = 1) +
    
    # Add numerical labels
    geom_text(aes(label = round(KL_divergence, 3)), 
              vjust = -1.8, size = 2.5, fontface = "bold") +
    
    # Separate panels for each temporal model
    facet_grid(temporal_model ~ ., 
               scales = "free_y", 
               space = "free_y") +
    
    # Color scales
    scale_size_continuous(range = c(2, 12), 
                          name = "|KL Divergence|",
                          guide = guide_legend(override.aes = list(stroke = 0))) +
    scale_fill_gradient2(low = "#FF3030", mid = "white", high = "blue3",
                         midpoint = 0, name = "KL Divergence") +
    scale_color_manual(values = c("Overestimation" = "darkblue", 
                                  "Underestimation" = "darkred", 
                                  "Neutral" = "gray50"),
                       name = "Bias Direction") +
    
    # Layout and styling
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(hjust = 1, size = 8),
      axis.text.y = element_text(size = 9),
      strip.text = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "gray95", color = NA),
      panel.spacing.y = unit(0.8, "lines"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    labs(
      title = "KL Divergence Analysis: Truncated vs Complete Models",
      subtitle = "Larger points indicate greater information loss",
      x = "Time Series",
      y = "Year",
      caption = "Positive values: truncated model overestimates | Negative: underestimates"
    )
  
  # Display the heatmap
  print(kl_heatmap)
  
} else {
  cat("Multiple temporal model results not found. Skipping comprehensive heatmap.\n")
}

cat("\n🎉 Analysis completed successfully!\n")
cat("📋 Summary of outputs:\n")
cat("   • Wide format E_x data:", filename, "\n")
cat("   • KL divergence results:", filename_KL, "\n")
cat("   • Comparison plots stored in plot_list_compare\n")
if (exists("kl_heatmap")) {
  cat("   • KL divergence heatmap visualization created\n")
}

ggsave(
  filename = file.path(output_directory, paste0("kl_heatmap.png")),
  plot = kl_heatmap,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white")
