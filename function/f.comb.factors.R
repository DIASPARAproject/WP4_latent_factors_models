# -------------------------------
# COMBINED FACTORS EVOLUTION PLOT
# -------------------------------

create_combined_factors_plot <- function(mcmc_data, factors_to_show = NULL) {
  
  if (is.null(factors_to_show)) {
    factors_to_show <- 1:min(K, K)  # Show up to 3 factors by default
  }
  
  combined_data <- data.frame()
  
  for (factor_k in factors_to_show) {
    # Extract factor data for factor k
    factor_pattern <- paste0("factor\\[", factor_k, ",")
    factor_data <- extract_posterior_samples(mcmc_data, factor_pattern)
    
    if (!is.null(factor_data)) {
      # Extract time indices
      factor_data$time <- as.numeric(gsub(".*\\[\\d+,\\s*(\\d+)\\]", "\\1", factor_data$parameter))
      factor_data$factor_id <- paste("Factor", factor_k)
      
      combined_data <- rbind(combined_data, factor_data)
    }
  }
  
  if (nrow(combined_data) == 0) {
    return(NULL)
  }
  
  # Calculate posterior summaries using all MCMC samples
  factor_summary <- combined_data %>%
    group_by(factor_id, time) %>%
    summarise(
      mean = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      q025 = quantile(value, 0.025, na.rm = TRUE),
      q975 = quantile(value, 0.975, na.rm = TRUE),
      q25 = quantile(value, 0.25, na.rm = TRUE),
      q75 = quantile(value, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(factor_id, time)
  
  # Color palette for factors
  factor_colors <- c("#4E79A7", "#E15759", "#59A14F", "#F28E2C", "#76B7B2")
  
  # Create combined evolution plot
  p <- ggplot(factor_summary, aes(x = time, color = factor_id, fill = factor_id)) +
    # 95% credible intervals
    geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.2, color = NA) +
    # Posterior means
    geom_line(aes(y = mean), linewidth = 1.2) +
    # Zero reference line
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.7) +
    scale_color_manual(values = factor_colors[1:length(factors_to_show)]) +
    scale_fill_manual(values = factor_colors[1:length(factors_to_show)]) +
    labs(
      title = "Latent Factors Evolution",
      subtitle = "Posterior means with 95% credible intervals from MCMC samples",
      x = "Time",
      y = "Factor Value",
      color = "Factor",
      fill = "Factor"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey60"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  return(p)
}
