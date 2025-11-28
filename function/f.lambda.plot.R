# -------------------------------
# IMPROVED LAMBDA DISTRIBUTION WITH REGION NAMES (HORIZONTAL)
# -------------------------------

create_lambda_distribution_with_labels <- function(mcmc_data, factor_k, labels) {
  
  lambda_pattern <- paste0("lambda\\[.*,\\s*", factor_k, "\\]")
  lambda_data <- extract_posterior_samples(mcmc_data, lambda_pattern)
  
  if (is.null(lambda_data)) {
    return(NULL)
  }
  
  # Extract variable indices and add region names
  lambda_data$variable <- as.numeric(gsub("lambda\\[(\\d+),.*", "\\1", lambda_data$parameter))
  lambda_data$region <- labels[lambda_data$variable]
  
  # Ensure proper ordering (top to bottom)
  lambda_data$region <- factor(lambda_data$region, levels = rev(labels))
  
  # Horizontal violin/box plot
  p <- ggplot(lambda_data, aes(y = region, x = value)) +
    geom_boxplot(width = 0.3, fill = "white", outlier.shape = NA) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    labs(
      title = paste("Factor Loadings Distribution - Factor", factor_k),
      subtitle = "Full posterior distributions from MCMC samples",
      x = "Loading Value",
      y = "Region"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "grey60"),
      axis.text.y = element_text(size = 9),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
  
  return(p)
}

create_lambda_combined_plot <- function(mcmc_data, labels, n_factors = 3) {
  
  all_data <- list()
  
  for (k in 1:n_factors) {
    lambda_pattern <- paste0("lambda\\[.*,\\s*", k, "\\]")
    lambda_data <- extract_posterior_samples(mcmc_data, lambda_pattern)
    
    if (!is.null(lambda_data)) {
      # Extract variable indices and add region names
      lambda_data$variable <- as.numeric(gsub("lambda\\[(\\d+),.*", "\\1", lambda_data$parameter))
      lambda_data$region <- labels[lambda_data$variable]
      lambda_data$factor <- paste("Factor", k)
      
      all_data[[k]] <- lambda_data
    }
  }
  
  combined_data <- do.call(rbind, all_data)
  
  # Ensure proper ordering
  combined_data$region <- factor(combined_data$region, levels = rev(labels))
  combined_data$factor <- factor(combined_data$factor, 
                                 levels = paste("Factor", 1:n_factors))
  
  # Plot facet_wrap
  p <- ggplot(combined_data, aes(y = region, x = value)) +
    geom_boxplot(width = 0.4, fill = "white", outlier.shape = NA) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    facet_wrap(~ factor, ncol = 3, scales = "free_x") +
    labs(
      title = "Factor Loadings Distribution - All Factors",
      subtitle = "Posterior distributions from MCMC samples",
      x = "Loading Value",
      y = "Region"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey60", size = 10),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(size = 8),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.text = element_text(face = "bold", size = 11)
    )
  
  return(p)
}
