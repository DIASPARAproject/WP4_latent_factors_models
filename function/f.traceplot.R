create_traceplot_ggplot <- function(mcmc_data, param_name, chains_colors = NULL) {
  
  if (is.null(chains_colors)) {
    chains_colors <- RColorBrewer::brewer.pal(min(8, max(3, coda::nchain(mcmc_data))), "Set2")
  }
  
  # Extract data for the specific parameter
  param_data <- mcmc_data[, param_name, drop = FALSE]
  
  # Convert to data frame for ggplot
  trace_df <- data.frame()
  
  for (chain in 1:coda::nchain(param_data)) {
    chain_data <- data.frame(
      iteration = 1:coda::niter(param_data),
      value = as.numeric(param_data[[chain]]),
      chain = factor(paste("Chain", chain))
    )
    trace_df <- rbind(trace_df, chain_data)
  }
  
  # Create the traceplot with ALL chains on the SAME plot with SAME Y-axis
  p <- ggplot(trace_df, aes(x = iteration, y = value, color = chain)) +
    geom_line(alpha = 0.8, linewidth = 0.5) +
    scale_color_manual(values = chains_colors[1:coda::nchain(param_data)]) +
    labs(
      title = paste("Traceplot:", param_name),
      x = "Iteration",
      y = "Value",
      color = "Chain"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.direction = "horizontal",
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 8)
    )
  
  
  return(p)
}