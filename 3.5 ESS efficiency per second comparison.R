# ==============================================================================
# MCMC EFFICIENCY CALCULATION: ESS PER PARAMETER AS A FUNCTION OF TIME
# For Dynamic Factor Analysis (DFA) NIMBLE Model
# ==============================================================================

library(coda)
library(ggplot2)
library(dplyr)
library(tidyr)

# ------------------------------------------------------------------------------
# 1. MAIN FUNCTION: Calculate ESS/time for each parameter
# ------------------------------------------------------------------------------

calculate_ess_efficiency <- function(mcmc_samples, computation_time_seconds, 
                                     model_name = "DFA Model") {
  #' Calculate ESS per second for each parameter
  #' 
  #' @param mcmc_samples: mcmc.list object from NIMBLE
  #' @param computation_time_seconds: computation time in seconds
  #' @param model_name: model name for identification
  #' @return data.frame with ESS, time, and efficiency per parameter
  
  # Convert to mcmc.list if necessary
  if (!inherits(mcmc_samples, "mcmc.list")) {
    mcmc_samples <- as.mcmc.list(list(as.mcmc(mcmc_samples)))
  }
  
  # Calculate ESS for each parameter
  ess_values <- effectiveSize(mcmc_samples)
  
  # Total number of iterations
  n_iter <- nrow(mcmc_samples[[1]]) * length(mcmc_samples)
  
  # Create results data.frame
  results <- data.frame(
    parameter = names(ess_values),
    ess = as.numeric(ess_values),
    n_iterations = n_iter,
    time_seconds = computation_time_seconds,
    time_minutes = computation_time_seconds / 60,
    time_hours = computation_time_seconds / 3600,
    model = model_name,
    stringsAsFactors = FALSE
  )
  
  # Calculate efficiency metrics
  results <- results %>%
    mutate(
      ess_per_second = ess / time_seconds,
      ess_per_minute = ess / time_minutes,
      ess_per_hour = ess / time_hours,
      ess_per_iteration = ess / n_iterations,
      iterations_per_second = n_iterations / time_seconds
    )
  
  return(results)
}

# ------------------------------------------------------------------------------
# 2. FUNCTION: Visualization
# ------------------------------------------------------------------------------

plot_ess_efficiency <- function(efficiency_data, top_n = 20) {
  #' Create plots to visualize MCMC efficiency
  #' 
  #' @param efficiency_data: data.frame from calculate_ess_efficiency
  #' @param top_n: number of parameters to display (most important ones)
  
  # Select top parameters by mean ESS
  top_params <- efficiency_data %>%
    group_by(parameter) %>%
    summarise(mean_ess = mean(ess)) %>%
    arrange(desc(mean_ess)) %>%
    head(top_n) %>%
    pull(parameter)
  
  data_plot <- efficiency_data %>%
    filter(parameter %in% top_params)
  
  # Plot 1: ESS per second (main efficiency metric)
  p1 <- ggplot(data_plot, aes(x = reorder(parameter, ess_per_second), 
                              y = ess_per_second, fill = model)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(title = "MCMC Efficiency: ESS per Second",
         subtitle = paste("Top", top_n, "parameters by ESS"),
         x = "Parameter",
         y = "ESS / second",
         fill = "Model") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"))
  
  # Plot 2: ESS total (absolute sampling quality)
  p2 <- ggplot(data_plot, aes(x = reorder(parameter, ess), 
                              y = ess, fill = model)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(title = "Effective Sample Size by Parameter",
         subtitle = paste("Top", top_n, "parameters"),
         x = "Parameter",
         y = "Effective Sample Size (ESS)",
         fill = "Model") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"))
  
  # Plot 3: Efficiency distribution
  p3 <- ggplot(efficiency_data, aes(x = model, y = ess_per_second, fill = model)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
    geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
    labs(title = "Efficiency Distribution Across All Parameters",
         x = "Model",
         y = "ESS / second") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold"))
  
  # Plot 4: ESS/iterations ratio (sampling efficiency independent of time)
  p4 <- ggplot(data_plot, aes(x = reorder(parameter, ess_per_iteration), 
                              y = ess_per_iteration, fill = model)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(title = "Sampling Efficiency",
         subtitle = "ESS per MCMC iteration (mixing quality)",
         x = "Parameter",
         y = "ESS / iteration",
         fill = "Model") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"))
  
  return(list(
    ess_per_second = p1,
    ess_total = p2,
    efficiency_distribution = p3,
    ess_per_iteration = p4
  ))
}

# ------------------------------------------------------------------------------
# 3. FUNCTION: Summary table
# ------------------------------------------------------------------------------

summarize_efficiency <- function(efficiency_data) {
  #' Create summary table of efficiency by model
  #' 
  #' @param efficiency_data: data.frame from calculate_ess_efficiency
  #' @return data.frame with aggregated statistics
  
  summary <- efficiency_data %>%
    group_by(model) %>%
    summarise(
      n_parameters = n(),
      total_time_hours = round(first(time_hours), 2),
      mean_ess = round(mean(ess), 1),
      median_ess = round(median(ess), 1),
      min_ess = round(min(ess), 1),
      mean_ess_per_sec = round(mean(ess_per_second), 3),
      median_ess_per_sec = round(median(ess_per_second), 3),
      total_iterations = first(n_iterations),
      iter_per_sec = round(first(iterations_per_second), 2)
    ) %>%
    arrange(desc(mean_ess_per_sec))
  
  return(summary)
}

# ------------------------------------------------------------------------------
# 4. DIRECT APPLICATION TO YOUR DFA MODEL
# ------------------------------------------------------------------------------

# Load your MCMC results (created by your parallel script)
mcmc <- readRDS(file.path(path_results, "mcmc_samples.rds"))

# Calculate computation time from your script output
# (end_time - start_time already calculated in your script)
computation_time_seconds <- as.numeric(execution_time, units = "secs")
computation_time_seconds = 484.34
# Alternative if execution_time not saved: manually input time in seconds
# computation_time_seconds <- 7200  # e.g., 2 hours = 7200 seconds

# Calculate efficiency
cat("\nCalculating ESS efficiency metrics...\n")
efficiency <- calculate_ess_efficiency(
  mcmc_samples = mcmc,
  computation_time_seconds = computation_time_seconds,
  model_name = "DFA-HMC"
)

# Display summary table
cat("\n=================================================================\n")
cat("MCMC EFFICIENCY SUMMARY\n")
cat("=================================================================\n\n")
summary_table <- summarize_efficiency(efficiency)
print(summary_table)

# Create plots
cat("\nGenerating efficiency plots...\n")
plots <- plot_ess_efficiency(efficiency, top_n = 15)

# Save plots to files
ggsave(filename = file.path(path_results, "ess_per_second.png"), 
       plot = plots$ess_per_second, 
       width = 10, height = 8, dpi = 300)

ggsave(filename = file.path(path_results, "ess_total.png"), 
       plot = plots$ess_total, 
       width = 10, height = 8, dpi = 300)

ggsave(filename = file.path(path_results, "efficiency_distribution.png"), 
       plot = plots$efficiency_distribution, 
       width = 8, height = 6, dpi = 300)

ggsave(filename = file.path(path_results, "ess_per_iteration.png"), 
       plot = plots$ess_per_iteration, 
       width = 10, height = 8, dpi = 300)

cat("✓ Plots saved to:", path_results, "\n")

# Display plots
print(plots$ess_per_second)
print(plots$ess_total)
print(plots$efficiency_distribution)
print(plots$ess_per_iteration)

# Save efficiency data for future comparison
saveRDS(efficiency, file = file.path(path_results, "ess_efficiency_data.rds"))
saveRDS(summary_table, file = file.path(path_results, "ess_efficiency_summary.rds"))

cat("\n=================================================================\n")
cat("EFFICIENCY ANALYSIS COMPLETE\n")
cat("=================================================================\n\n")
cat("Key efficiency metrics:\n")
cat("  Mean ESS per second:", round(summary_table$mean_ess_per_sec, 3), "\n")
cat("  Median ESS per second:", round(summary_table$median_ess_per_sec, 3), "\n")
cat("  Total parameters analyzed:", summary_table$n_parameters, "\n")
cat("  Computation time:", summary_table$total_time_hours, "hours\n\n")

# ==============================================================================
# MCMC EFFICIENCY CALCULATION: ESS PER PARAMETER AS A FUNCTION OF TIME
# For Dynamic Factor Analysis (DFA) NIMBLE Model
# ==============================================================================

library(coda)
library(ggplot2)
library(dplyr)
library(tidyr)

# ------------------------------------------------------------------------------
# 1. MAIN FUNCTION: Calculate ESS/time for each parameter
# ------------------------------------------------------------------------------

calculate_ess_efficiency <- function(mcmc_samples, computation_time_seconds, 
                                     model_name = "DFA Model") {
  #' Calculate ESS per second for each parameter
  #' 
  #' @param mcmc_samples: mcmc.list object from NIMBLE
  #' @param computation_time_seconds: computation time in seconds
  #' @param model_name: model name for identification
  #' @return data.frame with ESS, time, and efficiency per parameter
  
  # Convert to mcmc.list if necessary
  if (!inherits(mcmc_samples, "mcmc.list")) {
    mcmc_samples <- as.mcmc.list(list(as.mcmc(mcmc_samples)))
  }
  
  # Calculate ESS for each parameter
  ess_values <- effectiveSize(mcmc_samples)
  
  # Total number of iterations
  n_iter <- nrow(mcmc_samples[[1]]) * length(mcmc_samples)
  
  # Create results data.frame
  results <- data.frame(
    parameter = names(ess_values),
    ess = as.numeric(ess_values),
    n_iterations = n_iter,
    time_seconds = computation_time_seconds,
    time_minutes = computation_time_seconds / 60,
    time_hours = computation_time_seconds / 3600,
    model = model_name,
    stringsAsFactors = FALSE
  )
  
  # Calculate efficiency metrics
  results <- results %>%
    mutate(
      ess_per_second = ess / time_seconds,
      ess_per_minute = ess / time_minutes,
      ess_per_hour = ess / time_hours,
      ess_per_iteration = ess / n_iterations,
      iterations_per_second = n_iterations / time_seconds
    )
  
  return(results)
}

# ------------------------------------------------------------------------------
# 2. FUNCTION: Visualization
# ------------------------------------------------------------------------------

plot_ess_efficiency <- function(efficiency_data, top_n = 20) {
  #' Create plots to visualize MCMC efficiency
  #' 
  #' @param efficiency_data: data.frame from calculate_ess_efficiency
  #' @param top_n: number of parameters to display (most important ones)
  
  # Select top parameters by mean ESS
  top_params <- efficiency_data %>%
    group_by(parameter) %>%
    summarise(mean_ess = mean(ess)) %>%
    arrange(desc(mean_ess)) %>%
    head(top_n) %>%
    pull(parameter)
  
  data_plot <- efficiency_data %>%
    filter(parameter %in% top_params)
  
  # Plot 1: ESS per second (main efficiency metric)
  p1 <- ggplot(data_plot, aes(x = reorder(parameter, ess_per_second), 
                              y = ess_per_second, fill = model)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(title = "MCMC Efficiency: ESS per Second",
         subtitle = paste("Top", top_n, "parameters by ESS"),
         x = "Parameter",
         y = "ESS / second",
         fill = "Model") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"))
  
  # Plot 2: ESS total (absolute sampling quality)
  p2 <- ggplot(data_plot, aes(x = reorder(parameter, ess), 
                              y = ess, fill = model)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(title = "Effective Sample Size by Parameter",
         subtitle = paste("Top", top_n, "parameters"),
         x = "Parameter",
         y = "Effective Sample Size (ESS)",
         fill = "Model") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"))
  
  # Plot 3: Efficiency distribution
  p3 <- ggplot(efficiency_data, aes(x = model, y = ess_per_second, fill = model)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
    geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
    labs(title = "Efficiency Distribution Across All Parameters",
         x = "Model",
         y = "ESS / second") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold"))
  
  # Plot 4: ESS/iterations ratio (sampling efficiency independent of time)
  p4 <- ggplot(data_plot, aes(x = reorder(parameter, ess_per_iteration), 
                              y = ess_per_iteration, fill = model)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(title = "Sampling Efficiency",
         subtitle = "ESS per MCMC iteration (mixing quality)",
         x = "Parameter",
         y = "ESS / iteration",
         fill = "Model") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"))
  
  return(list(
    ess_per_second = p1,
    ess_total = p2,
    efficiency_distribution = p3,
    ess_per_iteration = p4
  ))
}

# ------------------------------------------------------------------------------
# 3. FUNCTION: Summary table
# ------------------------------------------------------------------------------

summarize_efficiency <- function(efficiency_data) {
  #' Create summary table of efficiency by model
  #' 
  #' @param efficiency_data: data.frame from calculate_ess_efficiency
  #' @return data.frame with aggregated statistics
  
  summary <- efficiency_data %>%
    group_by(model) %>%
    summarise(
      n_parameters = n(),
      total_time_hours = round(first(time_hours), 2),
      mean_ess = round(mean(ess), 1),
      median_ess = round(median(ess), 1),
      min_ess = round(min(ess), 1),
      mean_ess_per_sec = round(mean(ess_per_second), 3),
      median_ess_per_sec = round(median(ess_per_second), 3),
      total_iterations = first(n_iterations),
      iter_per_sec = round(first(iterations_per_second), 2)
    ) %>%
    arrange(desc(mean_ess_per_sec))
  
  return(summary)
}

# ------------------------------------------------------------------------------
# 4. DIRECT APPLICATION TO YOUR DFA MODEL
# ------------------------------------------------------------------------------

# Load your MCMC results (created by your parallel script)
mcmc <- readRDS(file.path(path_results, "mcmc_samples.rds"))

# Calculate computation time from your script output
# (end_time - start_time already calculated in your script)
computation_time_seconds <- as.numeric(execution_time, units = "secs")

# Alternative if execution_time not saved: manually input time in seconds
# computation_time_seconds <- 7200  # e.g., 2 hours = 7200 seconds

# Calculate efficiency
cat("\nCalculating ESS efficiency metrics...\n")
efficiency <- calculate_ess_efficiency(
  mcmc_samples = mcmc,
  computation_time_seconds = computation_time_seconds,
  model_name = "DFA-HMC"
)

# Display summary table
cat("\n=================================================================\n")
cat("MCMC EFFICIENCY SUMMARY\n")
cat("=================================================================\n\n")
summary_table <- summarize_efficiency(efficiency)
print(summary_table)

# Display plots
print(plots$ess_per_second)
print(plots$ess_total)
print(plots$efficiency_distribution)
print(plots$ess_per_iteration)

# Save efficiency data for future comparison
saveRDS(efficiency, file = file.path(path_results, "ess_efficiency_data.rds"))
saveRDS(summary_table, file = file.path(path_results, "ess_efficiency_summary.rds"))

cat("\n=================================================================\n")
cat("EFFICIENCY ANALYSIS COMPLETE\n")
cat("=================================================================\n\n")
cat("Key efficiency metrics:\n")
cat("  Mean ESS per second:", round(summary_table$mean_ess_per_sec, 3), "\n")
cat("  Median ESS per second:", round(summary_table$median_ess_per_sec, 3), "\n")
cat("  Total parameters analyzed:", summary_table$n_parameters, "\n")
cat("  Computation time:", summary_table$total_time_hours, "hours\n\n")

# ------------------------------------------------------------------------------
# 5. OPTIONAL: Compare parameters by type
# ------------------------------------------------------------------------------

# Identify parameter types from names with more specific patterns
efficiency_by_type <- efficiency %>%
  mutate(
    param_type = case_when(
      grepl("^factor\\[", parameter) ~ "Latent Factors",
      grepl("^lambda_free", parameter) ~ "Free Loadings",
      grepl("^lambda_positive", parameter) ~ "Positive Loadings",
      grepl("^log_lambda_positive", parameter) ~ "Log Positive Loadings",
      grepl("^lambda\\[", parameter) ~ "Combined Loadings",
      grepl("^phi\\[", parameter) ~ "AR(1) Coefficients",
      grepl("^log_sd_factor", parameter) ~ "Log Factor SD",
      grepl("^sd_factor", parameter) ~ "Factor SD",
      grepl("^log_sd_x", parameter) ~ "Log Observation SD",
      grepl("^sd_x", parameter) ~ "Observation SD",
      grepl("^mu_x", parameter) ~ "Series Means",
      grepl("^mu_mu$", parameter) ~ "Global Mean",
      grepl("^log_sd_mu$", parameter) ~ "Log Global SD",
      grepl("^sd_mu$", parameter) ~ "Global SD",
      grepl("^E_x", parameter) ~ "Expected Values",
      grepl("^y_obs", parameter) ~ "Observations",
      grepl("phi", parameter) ~ "Phi",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(param_type) %>%
  summarise(
    n_params = n(),
    mean_ess = round(mean(ess), 1),
    sd_ess = round(sd(ess), 1),
    mean_ess_per_sec = round(mean(ess_per_second), 3),
    sd_ess_per_sec = round(sd(ess_per_second), 3),
    mean_ess_per_iter = round(mean(ess_per_iteration), 3)
  ) %>%
  arrange(desc(mean_ess_per_sec))

cat("\nEFFICIENCY BY PARAMETER TYPE:\n")
print(efficiency_by_type)

# Scientific-style plot with error bars and better aesthetics
p_by_type <- ggplot(efficiency_by_type, 
                    aes(x = reorder(param_type, mean_ess_per_sec), 
                        y = mean_ess_per_sec)) +
  geom_col(width = 0.7, alpha = 0.8) +
  geom_errorbar(aes(ymin = pmax(0, mean_ess_per_sec - sd_ess_per_sec), 
                    ymax = mean_ess_per_sec + sd_ess_per_sec),
                width = 0.3, linewidth = 0.5) +
  coord_flip() +
#  scale_fill_viridis_c(option = "plasma", direction = -1, 
#                       name = "Number of\nParameters") +
  labs(title = "HMC Sampling Efficiency by Parameter Type",
       x = NULL,
       y = "ESS per second") +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    axis.text = element_text(color = "black")
  )

print(p_by_type)

ggsave(filename = file.path(path_results, "efficiency_by_type.png"), 
       plot = p_by_type, 
       width = 10, height = 7, dpi = 300)

ggsave(filename = file.path(path_results, "efficiency_vs_nparams.png"), 
       plot = p_efficiency_scatter, 
       width = 10, height = 7, dpi = 300)

# Check if sd_mu and log_sd_mu exist
if (any(grepl("^sd_mu$", efficiency$parameter))) {
  cat("\n✓ sd_mu parameter found in efficiency data\n")
} else {
  cat("\n⚠ Warning: sd_mu not found in parameter names\n")
  cat("Available parameters matching 'sd_mu':\n")
  print(grep("sd_mu", efficiency$parameter, value = TRUE))
}

if (any(grepl("^log_sd_mu$", efficiency$parameter))) {
  cat("✓ log_sd_mu parameter found in efficiency data\n")
} else {
  cat("⚠ Warning: log_sd_mu not found in parameter names\n")
}

cat("\n✓ Analysis complete! All results saved to:", path_results, "\n")

efficiency <- readRDS("results/ess_efficiency_data.rds")
ess_efficiency_summary <- readRDS("results/ess_efficiency_summary.rds")
