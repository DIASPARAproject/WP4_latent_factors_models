# ==============================================================================
# STEP 6: MODEL COMPARISON VISUALIZATION
# ==============================================================================
# Create comparison plots showing complete vs truncated model predictions

cat("Creating comparison visualizations...\n")

# Initialize plot storage
plot_list_compare <- list()

# Generate comparison plot for first label (can be extended to all labels)
label_name <- labels[1]

# Prepare data for plotting
raw_data <- ts_long %>% 
  filter(label == label_name)

complete_summary <- Ex_summary_tot %>% 
  filter(serie == label_name)

truncated_summary <- Ex_summary_totT %>% 
  filter(serie == label_name)

# Create comparison plot
comparison_plot <- ggplot() +
  # Raw observed data (baseline)
  geom_line(data = raw_data, 
            aes(x = year, y = values),
            color = "gray40", alpha = 0.6, linewidth = 0.8,
            linetype = "dotted") +
  
  # Complete model predictions with uncertainty
  geom_ribbon(data = complete_summary, 
              aes(x = year, ymin = lower, ymax = upper),
              fill = "dodgerblue3", alpha = 0.3) +
  geom_line(data = complete_summary, 
            aes(x = year, y = mean),
            color = "dodgerblue4", linewidth = 1.2) +
  
  # Truncated model predictions with uncertainty  
  geom_ribbon(data = truncated_summary, 
              aes(x = year, ymin = lower, ymax = upper),
              fill = "firebrick3", alpha = 0.3) +
  geom_line(data = truncated_summary, 
            aes(x = year, y = mean),
            color = "firebrick", linewidth = 1.2, linetype = "dashed") +
  
  # Styling
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = paste("Model Comparison:", label_name),
    x = "Year",
    y = "Predicted Values",
    caption = "Blue: Complete Model | Red: Truncated Model | Gray: Observed Data"
  )

# Store plot
plot_list_compare[[label_name]] <- comparison_plot

ggsave(
  filename = file.path(output_directory, paste0(label_name, ".png")),
  plot = plot_list_compare[[label_name]],
  width = DIAGNOSTIC_CONFIG$plot_width,
  height = DIAGNOSTIC_CONFIG$plot_height,
  dpi = 300,
  bg = "white")
