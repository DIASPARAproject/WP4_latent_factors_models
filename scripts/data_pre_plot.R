cat("\n=== VISUALIZATION PHASE ===\n")

#' Create comprehensive exploratory plots
#' Shows both transformed (logit) and natural scale survival probabilities

# Transform data to long format for ggplot
cat("Generating time series plots...\n")

# Long format with natural-scales values  
ts_logit_long <- ts_with_year %>%
  pivot_longer(
    cols      = -year, 
    names_to  = "region", 
    values_to = "ts_logit"
  )

# Long format with logit/delogit-scale values (can be modified)
ts_long <- ts_logit_long %>%
  mutate(
    ts_prob = plogis(ts_logit)  # inverse logit: exp(x)/(1+exp(x))
  )

# Plot 1: Survival probabilities on natural scale (0-1)
plot_natural <- ggplot(
  ts_long, 
  aes(x = year, y = ts_prob, group = region)
) +
  geom_line(
    linewidth = 0.5, 
    color = "steelblue", 
    alpha = 0.7
  ) +
  labs(
    title = "Atlantic Salmon Post-Smolt Survival Probabilities",
    subtitle = paste("Time series for", length(labels), "populations (natural scale)"),
    x = "Year",
    y = "Survival Probability"
  ) +
  scale_y_continuous(
    limits = c(0, 0.60),
    breaks = seq(0, 1, 0.2),
    labels = scales::percent
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.5),
    strip.background = element_rect(fill = "grey95")
  )

print(plot_natural)

# Save the plot
ggsave(
  filename = file.path(path_results, "survival_probabilities_natural_scale.png"),
  plot = plot_natural,
  width = 10, height = 6, dpi = 300, bg = "white"
)

# Plot 2: Survival on logit scale (modeling scale)
plot_logit <- ggplot(
  ts_logit_long,
  aes(x = year, y = ts_logit, group = region)
) +
  geom_line(
    linewidth = 0.5,
    color = "darkred",
    alpha = 0.7
  ) +
  labs(
    title = "Atlantic Salmon Post-Smolt Survival (Logit Scale)",
    subtitle = paste("Modeling scale for", length(labels), "populations"),
    x = "Year", 
    y = "Survival (logit scale)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.5)
  )

print(plot_logit)

# Save the logit-scale plot
ggsave(
  filename = file.path(path_results, "survival_probabilities_logit_scale.png"),
  plot = plot_logit,
  width = 10, height = 6, dpi = 300, bg = "white"
)

# Optional: Create faceted plot by region for detailed inspection
if (length(labels) <= 15) {  # Only if not too many labels
  
  plot_by_region <- ggplot(
    ts_prob_long,
    aes(x = year, y = ts_prob)
  ) +
    geom_line(color = "steelblue", linewidth = 0.7) +
    facet_wrap(~ region, scales = "free_y", ncol = 5) +
    labs(
      title = "Regional Time Series: Atlantic Salmon Survival",
      x = "Year",
      y = "Survival Probability"
    ) +
    scale_y_continuous(labels = scales::percent) +
    theme_bw(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  print(plot_by_region)
  
  ggsave(
    filename = file.path(path_results, "survival_by_region.png"),
    plot = plot_by_region,
    width = 12, height = 8, dpi = 300, bg = "white"
  )
}
