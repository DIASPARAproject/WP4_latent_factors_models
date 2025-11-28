# ==============================================================================
# STEP 1: PERFORM PCA TO DISCOVER DATA STRUCTURE
# ==============================================================================

# Center the data (PCA requires centered data)
x_centered <- scale(x, center = TRUE, scale = FALSE)

# Perform PCA
# Note: prcomp works on rows as observations, columns as variables
# So our time points are observations, series are variables
pca_result <- prcomp(x_centered, center = FALSE, scale. = FALSE)

# Extract PCA components
pca_loadings <- pca_result$rotation[, 1:K]  # How each series loads on each PC
pca_factors <- pca_result$x[, 1:K]          # Time-varying PC scores

# Print PCA summary
cat("=== PCA RESULTS ===\n")
variance_explained <- summary(pca_result)$importance[2, 1:K] * 100
cumulative_variance <- sum(variance_explained)
cat("Variance explained by each PC:\n")
for (k in 1:K) {
  cat(sprintf("  PC%d: %.1f%%\n", k, variance_explained[k]))
}
cat(sprintf("Cumulative variance: %.1f%%\n\n", cumulative_variance))

# Display loadings matrix
cat("PCA Loadings Matrix (Series × Factors):\n")
loadings_df <- as.data.frame(round(pca_loadings, 3))
colnames(loadings_df) <- paste0("PC", 1:K)
rownames(loadings_df) <- labels
print(loadings_df)
cat("\n")

# ==============================================================================
# STEP 2: ASSIGN SERIES TO PRIMARY FACTORS
# ==============================================================================

# For each series, identify which factor it loads most strongly on
# This creates natural groupings based on data structure
primary_factor <- apply(abs(pca_loadings), 1, which.max)

# Count series per factor
series_per_factor <- table(primary_factor)

cat("=== SERIES ASSIGNMENT TO PRIMARY FACTORS ===\n")
for (k in 1:K) {
  series_in_factor <- which(primary_factor == k)
  cat(sprintf("Factor %d: %d series (", k, length(series_in_factor)))
  cat(paste(series_in_factor, collapse = ", "))
  cat(")\n")
  
  # Show their loadings
  if (length(series_in_factor) > 0) {
    for (i in series_in_factor) {
      cat(sprintf("  Series %2d: loading = %6.3f\n", i, pca_loadings[i, k]))
    }
  }
}
cat("\n")

# ==============================================================================
# STEP 3: CREATE OPTIMIZED TRIANGULAR MASK (MINIMIZE ZEROS)
# ==============================================================================

# NEW APPROACH: Ensure exactly k zeros in column k (minimum for identification)
# This maximizes the number of estimated loadings while maintaining identifiability

create_optimized_triangular_mask <- function(pca_loadings, K, nb_series) {
  mask <- matrix(1, nrow = nb_series, ncol = K) 
  
  for (k in 1:K) {
    # Column k needs exactly (k-1) zeros for identification
    n_zeros_needed = k
    
    if (n_zeros_needed > 0) {
      # Find series that load weakly on this factor (candidates for zeros)
      loadings_abs <- abs(pca_loadings[, k])
      
      # Sort series by their loading strength (ascending = weakest first)
      weakest_series <- order(loadings_abs)
      
      # Set the (k-1) weakest loadings to zero
      zeros_indices <- weakest_series[1:n_zeros_needed]
      mask[zeros_indices, k] <- 0
    }
    
  }
  
  return(mask)
}

triangular_mask <- create_optimized_triangular_mask(pca_loadings, K, ncol(x))

# Show which series are set to zero for each factor
cat("=== SERIES WITH ZERO LOADINGS (WEAKEST LOADINGS) ===\n")
for (k in 1:K) {
  zero_series <- which(triangular_mask[, k] == 0)
  if (length(zero_series) > 0) {
    cat(sprintf("Factor %d: Series ", k))
    for (i in zero_series) {
      cat(sprintf("%d (loading=%.3f) ", i, pca_loadings[i, k]))
    }
    cat("\n")
  } else {
    cat(sprintf("Factor %d: No zeros (all series estimated)\n", k))
  }
}
print(triangular_mask)

# ==============================================================================
# STEP 4: CREATE POSITIVE CONSTRAINT MASK
# ==============================================================================
# Only choose positive constraint from ALLOWED loadings (mask == 1)
create_positive_mask <- function(pca_loadings, triangular_mask, K) {
  positive_mask <- matrix(0, nrow = nrow(pca_loadings), ncol = K)
  used_series <- c()
  
  for (k in 1:K) {
    # Get series that are ALLOWED to load on factor k (not fixed at zero)
    allowed_series <- which(triangular_mask[, k] == 1)
    available_series <- setdiff(allowed_series, used_series)
    
    if (length(available_series) > 0) {
      # Among allowed series, find the one with strongest loading
      loadings_abs <- abs(pca_loadings[available_series, k])
      strongest_idx <- available_series[which.max(loadings_abs)]
      
      # Set positive constraint on the strongest ALLOWED loading
      positive_mask[strongest_idx, k] <- 1
      used_series <- c(used_series, strongest_idx)
      cat(sprintf("Factor %d: Positive constraint on Series %d (loading=%.3f)\n", 
                  k, strongest_idx, pca_loadings[strongest_idx, k]))
    } else {
      warning(sprintf("Factor %d has no allowed loadings! Check triangular mask.\n", k))
    }
  }
  
  return(positive_mask)
}
positive_mask <- create_positive_mask(pca_loadings, triangular_mask, K)
# ==============================================================================
# STEP 6: VISUALIZE MASKS AND PCA STRUCTURE
# ==============================================================================

# Triangular mask visualization - FIX: convert to numeric
mask_df_plot <- melt(triangular_mask)
colnames(mask_df_plot) <- c("Series", "Factor", "Allowed")
# Convert factors to numeric
mask_df_plot$Series <- as.numeric(mask_df_plot$Series)
mask_df_plot$Factor <- as.numeric(mask_df_plot$Factor)

ggplot(mask_df_plot, aes(x = Factor, y = Series, fill = factor(Allowed))) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_manual(values = c("0" = "gray90", "1" = "steelblue"),
                    labels = c("0" = "Fixed at 0", "1" = "Estimated"),
                    name = "Loading") +
  scale_y_reverse(breaks = 1:ncol(x)) +
  scale_x_continuous(breaks = 1:K) +
  labs(title = "Strict Triangular Mask (PCA-Informed)", 
       x = "Factor", y = "Series") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        legend.position = "right")


# Bar plots of loadings by factor
par(mfrow = c(1, K), mar = c(4, 4, 3, 1))
for (k in 1:K) {
  # Color bars based on whether they're in the mask
  bar_colors <- ifelse(triangular_mask[, k] == 1, "steelblue", "gray90")
  border_colors <- ifelse(triangular_mask[, k] == 1, "steelblue", "gray70")
  
  barplot(pca_loadings[, k], 
          main = paste0("PCA Loadings - Factor ", k),
          ylab = "Loading", 
          xlab = "Series",
          col = bar_colors,
          border = border_colors,
          names.arg = 1:ncol(x))
  abline(h = 0, lty = 2, col = "black")
  
  # Add legend on first plot
  if (k == 1) {
    legend("topright", 
           legend = c("Estimated", "Fixed at 0"),
           fill = c("steelblue", "gray90"),
           border = c("steelblue", "gray70"),
           bty = "n", cex = 0.8)
  }
}
par(mfrow = c(1, 1))

# ==============================================================================
# STEP 7: APPLY CONSTRAINTS TO PCA LOADINGS
# ==============================================================================

# Apply triangular mask to PCA loadings
pca_loadings_constrained <- pca_loadings * triangular_mask

pca_factors <- pca_factors
for (k in 1:K) {
  pca_factors[, k] <- pca_factors[, k] - pca_factors[1, k]
}

cat("=== FACTORS CENTERED AT t=1 ===\n")
cat("First values of each factor:\n")
for (k in 1:K) {
  cat(sprintf("  Factor %d at t=1: %.6f (should be ~0)\n", 
              k, pca_factors[1, k]))
}
cat("Constrained loadings applied. Sign identification enforced.\n\n")

# ==============================================================================
# STEP 8: COMPUTE EMPIRICAL STANDARD DEVIATIONS
# ==============================================================================

# Extract empirical SDs from PCA for initialization
sd_factor_init <- apply(pca_factors, 2, sd)
sd_x_init <- apply(x - colMeans(x), 2, sd)

cat("=== EMPIRICAL STANDARD DEVIATIONS ===\n")
cat("Factor SDs (from PCA):\n")
for (k in 1:K) {
  cat(sprintf("  Factor %d: %.3f\n", k, sd_factor_init[k]))
}
cat(sprintf("\nMean series SD: %.3f\n", mean(sd_x_init)))
cat(sprintf("Range: [%.3f, %.3f]\n\n", min(sd_x_init), max(sd_x_init)))

# ==============================================================================
# STEP 9: VISUALIZE INITIAL VALUES
# ==============================================================================

cat("=== VISUALIZATION OF INITIAL VALUES ===\n")

# Plot initial factors (from PCA)
par(mfrow = c(K, 1), mar = c(3, 4, 2, 1))
for (k in 1:K) {
  plot(time_series[[1]], pca_factors[, k], 
       type = "l", lwd = 2, col = "steelblue",
       ylab = paste0("Factor ", k), 
       xlab = if (k == K) "Time" else "",
       main = paste0("Initial Factor ", k, " (from PCA)"))
  abline(h = 0, lty = 2, col = "gray50")
  grid(col = "gray90")
}
par(mfrow = c(1, 1))

# Plot initial constrained loadings
par(mfrow = c(1, K), mar = c(4, 4, 3, 1))
for (k in 1:K) {
  bar_colors <- ifelse(triangular_mask[, k] == 1, "steelblue", "gray90")
  border_colors <- ifelse(triangular_mask[, k] == 1, "steelblue", "gray70")
  
  barplot(pca_loadings_constrained[, k], 
          main = paste0("Initial Loadings - Factor ", k),
          ylab = "Loading", 
          xlab = "Series",
          col = bar_colors,
          border = border_colors,
          names.arg = 1:ncol(x))
  abline(h = 0, lty = 2)
}
par(mfrow = c(1, 1))
