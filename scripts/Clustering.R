# ==============================================================================
# FACTOR-BASED REGIONAL CLUSTERING WITH UNCERTAINTY PROPAGATION
# ==============================================================================
# This script performs the following tasks:
# 1. Extracts posterior samples (factor loadings, latent factors, intercepts) 
#    from an MCMC object.
# 2. Structures the samples into matrices for factor loadings (Z) and latent 
#    factors (f).
# 3. Computes posterior means of loadings and factors for clustering.
# 4. Applies hierarchical clustering to group labels by similarity in factor 
#    loadings, with optimal cluster determination.
# 5. Computes cluster centroids and cluster-specific intercepts.
# 6. Propagates uncertainty to generate cluster-level predictions over time.
# 7. Produces and saves a suite of publication-ready visualizations.
# ==============================================================================

# ==============================================================================
# 1. DATA EXTRACTION AND ORGANIZATION
# ==============================================================================

# --- Extract posterior samples for each parameter of interest from the MCMC ---
# lambda = factor loadings (labels × factor)
# factor = latent factors (factor × time)
# mu_x   = intercepts per labels
lambda_data <- extract_posterior_samples2(mcmc, "lambda\\[")
factor_data <- extract_posterior_samples2(mcmc, "factor\\[")
mu_data     <- extract_posterior_samples2(mcmc, "mu_x\\[")

# --- Define dataset dimensions ---
n_series <- length(labels)    # Number of labels (time series)
n_iters  <- nrow(lambda_data)  # Number of MCMC iterations
n_factors <- K                 # Number of latent factors
n_years  <- nrow(time_series)  # Number of years in the dataset


# --- Initialize arrays to structure posterior samples ---
# Z = [iterations × labels × factors] : factor loadings
# f = [iterations × factors × years]   : latent factors
Z <- array(NA, dim = c(n_iters, n_series, n_factors))
f <- array(NA, dim = c(n_iters, n_factors, n_years))

# --- Reshape flat posterior samples into structured matrices ---
# This loop organizes the MCMC outputs into arrays with clear indexing:
# - For each iteration, factor loadings are arranged labels × factor
# - Latent factors are arranged factor × year
for (i in 1:n_iters) {
  Z[i,,] <- matrix(lambda_data[i,], nrow = n_series, ncol = n_factors, byrow = FALSE)
  f[i,,] <- matrix(factor_data[i,], nrow = n_factors, ncol = n_years, byrow = FALSE)
}

# --- Compute posterior means across iterations ---
# These mean estimates are used for clustering and interpretation
Z_mean <- apply(Z, c(2, 3), mean) # labels × factor average loadings
f_mean <- apply(f, c(2, 3), mean) # Factor × year average trajectories

# --- Add informative row/column names ---
rownames(Z_mean) <- labels
colnames(Z_mean) <- paste0("Factor", 1:n_factors)

# ==============================================================================
# 2. OPTIMAL CLUSTERING
# ==============================================================================

# --- Perform hierarchical clustering using Ward's method ---
hclust_result <- hclust(dist(Z_mean), method = "ward.D2")

# --- Determine optimal number of clusters ---
# NbClust tests multiple indices and provides a consensus recommendation
set.seed(123)  # Ensure reproducibility of NbClust results
max_k <- min(10, n_series - 1)  # Avoid testing more clusters than labels
nbclust_result <- NbClust(Z_mean, distance = "euclidean", min.nc = 2, 
                          max.nc = max_k, method = "ward.D2")

optimal_k <- as.numeric(names(sort(table(nbclust_result$Best.nc[1, ]), decreasing = TRUE)[1]))  # Extract the chosen number of clusters
optimal_k = 3
clusters  <- cutree(hclust_result, k = optimal_k)  # Assign labels to clusters
cat("Optimal number of clusters:", optimal_k, "\n")

# ==============================================================================
# 3. CENTROIDS AND INTERCEPTS
# ==============================================================================

# --- Compute cluster centroids ---
# Centroids represent the "average factor loading profile" of labels in each cluster.
# Here, we use the *median* for robustness against outliers.
cluster_data <- as.data.frame(Z_mean) %>%
  mutate(cluster = factor(clusters))

centroids_matrix <- cluster_data %>%
  group_by(cluster) %>%
  summarise(across(starts_with("Factor"), median), .groups = "drop") %>%
  column_to_rownames("cluster") %>%
  as.matrix()

# --- Compute cluster-specific intercepts ---
# Intercepts capture baseline shifts per cluster (averaged over its labels).
colnames(mu_data) <- labels

cluster_intercepts <- as.data.frame(mu_data) %>%
  mutate(iteration = row_number()) %>%
  pivot_longer(-iteration, names_to = "labels", values_to = "mu") %>%
  mutate(cluster = factor(clusters[labels])) %>%
  group_by(cluster) %>%
  summarise(mu_cluster = mean(mu), .groups = "drop")

cluster_colors <- brewer.pal(max(3, min(optimal_k, 8)), "Set1")[1:optimal_k]

# ==============================================================================
# 4. PREDICTIONS WITH UNCERTAINTY PROPAGATION
# ==============================================================================

# --- Initialize prediction array ---
# prediction_samples = [iterations × clusters × years]
prediction_samples <- array(NA, dim = c(n_iters, optimal_k, n_years))

# --- Generate predictions for each iteration ---
# Multiply cluster centroids (cluster × factor) with latent factors (factor × year)
# to reconstruct time series at the cluster level.
for (i in 1:n_iters) {
  prediction_samples[i,,] <- centroids_matrix %*% f[i,,]
}

# --- Add cluster intercepts (shifting predictions) ---
for (cl in 1:optimal_k) {
  prediction_samples[, cl, ] <- prediction_samples[, cl, ] + cluster_intercepts$mu_cluster[cl]
}

# --- Summarize predictions with credible intervals ---
predictions_tidy <- expand.grid(year = years, cluster = factor(1:optimal_k)) %>%
  mutate(
    mean  = as.vector(t(apply(prediction_samples, c(2,3), mean))),      # Posterior mean
    lower = as.vector(t(apply(prediction_samples, c(2,3), quantile, 0.05))), # 5% bound
    upper = as.vector(t(apply(prediction_samples, c(2,3), quantile, 0.95)))  # 95% bound
  )

# ==============================================================================
# 5. VISUALIZATIONS
# ==============================================================================
clustering_plots <- list()

# --- Plot 1: Cluster centroids (factor loading patterns) ---
centroids_long <- centroids_matrix %>%
  as.data.frame() %>%
  mutate(cluster = factor(row_number())) %>%
  pivot_longer(-cluster, names_to = "Factor", values_to = "Loading")

p1 <- ggplot(centroids_long, aes(x = Factor, y = Loading, color = cluster, group = cluster)) +
  geom_line(size = 1.5) + geom_point(size = 3) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(title = "Cluster Centroids: Factor Loading Patterns",
       x = "Latent Factor", y = "Loading Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p1)

par(default_par)

# --- Plot 2: Dendrogram with colored branches ---
# Visual representation of hierarchical clustering, with clusters colored.

clustering_plots[["dendrogram"]] <- function() {
  dendrogram <- as.dendrogram(hclust_result)
  colored_dend <- dendrogram %>%
    color_branches(k = optimal_k, col = cluster_colors) %>%
    color_labels(k = optimal_k, col = cluster_colors)
  
  plot(colored_dend, main = paste("Regional Clustering (k =", optimal_k, ")"))
  legend("topright", legend = paste("Cluster", 1:optimal_k), 
         fill = cluster_colors, bty = "n")
}

# --- Plot 3: Cluster-level predictions over time ---

cluster_df <- tibble(label = names(clusters),
                     cluster = as.factor(clusters))

ts_long_clustered <- ts_long %>%
  left_join(cluster_df, by = "label")

p2 <- ggplot(predictions_tidy, aes(x = year, group = cluster)) +
  geom_line(data = ts_long_clustered, aes(x = year, y = values, group = label), 
  color = "gray70", alpha = 0.6, size = 0.6) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = cluster), alpha = 0.3) +
  geom_line(aes(y = mean, color = cluster), size = 1.2) +
  facet_wrap(~ cluster, labeller = labeller(cluster = function(x) paste("Cluster", x))) +
  scale_color_manual(values = cluster_colors, guide = "none") +
  scale_fill_manual(values = cluster_colors, guide = "none") +
  labs(title = "Cluster Predictions",
       x = "Year", y = "Predicted Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        strip.text = element_text(face = "bold"))
print(p2)


clustering_plots[["cluster_centroids"]] <- p1
clustering_plots[["cluster_predictions"]] <- p2

# ==============================================================================
# 5 bis. SAVE PLOTS
# ==============================================================================

if (save_plots) {
  # Create output directory if it doesn't exist
  clustering_dir <- file.path(path_results, "Clustering")
  if (!dir.exists(clustering_dir)) dir.create(clustering_dir, recursive = TRUE)
  
  cat("Step 6: Saving clustering plots...\n")
  
  for (plot_name in names(clustering_plots)) {
    file_path <- file.path(clustering_dir, paste0(plot_name, ".png"))
    
    if (inherits(clustering_plots[[plot_name]], "ggplot")) {
      ggsave(filename = file_path,
             plot = clustering_plots[[plot_name]],
             width = DIAGNOSTIC_CONFIG$plot_width,
             height = DIAGNOSTIC_CONFIG$plot_height,
             dpi = 300,
             bg = "white")
    } else if (is.function(clustering_plots[[plot_name]])) {
      # For base R plots like dendrogram
      png(filename = file_path, width = DIAGNOSTIC_CONFIG$plot_width*300,
          height = DIAGNOSTIC_CONFIG$plot_height*300, res = 300)
      clustering_plots[[plot_name]]()  # Call the plotting function
      dev.off()
    }
  }
  
  cat("  ✓ Clustering plots saved to:", clustering_dir, "\n")
}


# ==============================================================================
# 6. RESULTS SUMMARY
# ==============================================================================

# --- Cluster sizes ---
cluster_sizes <- table(clusters)
cat("\n=== CLUSTERING SUMMARY ===\n")
cat("labels:", n_series, "| Factors:", n_factors, "| Clusters:", optimal_k, "\n")
cat("MCMC iterations:", n_iters, "| Years:", length(years), "\n\n")

cat("Cluster sizes:\n")
for(i in 1:optimal_k) {
  cat("Cluster", i, ":", cluster_sizes[i], "labels\n")
}

# --- Centroid loadings (rounded for readability) ---
cat("\nCentroid loadings:\n")
print(round(centroids_matrix, 3))

# --- Cluster intercepts (rounded for readability) ---
cat("\nCluster intercepts:\n")
print(cluster_intercepts %>% mutate(mu_cluster = round(mu_cluster, 3)))

