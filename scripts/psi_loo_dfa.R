# ==============================================================================
# PSIS-LOO CALCULATION WITH POST-HOC LOG-LIKELIHOOD COMPUTATION
# ==============================================================================
# This script recalculates log-likelihoods from MCMC samples and computes PSIS-LOO

# Load required packages
library(loo)        # For PSIS-LOO calculation
library(coda)       # For MCMC object handling

cat("========================================\n")
cat("PSIS-LOO with Post-hoc Log-Likelihood\n")
cat("========================================\n\n")

# ------------------------------------------------------------------------------
# LOAD MCMC SAMPLES AND DATA
# ------------------------------------------------------------------------------

cat("Loading MCMC samples and data...\n")
mcmc <- readRDS("results/mcmc_samples.rds")
data.nimble <- readRDS("data/data.rds")
const.nimble <- readRDS("data/const.rds")

cat("✓ MCMC samples loaded\n")
cat("✓ Data and constants loaded\n")

# Extract observed data and observation errors
y_obs <- data.nimble$x         # Observed data matrix (n x nb_series)
omega_obs <- data.nimble$omega_obs  # Observation standard errors (n x nb_series)

n <- const.nimble$n                 # Number of time points
nb_series <- const.nimble$nb_series # Number of time series

cat("  Data dimensions:", n, "time points x", nb_series, "series\n")

# ------------------------------------------------------------------------------
# COMBINE MCMC CHAINS
# ------------------------------------------------------------------------------

cat("\nCombining MCMC chains...\n")

# Convert mcmc.list to single matrix (all chains combined)
all_samples <- do.call(rbind, lapply(mcmc, as.matrix))
n_iter <- nrow(all_samples)

cat("✓ Combined", length(mcmc), "chains\n")
cat("  Total iterations:", n_iter, "\n")

# ------------------------------------------------------------------------------
# EXTRACT LATENT STATE SAMPLES
# ------------------------------------------------------------------------------

cat("\nExtracting latent state samples (x)...\n")

# Extract all x[t,i] columns from MCMC samples
x_cols <- grep("^x\\[", colnames(all_samples), value = TRUE)

if(length(x_cols) == 0) {
  stop("ERROR: No x[t,i] samples found. Ensure 'x' is monitored in MCMC.")
}

# Verify we have all expected x values
expected_x_cols <- n * nb_series
if(length(x_cols) != expected_x_cols) {
  warning("Expected ", expected_x_cols, " x columns but found ", length(x_cols))
}

cat("✓ Extracted", length(x_cols), "latent state columns\n")

# ------------------------------------------------------------------------------
# COMPUTE LOG-LIKELIHOOD FOR EACH MCMC ITERATION
# ------------------------------------------------------------------------------

cat("\nCalculating log-likelihoods for each MCMC iteration...\n")
cat("This may take several minutes...\n\n")

start_time <- Sys.time()

# Initialize log-likelihood matrix
# Rows = MCMC iterations, Columns = observations (flattened t,i pairs)
n_obs <- n * nb_series
log_lik_matrix <- matrix(NA, nrow = n_iter, ncol = n_obs)

# Progress tracking
pb <- txtProgressBar(min = 0, max = n_iter, style = 3)

# Loop through each MCMC iteration
for(iter in 1:n_iter) {
  
  obs_idx <- 1  # Track observation index in flattened array
  
  # Loop through time and series
  for(t in 1:n) {
    for(i in 1:nb_series) {
      
      # Get latent state value for this iteration
      x_name <- paste0("x[", t, ", ", i, "]")
      x_value <- all_samples[iter, x_name]
      
      # Get observed value and observation error
      y_value <- y_obs[t, i]
      omega_value <- omega_obs[t, i]
      
      # Calculate log-likelihood: y_obs[t,i] ~ N(x[t,i], omega_obs[t,i])
      # log(p(y|x)) = -0.5 * [(y - x)^2 / omega^2 + log(2*pi*omega^2)]
      
      if(!is.na(y_value) && !is.na(omega_value) && omega_value > 0) {
        residual <- y_value - x_value
        log_lik_matrix[iter, obs_idx] <- -0.5 * (
          (residual^2 / omega_value^2) + 
            log(2 * pi * omega_value^2)
        )
      } else {
        # Handle missing data: contribution is zero (or could remove from analysis)
        log_lik_matrix[iter, obs_idx] <- 0
      }
      
      obs_idx <- obs_idx + 1
    }
  }
  
  # Update progress bar
  if(iter %% 100 == 0) {
    setTxtProgressBar(pb, iter)
  }
}

close(pb)

end_time <- Sys.time()
computation_time <- difftime(end_time, start_time, units = "secs")

cat("\n✓ Log-likelihood computation completed in", 
    round(computation_time, 2), "seconds\n")
cat("  Matrix dimensions:", nrow(log_lik_matrix), "iterations x", 
    ncol(log_lik_matrix), "observations\n")

# ------------------------------------------------------------------------------
# REMOVE OBSERVATIONS WITH MISSING DATA (if any)
# ------------------------------------------------------------------------------

# Identify columns (observations) with all zeros or NAs (missing data)
valid_obs <- apply(log_lik_matrix, 2, function(x) !all(x == 0) && !any(is.na(x)))
n_valid <- sum(valid_obs)
n_missing <- n_obs - n_valid

if(n_missing > 0) {
  cat("\nℹ Removing", n_missing, "observations with missing data\n")
  log_lik_matrix <- log_lik_matrix[, valid_obs, drop = FALSE]
  cat("  Remaining observations:", n_valid, "\n")
}

# ------------------------------------------------------------------------------
# CALCULATE PSIS-LOO
# ------------------------------------------------------------------------------

cat("\nCalculating PSIS-LOO...\n")
cat("This may take a few minutes...\n\n")

start_time_loo <- Sys.time()

# Calculate relative efficiency for importance sampling
# This accounts for autocorrelation in MCMC chains
r_eff <- relative_eff(exp(log_lik_matrix), 
                      chain_id = rep(1:length(mcmc), 
                                     each = nrow(as.matrix(mcmc[[1]]))))

# Calculate PSIS-LOO
loo_result <- loo(log_lik_matrix, 
                  r_eff = r_eff,
                  cores = parallel::detectCores() - 1)

end_time_loo <- Sys.time()

cat("✓ PSIS-LOO calculation completed in", 
    round(difftime(end_time_loo, start_time_loo, units = "secs"), 2), 
    "seconds\n\n")

# ------------------------------------------------------------------------------
# DISPLAY RESULTS
# ------------------------------------------------------------------------------

cat("========================================\n")
cat("PSIS-LOO RESULTS\n")
cat("========================================\n\n")

print(loo_result)

cat("\n----------------------------------------\n")
cat("KEY METRICS:\n")
cat("----------------------------------------\n")
cat("ELPD (Expected Log Predictive Density):", 
    round(loo_result$estimates["elpd_loo", "Estimate"], 2), "\n")
cat("ELPD SE:", 
    round(loo_result$estimates["elpd_loo", "SE"], 2), "\n")
cat("LOO-IC (lower is better):", 
    round(-2 * loo_result$estimates["elpd_loo", "Estimate"], 2), "\n")
cat("Effective parameters (p_loo):", 
    round(loo_result$estimates["p_loo", "Estimate"], 2), "\n\n")

# ------------------------------------------------------------------------------
# DIAGNOSTIC CHECKS
# ------------------------------------------------------------------------------

cat("----------------------------------------\n")
cat("DIAGNOSTIC INFORMATION:\n")
cat("----------------------------------------\n")

# Check Pareto k diagnostic values
pareto_k <- loo_result$diagnostics$pareto_k
k_threshold_table <- data.frame(
  Threshold = c("< 0.5 (Good)", "0.5 - 0.7 (OK)", "0.7 - 1.0 (Bad)", "> 1.0 (Very bad)"),
  Count = c(
    sum(pareto_k < 0.5),
    sum(pareto_k >= 0.5 & pareto_k < 0.7),
    sum(pareto_k >= 0.7 & pareto_k < 1.0),
    sum(pareto_k >= 1.0)
  ),
  Percentage = round(c(
    mean(pareto_k < 0.5) * 100,
    mean(pareto_k >= 0.5 & pareto_k < 0.7) * 100,
    mean(pareto_k >= 0.7 & pareto_k < 1.0) * 100,
    mean(pareto_k >= 1.0) * 100
  ), 1)
)

print(k_threshold_table)

# Warnings and recommendations
if(any(pareto_k > 0.7)) {
  cat("\n⚠ WARNING: Some Pareto k values exceed 0.7\n")
  cat("Number of problematic observations:", sum(pareto_k > 0.7), "\n")
  cat("Recommendation: Consider using K-fold cross-validation\n")
  
  # Identify problematic observations
  problematic_idx <- which(pareto_k > 0.7)
  if(length(problematic_idx) <= 20) {
    cat("\nProblematic observation indices:\n")
    # Convert flat index back to (t, i) pairs
    problematic_pairs <- data.frame(
      obs_idx = problematic_idx,
      time = ((problematic_idx - 1) %/% nb_series) + 1,
      series = ((problematic_idx - 1) %% nb_series) + 1,
      pareto_k = pareto_k[problematic_idx]
    )
    print(problematic_pairs)
  } else {
    cat("Too many problematic observations to display individually\n")
    cat("Maximum Pareto k:", round(max(pareto_k), 3), "\n")
  }
} else if(any(pareto_k > 0.5)) {
  cat("\nℹ Note: Some Pareto k values are between 0.5 and 0.7\n")
  cat("LOO estimates are acceptable but not ideal\n")
} else {
  cat("\n✓ All Pareto k values < 0.5: LOO estimates are reliable\n")
}

# ------------------------------------------------------------------------------
# COMPARE WITH WAIC (if available)
# ------------------------------------------------------------------------------

cat("\n----------------------------------------\n")
cat("COMPARISON WITH WAIC:\n")
cat("----------------------------------------\n")

if(file.exists("results/WAIC_all_chains.rds")) {
  WAIC_result <- readRDS("results/WAIC_all_chains.rds")
  
  cat("WAIC:", round(WAIC_result$WAIC, 2), "\n")
  cat("LOO-IC:", round(-2 * loo_result$estimates["elpd_loo", "Estimate"], 2), "\n")
  cat("Difference (WAIC - LOO-IC):", 
      round(WAIC_result$WAIC - (-2 * loo_result$estimates["elpd_loo", "Estimate"]), 2), "\n")
  cat("\nNote: Small differences are expected. Large differences may indicate issues.\n")
} else {
  cat("WAIC results not found for comparison.\n")
}

# ------------------------------------------------------------------------------
# SAVE RESULTS
# ------------------------------------------------------------------------------

cat("\n----------------------------------------\n")
cat("SAVING RESULTS:\n")
cat("----------------------------------------\n")

# Save main LOO result
saveRDS(loo_result, file = "results/psis_loo_result.rds")
cat("✓ PSIS-LOO results saved to: results/psis_loo_result.rds\n")

# Save log-likelihood matrix for future use
saveRDS(log_lik_matrix, file = "results/log_lik_matrix.rds")
cat("✓ Log-likelihood matrix saved to: results/log_lik_matrix.rds\n")

# Save summary table
loo_summary <- data.frame(
  Metric = rownames(loo_result$estimates),
  Estimate = loo_result$estimates[, "Estimate"],
  SE = loo_result$estimates[, "SE"]
)
write.csv(loo_summary, "results/psis_loo_summary.csv", row.names = FALSE)
cat("✓ Summary table saved to: results/psis_loo_summary.csv\n")

# Save Pareto k diagnostics with observation identifiers
pareto_k_df <- data.frame(
  observation = 1:length(pareto_k),
  time_point = ((1:length(pareto_k) - 1) %/% nb_series) + 1,
  series = ((1:length(pareto_k) - 1) %% nb_series) + 1,
  pareto_k = pareto_k
)
write.csv(pareto_k_df, "results/pareto_k_diagnostics.csv", row.names = FALSE)
cat("✓ Pareto k diagnostics saved to: results/pareto_k_diagnostics.csv\n")

# ------------------------------------------------------------------------------
# SUMMARY STATISTICS
# ------------------------------------------------------------------------------

cat("\n========================================\n")
cat("COMPUTATION SUMMARY\n")
cat("========================================\n")
cat("Total computation time:", 
    round(computation_time + difftime(end_time_loo, start_time_loo, units = "secs"), 2), 
    "seconds\n")
cat("Log-likelihood calculations:", format(n_iter * n_obs, big.mark = ","), "\n")
cat("Observations analyzed:", n_valid, "\n")
cat("MCMC iterations used:", n_iter, "\n")
cat("\n========================================\n")
cat("PSIS-LOO CALCULATION COMPLETE\n")
cat("========================================\n\n")

cat("NEXT STEPS:\n")
cat("1. Review Pareto k diagnostics above\n")
cat("2. If many k > 0.7, consider K-fold cross-validation\n")
cat("3. Use LOO-IC for model comparison (lower is better)\n")
cat("4. Compare models: loo_compare(loo1, loo2, ...)\n")
cat("5. Plot Pareto k: plot(loo_result)\n")