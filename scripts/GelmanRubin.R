# -------------------------------------------------------------
# COMPUTE GELMAN-RUBIN DIAGNOSTICS WITH ERROR HANDLING
# -------------------------------------------------------------

tryCatch({
  gelman_results_raw <- coda::gelman.diag(
    mcmc,
    confidence = DIAGNOSTIC_CONFIG$confidence_level,
    transform = TRUE,
    autoburnin = TRUE,
    multivariate = FALSE
  )
}, error = function(e) {
  stop("Gelman-Rubin diagnostic failed: ", e$message)
})

# Extract point estimates
psrf_values <- gelman_results_raw$psrf[, "Point est."]

# Create structured data frame with parameter classification
gelman_results <- data.frame(
  Parameter = names(psrf_values),
  PSRF = as.numeric(psrf_values),
  stringsAsFactors = FALSE
) %>%
  dplyr::mutate(
    # Automatic parameter type detection
    Type = classify_parameters(Parameter),
    # Flag problematic parameters
    Converged = PSRF <= DIAGNOSTIC_CONFIG$psrf_threshold,
    # Extract parameter indices for grouping
    Index = extract_parameter_indices(Parameter)
  ) %>%
  dplyr::arrange(desc(PSRF))  # Sort by worst convergence first

cat("  ✓ Computed diagnostics for", nrow(gelman_results), "parameters\n")

# Gelman-Rubin summary
gelman_summary <- gelman_results %>%
  dplyr::group_by(Type) %>%
  dplyr::summarise(
    n_params = dplyr::n(),
    max_PSRF = max(PSRF, na.rm = TRUE),
    mean_PSRF = mean(PSRF, na.rm = TRUE),
    n_converged = sum(Converged, na.rm = TRUE),
    convergence_rate = n_converged / n_params,
    .groups = "drop"
  )

# Overall assessment
overall_convergence <- mean(gelman_results$Converged, na.rm = TRUE)