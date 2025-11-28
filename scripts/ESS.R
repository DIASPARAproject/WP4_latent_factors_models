# ------------------------------------------------------------------------------
# EFFECTIVE SAMPLE SIZE - QUANTIFYING MCMC EFFICIENCY
# ------------------------------------------------------------------------------
# Effective Sample Size (ESS) tells us how many "independent" samples our correlated 
# MCMC chains provide. This is crucial for Dynamic Factor Analysis models where 
# parameter correlations can be high. Low ESS indicates we may need longer chains 
# or different sampling strategies. By computing ESS for each parameter, we identify 
# which model components are most challenging to sample efficiently.

cat("Computing effective sample sizes...\n")

# Calculate ESS for each parameter
ess_values <- coda::effectiveSize(mcmc)

# Create data frame with ESS results
ess_results <- data.frame(
  Parameter = names(ess_values),
  ESS = as.numeric(ess_values),
  stringsAsFactors = FALSE
) %>%
  dplyr::mutate(
    Type = classify_parameters(Parameter),
    ESS_Adequate = ESS >= DIAGNOSTIC_CONFIG$ess_min_threshold,
    ESS_Rate = ESS / (coda::nchain(mcmc) * coda::niter(mcmc))
  ) %>%
  dplyr::arrange(ESS)  # Sort by lowest ESS first

cat("  ✓ Computed ESS for", nrow(ess_results), "parameters\n")

# ESS summary
ess_summary <- ess_results %>%
  dplyr::group_by(Type) %>%
  dplyr::summarise(
    n_params = dplyr::n(),
    min_ESS = min(ESS, na.rm = TRUE),
    mean_ESS = mean(ESS, na.rm = TRUE),
    median_ESS = median(ESS, na.rm = TRUE),
    n_adequate = sum(ESS_Adequate, na.rm = TRUE),
    adequacy_rate = n_adequate / n_params,
    .groups = "drop"
  )

# Overall assessment
overall_ess_adequacy <- mean(ess_results$ESS_Adequate, na.rm = TRUE)