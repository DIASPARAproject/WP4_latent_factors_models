cat("\nDFA-SPECIFIC PARAMETER ANALYSIS\n")

#' Access diagnostic results
gelman_diagnostics <- diagnostic_results$gelman_diagnostics
ess_diagnostics <- diagnostic_results$ess_diagnostics

#' Analyze latent states convergence
cat("LATENT STATES ANALYSIS:\n")
latent_results <- gelman_diagnostics %>%
  dplyr::filter(Type == "latent_states") %>%
  dplyr::arrange(desc(PSRF))

if (nrow(latent_results) > 0) {
  cat("  • Number of latent states:", nrow(latent_results), "\n")
  cat("  • Convergence rate:", 
      scales::percent(mean(latent_results$Converged, na.rm = TRUE), accuracy = 0.1), "\n")
  cat("  • Mean PSRF:", round(mean(latent_results$PSRF, na.rm = TRUE), 4), "\n")
  cat("  • Max PSRF:", round(max(latent_results$PSRF, na.rm = TRUE), 4), "\n")
  
  # Show worst converging latent states (ignore NAs in Converged)
  worst_states <- latent_results %>%
    dplyr::filter(!is.na(Converged) & Converged == FALSE)
  if (nrow(worst_states) > 0) {
    cat("  • Problematic states:\n")
    for (i in 1:nrow(worst_states)) {
      cat("    -", worst_states$Parameter[i], ": PSRF =", 
          round(worst_states$PSRF[i], 4), "\n")
    }
  }
}

#' Analyze factor loadings convergence  
cat("\nFACTOR LOADINGS ANALYSIS:\n")
loading_results <- gelman_diagnostics %>%
  dplyr::filter(Type == "loadings") %>%
  dplyr::arrange(desc(PSRF))

if (nrow(loading_results) > 0) {
  cat("  • Number of factor loadings:", nrow(loading_results), "\n")
  cat("  • Convergence rate:", 
      scales::percent(mean(loading_results$Converged, na.rm = TRUE), accuracy = 0.1), "\n")
  cat("  • Mean PSRF:", round(mean(loading_results$PSRF, na.rm = TRUE), 4), "\n")
  cat("  • Max PSRF:", round(max(loading_results$PSRF, na.rm = TRUE), 4), "\n")
}

#' Analyze factor loadings convergence  
cat("\nCOMMON FACTORS ANALYSIS:\n")
loading_results <- gelman_diagnostics %>%
  dplyr::filter(Type == "factors") %>%
  dplyr::arrange(desc(PSRF))

if (nrow(loading_results) > 0) {
  cat("  • Number of common factors:", nrow(loading_results), "\n")
  cat("  • Convergence rate:", 
      scales::percent(mean(loading_results$Converged, na.rm = TRUE), accuracy = 0.1), "\n")
  cat("  • Mean PSRF:", round(mean(loading_results$PSRF, na.rm = TRUE), 4), "\n")
  cat("  • Max PSRF:", round(max(loading_results$PSRF, na.rm = TRUE), 4), "\n")
}



#' Analyze variance parameters
cat("\nVARIANCE PARAMETERS ANALYSIS:\n")
variance_results <- gelman_diagnostics %>%
  dplyr::filter(stringr::str_detect(Type, "sd")) %>%
  dplyr::arrange(desc(PSRF))

if (nrow(variance_results) > 0) {
  cat("  • Number of variance parameters:", nrow(variance_results), "\n")
  cat("  • Convergence rate:", 
      scales::percent(mean(variance_results$Converged), accuracy = 0.1), "\n")
  cat("  • Mean PSRF:", round(mean(variance_results$PSRF), 4), "\n")
  
  # Variance parameters are often more challenging to estimate
  cat("  • Factor SD convergence:", 
      scales::percent(mean(variance_results$Converged[variance_results$Type == "factor_sd"]), 
                      accuracy = 0.1), "\n")
  cat("  • Process SD convergence:", 
      scales::percent(mean(variance_results$Converged[variance_results$Type == "process_sd"]), 
                      accuracy = 0.1), "\n")
}

#' Analyze autoregressive parameters
cat("\nAUTOREGRESSIVE PARAMETERS ANALYSIS:\n")
ar_results <- gelman_diagnostics %>%
  dplyr::filter(Type == "autocorrelation") %>%
  dplyr::arrange(desc(PSRF))

if (nrow(ar_results) > 0) {
  cat("  • Number of AR parameters:", nrow(ar_results), "\n")
  cat("  • Convergence rate:", 
      scales::percent(mean(ar_results$Converged), accuracy = 0.1), "\n")
  cat("  • Mean PSRF:", round(mean(ar_results$PSRF), 4), "\n")
  
  # Check for parameters near unit root (phi close to 1)
  # This would require access to actual parameter values
  cat("  • Note: Check posterior distributions for near-unit-root behavior\n")
}