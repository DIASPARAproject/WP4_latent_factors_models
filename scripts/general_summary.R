# ------------------------------------------------------------------------------
# BAYESIAN MODEL ASSESSMENT - THE FINAL VERDICT
# ------------------------------------------------------------------------------
# Synthesizing all diagnostic information into a clear model health assessment. 
# This automated evaluation system provides objective criteria for model adequacy, 
# moving beyond individual parameter diagnostics to an overall model rating. 
# The assessment includes DFA-specific recommendations that acknowledge the unique 
# challenges of factor analysis models, such as identification constraints and 
# hierarchical variance estimation.

cat("\nCOMPREHENSIVE MODEL ASSESSMENT\n")

#' Generate comprehensive assessment
convergence_summary <- diagnostic_results$convergence_summary

#' Model-specific assessment criteria
overall_convergence <- convergence_summary$overall_convergence
overall_ess_adequacy <- convergence_summary$overall_ess_adequacy

cat("DYNAMIC FACTOR ANALYSIS MODEL HEALTH CHECK:\n")

# Overall model status
if (overall_convergence >= 0.95 && overall_ess_adequacy >= 0.80) {
  cat("📊 MODEL STATUS: EXCELLENT\n")
} else if (overall_convergence >= 0.90 && overall_ess_adequacy >= 0.70) {
  cat("📊 MODEL STATUS: GOOD\n") 
} else if (overall_convergence >= 0.80 && overall_ess_adequacy >= 0.60) {
  cat("📊 MODEL STATUS: MODERATE - Improvements Recommended\n")
} else {
  cat("📊 MODEL STATUS: POOR - Significant Issues Detected\n")
}

cat("\nKEY METRICS:\n")
cat("  • Overall convergence rate:", scales::percent(overall_convergence, 0.1), "\n")
cat("  • ESS adequacy rate:", scales::percent(overall_ess_adequacy, 0.1), "\n")

#' DFA-specific recommendations
cat("\nMODEL-SPECIFIC RECOMMENDATIONS:\n")

# Latent states assessment
if (exists("latent_results") && nrow(latent_results) > 0) {
  latent_convergence <- mean(latent_results$Converged, na.rm = TRUE)
  if (latent_convergence < 0.90) {
    cat("  ⚠️  LATENT STATES: Poor convergence detected\n")
    cat("     - Consider longer chains or different initialization\n")
    cat("     - Check for identification issues in factor model\n")
  } else {
    cat("  ✅ LATENT STATES: Convergence satisfactory\n")
  }
}

# Factor loadings assessment  
if (exists("loading_results") && nrow(loading_results) > 0) {
  loading_convergence <- mean(loading_results$Converged, na.rm = TRUE)
  if (loading_convergence < 0.95) {
    cat("  ⚠️  FACTOR LOADINGS: Convergence issues detected\n")
    cat("     - Review factor identification constraints\n")
    cat("     - Consider alternative prior specifications\n")
  } else {
    cat("  ✅ FACTOR LOADINGS: Convergence satisfactory\n")
  }
}

# Variance parameters assessment
if (exists("variance_results") && nrow(variance_results) > 0) {
  variance_convergence <- mean(variance_results$Converged)
  if (variance_convergence < 0.85) {
    cat("  ⚠️  VARIANCE PARAMETERS: Challenging convergence\n")
    cat("     - Common issue with hierarchical variance models\n")
    cat("     - Consider informative priors or reparameterization\n")
  } else {
    cat("  ✅ VARIANCE PARAMETERS: Convergence acceptable\n")
  }
}

# WAIC consistency check
if (exists("waic_range") && waic_range > 10) {
  cat("  ⚠️  MODEL SELECTION: WAIC inconsistency between chains\n")
  cat("     - May indicate convergence issues\n")
  cat("     - Use combined WAIC for model comparison\n")
}

# ------------------------------
# RESULTS EXPORT AND SUMMARY - CREATING A COMPREHENSIVE RECORD
# ------------------------------
# Professional documentation: This final section creates a comprehensive diagnostic 
# report that serves multiple purposes - immediate reference for model evaluation, 
# documentation for reproducible research, and communication tool for collaborators. 
# The structured report format ensures that key findings are clearly communicated 
# and that the diagnostic process can be replicated or audited by others.

cat("\nEXPORTING RESULTS AND FINAL SUMMARY\n")

#' Save comprehensive diagnostic report
export_diagnostic_report <- function(diagnostic_results, mcmc_info, output_file) {
  
  sink(output_file)
  
  cat("DYNAMIC FACTOR ANALYSIS - BAYESIAN MCMC DIAGNOSTIC REPORT\n")
  cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  
  cat("MODEL CONFIGURATION:\n")
  cat("  Model Type: Dynamic Factor Analysis with AR(1) structure\n")
  cat("  MCMC Chains:", mcmc_info$n_chains, "\n")
  cat("  Iterations per chain:", format(mcmc_info$n_iterations, big.mark = ","), "\n")
  cat("  Total samples:", format(mcmc_info$total_samples, big.mark = ","), "\n")
  cat("  Parameters monitored:", mcmc_info$n_variables, "\n\n")
  
  cat("CONVERGENCE ASSESSMENT:\n")
  print(diagnostic_results$convergence_summary)
  
  if (exists("WAIC_all_chains")) {
    cat("\n\nMODEL SELECTION CRITERIA:\n")
    cat("Overall WAIC:\n")
    print(WAIC_all_chains)
  }
  
  sink()
}
