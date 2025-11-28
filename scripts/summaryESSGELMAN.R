cat("Generating convergence summary...\n")

# Print summary to console
cat("CONVERGENCE ASSESSMENT SUMMARY\n")
cat("Overall Convergence Rate:", scales::percent(overall_convergence, 1), "\n")
cat("Overall ESS Adequacy Rate:", scales::percent(overall_ess_adequacy, 1), "\n\n")

cat("Gelman-Rubin Summary by Parameter Type:\n")
print(gelman_summary)

cat("\nEffective Sample Size Summary by Parameter Type:\n")
print(ess_summary)

# Problematic parameters
problematic_psrf <- gelman_results %>%
  dplyr::filter(!Converged) %>%
  dplyr::arrange(desc(PSRF))

if (nrow(problematic_psrf) > 0) {
  cat("\nParameters with poor convergence (PSRF >", DIAGNOSTIC_CONFIG$psrf_threshold, "):\n")
  print(utils::head(problematic_psrf[c("Parameter", "PSRF", "Type")], 20))
}

convergence_summary <- list(
  gelman_summary = gelman_summary,
  ess_summary = ess_summary,
  overall_convergence = overall_convergence,
  overall_ess_adequacy = overall_ess_adequacy,
  problematic_parameters = problematic_psrf
)

# ------------------------------------------------------------------------------
# RESULTS PRESERVATION - BUILDING A PERMANENT RECORD
# ------------------------------------------------------------------------------
# Reproducible research requires systematic documentation. By saving both raw 
# diagnostic data (CSV files) and visualizations (PNG files), we create a complete 
# record that can be revisited, shared, or used for comparative studies. The text 
# summary provides a human-readable snapshot of model health that's invaluable for 
# project documentation and reporting.

if (save_plots) {
  cat("Saving diagnostic results...\n")
  
  # Save data tables
  write.csv(gelman_results, file.path(output_directory, "gelman_diagnostics.csv"), row.names = FALSE)
  write.csv(ess_results, file.path(output_directory, "ess_diagnostics.csv"), row.names = FALSE)
  # Save summary as text file
  sink(file.path(output_directory, "convergence_summary.txt"))
  cat("MCMC CONVERGENCE ASSESSMENT SUMMARY\n")
  cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  print(convergence_summary)
  sink()
  
  cat("  ✓ Results saved to:", output_directory, "\n")
}
