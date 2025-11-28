cat("\n=== ANALYSIS SUMMARY ===\n")
cat("Data processing completed successfully!\n\n")

cat("Dataset characteristics:\n")
cat("  • Study system: Atlantic salmon post-smolt survival\n")
cat("  • Geographic scope:", length(labels), "populations across North Atlantic\n") 
cat("  • Temporal extent:", min(merged_table$year), "-", max(merged_table$year), 
    "(", diff(range(merged_table$year)) + 1, "years )\n")
cat("  • Data dimensions:", nrow(X_observed), "time points ×", ncol(X_observed), "labels\n")
cat("  • Missing observations:", round(100 * sum(is.na(X_observed)) / length(X_observed), 1), "%\n")

cat("\nFiles created:\n")
cat("  • nimble_data.rds: Observed survival data for modeling\n")
cat("  • nimble_constants.rds: Model dimensions and fixed parameters\n")
cat("  • Visualization plots saved in results/ directory\n")

cat("\nNext steps:\n")
cat("  1. Define NIMBLE model structure (state-space with AR(1) process)\n")
cat("  2. Configure and run MCMC sampling (parallel chains recommended)\n") 
cat("  3. Assess convergence using Gelman-Rubin diagnostics\n")
cat("  4. Analyze posterior distributions and model fit\n")

cat("Data preparation complete. Ready for Bayesian modeling!\n")
cat("Analysis prepared at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")