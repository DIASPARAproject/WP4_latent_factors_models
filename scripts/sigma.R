cat("Computing observational uncertainties...\n")

# Retrieve lower and upper quantile matrices from uncertainty bounds
q5  <- uncertainty_bounds$q5
q95 <- uncertainty_bounds$q95

# Validate inputs
if (!is.matrix(q5) || !is.matrix(q95)) {
  stop("Both q5 and q95 must be matrices. 
       Please check that uncertainty extraction was successful.")
}
if (!identical(dim(q5), dim(q95))) {
  stop("q5 and q95 must have the same dimensions 
       (same number of labels and time points).")
}

# Standard normal quantiles corresponding to 5% and 95%
z_005 <- qnorm(0.05)  # ≈ -1.645
z_095 <- qnorm(0.95)  # ≈ +1.645

# Compute observational standard deviations
#    Formula: sigma = (upper - lower) / (z_upper - z_lower)
sigma_observations <- abs((q95 - q5) / (z_095 - z_005))

# Quality check: identify non-finite values (Inf or NaN)
if (any(!is.finite(sigma_observations))) {
  warning("Some non-finite values were detected in computed sigma_obs.
          These may come from missing values in q5/q95.")
}

# Final confirmation message with range of sigma values
cat("✅ Computed observational uncertainties. Range =",
    round(range(sigma_observations, na.rm = TRUE), 4), "\n")


# Convert to matrices for NIMBLE modeling
X_observed   <- as.matrix(time_series)        # Median logit survival
Q5_bounds    <- as.matrix(uncertainty_bounds$q5)  # Lower uncertainty
Q95_bounds   <- as.matrix(uncertainty_bounds$q95) # Upper uncertainty
Sigma_obs    <- as.matrix(sigma_observations)     # Observation errors

# Data quality checks
cat("\nData quality assessment:\n")
cat("  Missing values in observations:", sum(is.na(X_observed)), "\n")
cat("  Infinite values in observations:", sum(!is.finite(X_observed)), "\n")
cat("  Range of survival (logit scale):", round(range(X_observed, na.rm = TRUE), 3), "\n")
cat("  Range of observational errors:", round(range(Sigma_obs, na.rm = TRUE), 4), "\n")
