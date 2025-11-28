extract_posterior_samples2 <- function(mcmc_data, param_pattern) {
  # Check MCMC object type
  if (coda::is.mcmc.list(mcmc_data)) {
    # Convert mcmc.list to single matrix
    mcmc_matrix <- do.call(rbind, lapply(mcmc_data, as.matrix))
  } else if (coda::is.mcmc(mcmc_data)) {
    mcmc_matrix <- as.matrix(mcmc_data)
  } else {
    stop("mcmc_data must be an mcmc or mcmc.list object")
  }
  
  # Get parameter names
  if (coda::is.mcmc.list(mcmc_data)) {
    param_names <- coda::varnames(mcmc_data[[1]])
  } else {
    param_names <- coda::varnames(mcmc_data)
  }
  
  # Find parameters matching the pattern
  matching_params <- param_names[grepl(param_pattern, param_names)]
  
  if (length(matching_params) == 0) {
    warning("No parameters found matching pattern: ", param_pattern)
    return(NULL)
  }
  
  # Check that columns exist
  missing_cols <- setdiff(matching_params, colnames(mcmc_matrix))
  if (length(missing_cols) > 0) {
    warning("Missing columns: ", paste(missing_cols, collapse = ", "))
    matching_params <- intersect(matching_params, colnames(mcmc_matrix))
  }
  
  if (length(matching_params) == 0) {
    return(NULL)
  }
  
  # Extract data
  param_data <- mcmc_matrix[, matching_params, drop = FALSE]
  return(param_data)
}
