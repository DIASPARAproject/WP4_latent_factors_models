#' Create Factor Evolution Plot with Year Axis
#'
#' @param mcmc_data MCMC samples from NIMBLE (mcmc.list or mcmc object)
#' @param factor_k Integer, which factor to plot (1, 2, 3, ...)
#' @param years Numeric vector of years corresponding to time indices (optional)
#' @param start_year Integer, starting year if years vector not provided (optional)
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' # With years vector
#' plot1 <- create_factor_evolution_plot(mcmc, factor_k = 1, years = time_series$year)
#' 
#' # With start year
#' plot1 <- create_factor_evolution_plot(mcmc, factor_k = 1, start_year = 2000)
#' 
#' # Without years (uses time index)
#' plot1 <- create_factor_evolution_plot(mcmc, factor_k = 1)
create_factor_evolution_plot <- function(mcmc_data, factor_k, years = NULL, start_year = NULL) {
  
  # Load required packages
  if (!require(dplyr)) stop("Package 'dplyr' required")
  if (!require(ggplot2)) stop("Package 'ggplot2' required")
  if (!require(coda)) stop("Package 'coda' required")
  
  # Build pattern for factor k
  factor_pattern <- paste0("factor\\[", factor_k, ",")
  
  # Extract factor data using helper function
  # Assumes extract_posterior_samples() exists in your environment
  if (!exists("extract_posterior_samples")) {
    stop("Function 'extract_posterior_samples' not found. Please source it first.")
  }
  
  factor_data <- extract_posterior_samples(mcmc_data, factor_pattern)
  
  if (is.null(factor_data) || nrow(factor_data) == 0) {
    cat("No data found for factor", factor_k, "\n")
    cat("Pattern used:", factor_pattern, "\n")
    
    # Diagnostic: show some parameter names
    if (coda::is.mcmc.list(mcmc_data)) {
      all_params <- coda::varnames(mcmc_data[[1]])
    } else {
      all_params <- coda::varnames(mcmc_data)
    }
    cat("First available parameters:", head(all_params, 10), "\n")
    
    return(NULL)
  }
  
  # Extract time index from parameter names
  factor_data$time <- as.numeric(gsub(".*\\[\\d+,\\s*(\\d+)\\].*", "\\1", factor_data$parameter))
  
  # Check time indices
  if (all(is.na(factor_data$time))) {
    # Try alternative pattern
    factor_data$time <- as.numeric(gsub(".*\\.(\\d+)$", "\\1", factor_data$parameter))
    
    if (all(is.na(factor_data$time))) {
      cat("Unable to extract time indices\n")
      cat("Example parameter names:", head(factor_data$parameter, 5), "\n")
      return(NULL)
    }
  }
  
  # Filter missing values
  factor_data <- factor_data[!is.na(factor_data$value) & !is.na(factor_data$time), ]
  
  if (nrow(factor_data) == 0) {
    cat("No valid data after filtering\n")
    return(NULL)
  }
  
  # Calculate posterior summaries
  factor_summary <- factor_data %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(
      mean = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      q025 = quantile(value, 0.025, na.rm = TRUE),
      q975 = quantile(value, 0.975, na.rm = TRUE),
      q25 = quantile(value, 0.25, na.rm = TRUE),
      q75 = quantile(value, 0.75, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      n_samples = n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(time)
  
  # Create year column based on inputs
  if (!is.null(years)) {
    # Use provided years vector
    if (length(years) == nrow(factor_summary)) {
      factor_summary$year <- years
    } else if (length(years) >= max(factor_summary$time)) {
      # Map time indices to years
      factor_summary$year <- years[factor_summary$time]
    } else {
      warning(paste0("Length of years (", length(years), 
                     ") doesn't match time points. Using time index instead."))
      factor_summary$year <- factor_summary$time
    }
  } else if (!is.null(start_year)) {
    # Generate years from start_year
    factor_summary$year <- start_year + (factor_summary$time - 1)
  } else {
    # Use time index as fallback
    factor_summary$year <- factor_summary$time
  }
  
  # Diagnostic output
  cat("\n=== Factor", factor_k, "Summary ===\n")
  cat("Number of time points:", nrow(factor_summary), "\n")
  cat("Number of samples per point:", unique(factor_summary$n_samples), "\n")
  cat("Range of posterior means:", sprintf("%.3f to %.3f", 
                                           min(factor_summary$mean), 
                                           max(factor_summary$mean)), "\n")
  cat("Range of 95% CI:", sprintf("%.3f to %.3f", 
                                  min(factor_summary$q025), 
                                  max(factor_summary$q975)), "\n")
  cat("Year range:", sprintf("%s to %s", 
                             min(factor_summary$year), 
                             max(factor_summary$year)), "\n\n")
  
  # Create the plot
  p <- ggplot2::ggplot(factor_summary, ggplot2::aes(x = year)) +
    # 95% credible interval
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q025, ymax = q975), 
                         alpha = 0.2, fill = "#4E79A7") +
    # 50% credible interval
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q25, ymax = q75), 
                         alpha = 0.4, fill = "#4E79A7") +
    # Posterior mean
    ggplot2::geom_line(ggplot2::aes(y = mean), color = "#4E79A7", linewidth = 1.2) +
    # Zero reference line
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", 
                        color = "grey50", alpha = 0.7) +
    ggplot2::labs(
      title = paste("Latent Factor", factor_k, "Evolution"),
      subtitle = "Posterior mean with 50% and 95% credible intervals",
      x = "Year",
      y = paste("Factor", factor_k, "Value")
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(color = "grey60"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "grey90", linewidth = 0.5),
      panel.grid.major.y = ggplot2::element_line(color = "grey90", linewidth = 0.5)
    )
  
  return(p)
}


#' Create All Factor Evolution Plots
#'
#' Convenience function to create plots for all factors
#'
#' @param mcmc_data MCMC samples from NIMBLE
#' @param n_factors Number of factors (e.g., 3)
#' @param years Numeric vector of years (optional)
#' @param start_year Integer, starting year (optional)
#' @param combine Logical, whether to combine plots using patchwork (default: FALSE)
#'
#' @return List of ggplot2 objects, or combined plot if combine = TRUE
#' @export
create_all_factor_plots <- function(mcmc_data, n_factors, years = NULL, 
                                    start_year = NULL, combine = FALSE) {
  
  plots <- lapply(1:n_factors, function(k) {
    create_factor_evolution_plot(mcmc_data, factor_k = k, 
                                 years = years, start_year = start_year)
  })
  
  names(plots) <- paste0("factor_", 1:n_factors)
  
  if (combine) {
    if (!require(patchwork)) {
      warning("Package 'patchwork' not available. Returning list of plots.")
      return(plots)
    }
    
    combined <- Reduce(`/`, plots) +
      patchwork::plot_annotation(
        title = "Latent Factor Dynamics",
        subtitle = if (!is.null(years)) {
          paste0("Estimated from ", min(years), " to ", max(years))
        } else if (!is.null(start_year)) {
          paste0("Starting from year ", start_year)
        } else {
          "Time series decomposition"
        }
      )
    
    return(combined)
  }
  
  return(plots)
}