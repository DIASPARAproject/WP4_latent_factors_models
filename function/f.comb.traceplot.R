# -------------------------------
# FUNCTION TO CREATE COMBINED TRACEPLOTS FOR A PARAMETER GROUP
# -------------------------------

create_combined_traceplot <- function(mcmc_data, param_group, group_name, max_params = 12) {
  
  # Limit number of parameters to avoid overcrowded plots
  if (length(param_group) > max_params) {
    param_group <- param_group[1:max_params]
    warning("Displaying only first ", max_params, " parameters for group: ", group_name)
  }
  
  # Check which parameters actually exist in the MCMC data
  available_params <- param_group[param_group %in% coda::varnames(mcmc_data)]
  
  if (length(available_params) == 0) {
    warning("No parameters from group '", group_name, "' found in MCMC data")
    return(NULL)
  }
  
  # Create individual plots for each parameter
  plot_list <- list()
  
  for (i in seq_along(available_params)) {
    param_name <- available_params[i]
    tryCatch({
      plot_list[[i]] <- create_traceplot_ggplot(mcmc_data, param_name)
    }, error = function(e) {
      warning("Could not create traceplot for ", param_name, ": ", e$message)
      return(NULL)
    })
  }
  
  # Remove NULL plots
  plot_list <- plot_list[!sapply(plot_list, is.null)]
  
  if (length(plot_list) == 0) {
    return(NULL)
  }
  
  # Combine plots using patchwork
  if (length(plot_list) == 1) {
    combined_plot <- plot_list[[1]]
  } else {
    # Calculate layout dimensions
    ncols <- min(3, length(plot_list))
    nrows <- ceiling(length(plot_list) / ncols)
    
    combined_plot <- wrap_plots(plot_list, ncol = ncols) +
      plot_annotation(
        title = paste("Traceplots:", stringr::str_to_title(gsub("_", " ", group_name))),
        subtitle = paste("Parameters:", length(available_params), "| Chains:", coda::nchain(mcmc_data)),
        theme = theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 11, color = "grey60")
        )
      )
  }
  
  return(combined_plot)
}
