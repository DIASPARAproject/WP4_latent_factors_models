# Load and attach required packages for the analysis
# Includes Bayesian modeling, data manipulation, and visualization tools
required_packages <- c(
  "nimble","coda","dplyr","tidyr","ggplot2","plotly","here","stringr","parallel","MASS",
  "readr","FNN","gridExtra","stringr","patchwork","scales","tibble","NbClust","cluster","dendextend","RColorBrewer")

# Check for missing packages and install if necessary
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, dependencies = TRUE)
}

# Load all required packages
invisible(lapply(required_packages, library, character.only = TRUE, quietly = TRUE))
