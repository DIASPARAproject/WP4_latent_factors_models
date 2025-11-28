# Set project root directory
project_root <- here::here()
cat("Project root directory:", project_root, "\n")

# Define subdirectory paths relative to the project root
# --> Each variable represents a folder where different 
#     types of files will be stored.
path_scripts <- file.path(project_root, "scripts")      # For R scripts
path_functions <- file.path(project_root, "function")   # For custom functions
path_data      <- file.path(project_root, "Table")      # For raw data tables (CSV, Excel, etc.)
path_processed <- file.path(project_root, "data")       # For cleaned or processed data
path_models    <- file.path(project_root, "model")      # For model files or scripts
path_results   <- file.path(project_root, "results")    # For outputs, figures, tables, reports

# Collect all critical directories in a vector
# --> These are the folders that must exist for the project to run correctly
required_dirs <- c(path_scripts, path_functions, path_data, path_processed, path_models, path_results)

# Check if each required directory exists
# --> If not, the script will create it automatically.
# --> A message is printed for each directory to inform the user.
for (dir_path in required_dirs) {
  
  if (!dir.exists(dir_path)) {
    # Case 1: Directory does NOT exist
    message("Directory missing. Creating: ", dir_path)
    
    # dir.create() creates the folder
    # recursive = TRUE ensures that any parent folders are also created if needed
    dir.create(dir_path, recursive = TRUE)
    
  } else {
    # Case 2: Directory already exists
    message("✓ Directory already exists: ", dir_path)
  }
}