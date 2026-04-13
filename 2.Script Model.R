################################################################################
#                    PARALLEL BAYESIAN MCMC FOR DYNAMIC FACTOR ANALYSIS
#                      PART 2: MCMC PROCESSING AND DATA FOR NIMBLE
################################################################################
#
# Script: Bayesian Dynamic Factor Analysis using NIMBLE with parallel MCMC
# Author: Jeremy Bourdon (jeremy.bourdon@agrocampus-ouest.fr)
# Date: September 2025
#
# Description:
# This script performs Bayesian inference on Dynamic Factor Analysis (DFA) models
# using parallel Markov Chain Monte Carlo (MCMC) sampling via the NIMBLE framework.
# The analysis identifies common latent trends across multiple time series and
# quantifies uncertainty in model parameters and predictions.
#
################################################################################
# WHAT IS DYNAMIC FACTOR ANALYSIS (DFA)?
################################################################################
#
# Dynamic Factor Analysis is a dimension reduction technique for multivariate
# time series that identifies a small number of unobserved "common factors" or
# "latent trends" that drive the variation across multiple observed time series.
#
# Key Concepts:
#
# 1. DIMENSION REDUCTION:
#    Instead of analyzing n separate time series independently, DFA identifies
#    K common factors (typically K=2-5) that explain most of the shared variation.
#    This reduces complexity while preserving the main temporal patterns.
#
# 2. COMMON vs. SERIES-SPECIFIC VARIATION:
#    - Common factors: Shared trends affecting multiple time series
#    - Factor loadings: How strongly each series responds to each common factor
#    - Series-specific variation: Unique fluctuations in individual time series
#    - Observation error: Measurement uncertainty in the data
#
# 3. ECOLOGICAL INTERPRETATION:
#    Common factors often represent:
#    - Large-scale environmental drivers (climate, ocean conditions)
#    - Ecosystem-wide processes (productivity, predation pressure)
#    - Anthropogenic impacts (fishing pressure, habitat loss)
#    - Methodological factors (changes in survey methods)
#
################################################################################
# KEY MODEL PARAMETERS TO ESTIMATE
################################################################################
#
# 1. LATENT FACTORS (factor):
#    - The unobserved common trends driving the time series
#    - Dimensions: [n_years x K_factors]
#    - Most important output for ecological interpretation
#
# 2. FACTOR LOADINGS (lambda):
#    - Strength and direction of each series' response to each factor
#    - Dimensions: [n_series x K_factors]
#
# 3. AUTOREGRESSIVE COEFFICIENTS (phi):
#    - Measure persistence/memory in each latent factor
#    - phi ≈ 1: Random walk behavior
#    - phi ≈ 0: White noise behavior
#    - phi < 0: Oscillatory behavior
#
# 4. PROCESS VARIANCES (sd_factor):
#    - Variability in latent factor innovations
#    - Controls how much factors can change between time steps
#    - Higher values = more flexible/variable trends
#
# 5. OBSERVATION ERRORS (omega_obs or sd_x):
#    - Measurement uncertainty in observed data
#    - Can be fixed (from uncertainty data) or estimated
#    - Balances model fit vs. smoothness of latent trends
#
# 6. SERIES-SPECIFIC MEANS (mu_x):
#    - Baseline level for each time series
#    - Accounts for different scales/units across series
#
################################################################################
# REQUIRED INPUT FILES AND DATA STRUCTURE
################################################################################
#
# This script expects the following files to exist:
#
#    DATA STRUCTURE REQUIREMENTS:
#    - x: Observation matrix [n_years x n_series]
#    - y: Uncertainty matrix (same dimensions as x)
#    - triangular_mask: Identifiability constraint matrix
#    - K: Number of latent factors to estimate
#
# ------------------------------------------------------------------------------
# DATA PREPARATION FOR NIMBLE DFA MODEL
# ------------------------------------------------------------------------------

time_series <- read_csv("data/time_series_matrix.csv")
uncertainties <- read_csv("data/sigma_obs_matrix.csv")

# Convert input data to matrix format (required by NIMBLE)
y_obs <- as.matrix(time_series[-1])              # Time series observations (years × populations)
omega_observed  <- as.matrix(uncertainties[-1])
K <- 3                                     # Number of latent factors to extract

n_truncate <- 3

years_full <- merged_table$year
n_total    <- nrow(y_obs)
n_fit      <- n_total - n_truncate

cat("Années totales    :", n_total, "(", min(years_full), "–", max(years_full), ")\n")
cat("Années pour le fit:", n_fit,   "(", min(years_full), "–", years_full[n_fit], ")\n")
cat("Années tronquées  :", n_truncate, "(", years_full[n_fit + 1], "–", max(years_full), ")\n\n")

# Sauvegarder les données complètes pour la comparaison post-hoc
y_obs_full <- y_obs

# Masquer les 3 dernières années (→ NA = prédites par le modèle)
y_obs_truncated <- y_obs
y_obs_truncated[(n_fit + 1):n_total, ] <- NA


# ------------------------------------------------------------------------------
# MCMC CONFIGURATION PARAMETERS
# ------------------------------------------------------------------------------

n.chains <- 3               # Number of parallel MCMC chains
n.keep <- 5000              # Posterior samples to retain per chain
n.thin <- 400                 # Thinning interval (saves every 100th sample)
n.burnin <- 200000           # Burn-in period (discarded samples)
n.iter <- n.keep * n.thin + n.burnin  # Total iterations per chain

cat("MCMC Setup:\n")
cat("  Total iterations per chain:", format(n.iter, big.mark = ","), "\n")
cat("  Burn-in period:", format(n.burnin, big.mark = ","), "\n")
cat("  Final samples per chain:", n.keep, "\n")
cat("  Total retained samples:", n.chains * n.keep, "\n\n")

source(file.path(path_scripts,"PCA.R"))

y_obs <- y_obs_truncated
lambda_type <- matrix(0, nrow = ncol(y_obs), ncol = K)
lambda_type[triangular_mask == 1 & positive_mask == 0] <- 1  
lambda_type[triangular_mask == 1 & positive_mask == 1] <- 2  

# ------------------------------------------------------------------------------
# PREPARE DATA STRUCTURES FOR NIMBLE
# ------------------------------------------------------------------------------

# Create data list for NIMBLE (observations and uncertainties)
data.nimble <- list(
  y_obs = y_obs                     # Matrix of time series observations
)

# Create constants list for NIMBLE (dimensions and constraints)
const.nimble <- list(
  n = dim(y_obs)[1],                     # Number of time points
  nb_series = dim(y_obs)[2],             # Number of time series
  K = K,
  lambda_type = lambda_type,
  omega_obs = omega_observed         # Observation error standard deviations
)

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Save data objects to disk (needed for parallel workers)
saveRDS(data.nimble, file = "data/data.rds")
saveRDS(const.nimble, file = "data/const.rds")

# ------------------------------------------------------------------------------
# DEFINE INITIAL VALUES FOR MCMC CHAINS
# ------------------------------------------------------------------------------

# Half-Cauchy sampler
rhalfcauchy <- function(n, scale = 1) {
  abs(rcauchy(n, location = 0, scale = scale))
}

# IMPORTANT: Smart initialization using PCA with chain-specific perturbations
# This ensures chains start near a sensible solution but explore independently

inits <- function(chain_id = 1) {
  
  set.seed(chain_id * 42)
  perturb <- function(x, sd = 0.05) x + rnorm(length(x), 0, sd * abs(x) + 1e-4)

  lambda_free_init     <- pca_loadings_constrained
  lambda_positive_init <- abs(pca_loadings_constrained)
  
  lambda_free_init     <- apply(lambda_free_init,     2, perturb, sd = 0.05)
  lambda_positive_init <- pmax(apply(lambda_positive_init, 2, perturb, sd = 0.05), 1e-4)
  
  list(
    mu_mu = mean(y_obs, na.rm = TRUE),
    mu_x  = colMeans(y_obs, na.rm = TRUE),
    
    sd_mu     = 0.2,
    sd_x      = pmax(sd_x_init * runif(ncol(y_obs), 0.9, 1.1), 0.01),
    
    sd_factor = pmax(sd_factor_init * runif(K, 0.9, 1.1), 0.01),
    
    lambda_free     = lambda_free_init,
    lambda_positive = lambda_positive_init
    
    # phi = runif(K, 0.3, 0.7)
  )
}

# Generate initial values for multiple chains
# Each chain gets slightly different starting values
inits.nimble.3chains <- list(
  inits(chain_id = 1),
  inits(chain_id = 2),
  inits(chain_id = 3)
)

inits.nimble.1chain <- inits(chain_id = 1)

# Save initial values to disk (parallel workers will load these)
ifelse(!dir.exists(file.path("./", "data")), dir.create(file.path("./", "data")), "Dir ./data already exists")
saveRDS(inits.nimble.1chain, file = "data/inits.nimble.1chain.rds")
saveRDS(inits.nimble.3chains, file = "data/inits.nimble.3chains.rds")

# Reload initial values (ensures they're available in current environment)
inits.nimble.1chain <- readRDS("data/inits.nimble.1chain.rds")
inits.nimble.3chains <- readRDS("data/inits.nimble.3chains.rds")

################################################################################
# PARALLEL MCMC EXECUTION
################################################################################

# Control variable for parallel execution
para <- "TRUE"  # Set to "FALSE" to disable parallel processing

if(para == "TRUE") {
  
  cat("Starting parallel MCMC sampling for DFA model...\n\n")
  
  # ------------------------------------------------------------------------------
  # CLUSTER SETUP
  # ------------------------------------------------------------------------------
  
  # Create computational cluster with one worker per MCMC chain
  # outfile = "" allows console output from workers to be displayed
  my.cl <- makeCluster(n.chains, outfile = "")
  cat("✓ Created cluster with", n.chains, "worker processes\n")
  
  # Export MCMC parameters to all worker processes
  # Workers need these values to run MCMC with consistent settings
  clusterExport(cl = my.cl, c('n.iter','n.burnin','n.thin'), envir = .GlobalEnv) 
  cat("✓ Exported MCMC parameters to all workers\n")
  
  # ------------------------------------------------------------------------------
  # WORKER INITIALIZATION
  # ------------------------------------------------------------------------------
  
  # Execute initialization code on all workers simultaneously
  # This sets up the NIMBLE model and compiles it on each worker
  clusterEvalQ(cl = my.cl, expr = {
    
    # Load required packages on each worker
    library(nimble)  # Bayesian modeling framework
    library(coda)    # MCMC diagnostics and output handling
    
    # Load data and constants that were saved to disk
    data.nimble <- readRDS("data/data.rds")
    const.nimble <- readRDS("data/const.rds")
    
    # Load NIMBLE model definition from external file
    # This file contains the Bayesian DFA model specification
    source("model/DFA_AR1.txt")
    
    # Create NIMBLE model object
    # This builds the computational graph for the Bayesian model
    set.seed(123)  # Reproducible model initialization
    model.nimble <- nimbleModel(code = model.nimble, name = 'model.nimble',
                                constants = const.nimble, data = data.nimble)
    
    # Compile model to C++ for fast execution
    # This step significantly improves MCMC sampling speed
    compiled.model <- compileNimble(model.nimble)
    
    # Define which parameters to monitor during MCMC
    # These are the quantities we want to estimate from the posterior
    monitor <- c(
      "mu_x", "mu_mu","sd_factor", "sd_x", "sd_mu",
      "factor", "lambda","lambda_free", "lambda_positive",
      "E_x", "x","phi")
    
    # Configure MCMC algorithm
    # enableWAIC = TRUE calculates model selection criterion
    conf.mcmc.model <- configureMCMC(model.nimble, thin = 1, monitors = monitor, enableWAIC = TRUE)
    MCMC.model <- buildMCMC(conf.mcmc.model)
    # Compile MCMC algorithm to C++ for maximum efficiency
    compiled.MCMC.model <- compileNimble(MCMC.model, project = model.nimble, showCompilerOutput = TRUE)
  })
  
  cat("✓ NIMBLE models compiled on all workers\n")
  
  # ------------------------------------------------------------------------------
  # MCMC EXECUTION FUNCTION
  # ------------------------------------------------------------------------------
  
  # Define function to run MCMC on a single chain
  # This function will be called by each worker with different initial values
  mcmc_nimble_1chain <- function(inits.1chain) {
    runMCMC(compiled.MCMC.model,
            niter = n.iter,                   # Total iterations
            nburnin = n.burnin,               # Burn-in period
            nchains = 1,                      # One chain per worker
            thin = n.thin,                    # Thinning interval
            inits = inits.1chain,             # Initial values for this chain
            progressBar = TRUE,               # Show progress
            samples = TRUE,                   # Return posterior samples
            samplesAsCodaMCMC = TRUE,         # Format output as coda mcmc object
            summary = FALSE,                  # Skip summary statistics (saves memory)
            WAIC = TRUE)                      # Calculate WAIC for model comparison
  }
  
  # Load initial values for each chain
  inits.nimble.3chains <- readRDS("data/inits.nimble.3chains.rds")
  list.values.cluster <- inits.nimble.3chains
  
  # ------------------------------------------------------------------------------
  # PARALLEL MCMC SAMPLING
  # ------------------------------------------------------------------------------
  
  cat("Beginning intensive MCMC sampling...\n")
  cat("This will take considerable time (potentially hours).\n")
  
  # Record start time for performance monitoring
  start_time <- Sys.time()
  cat("Start time:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # Execute MCMC sampling in parallel
  # Each worker runs one chain with different initial values
  mcmc_parallel <- parLapply(cl = my.cl, list.values.cluster, fun = mcmc_nimble_1chain)
  
  # Record completion time
  end_time <- Sys.time()
  
  # ------------------------------------------------------------------------------
  # PROCESS RESULTS
  # ------------------------------------------------------------------------------
  
  cat("\nMCMC sampling completed!\n")
  cat("End time:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  # Combine samples from all chains into coda mcmc.list object
  # This format is standard for MCMC diagnostics and analysis
  mcmc <- list(mcmc_parallel[[1]]$samples, mcmc_parallel[[2]]$samples, mcmc_parallel[[3]]$samples)
  mcmc <- mcmc.list(mcmc)
  
  # Extract WAIC values from each chain (for model selection)
  WAIC_3chains_separate <- list(mcmc_parallel[[1]]$WAIC, mcmc_parallel[[2]]$WAIC, mcmc_parallel[[3]]$WAIC)
  
  # Clean up computational resources
  stopCluster(my.cl)
  cat("✓ Computational cluster shut down\n")
  # Save MCMC samples to disk for future analysis
  cat("Saving MCMC samples to disk...\n")
  saveRDS(mcmc, file = file.path(path_results, "mcmc_samples.rds"))
  cat("✓ MCMC samples saved to: results/mcmc_samples.rds\n")
  
  # ------------------------------------------------------------------------------
  # CALCULATE OVERALL MODEL FIT STATISTICS
  # ------------------------------------------------------------------------------
  
  cat("Calculating overall WAIC...\n")
  
  # Reload model components for final WAIC calculation
  data.nimble <- readRDS("data/data.rds")
  const.nimble <- readRDS("data/const.rds")
  
  # Recreate model in main R session
  source("model/DFA_AR1.txt")
  myModel <- nimbleModel(code = model.nimble, name = 'model.nimble',
                         constants = const.nimble, data = data.nimble)
  CmyModel <- compileNimble(myModel)
  
  # Combine all samples for overall WAIC calculation
  samples <- do.call(rbind, lapply(mcmc_parallel, function(x) x$samples))
  WAIC_all_chains <- calculateWAIC(samples, myModel)

  # ------------------------------------------------------------------------------
  # RESULTS SUMMARY
  # ------------------------------------------------------------------------------
  
  cat("MCMC SAMPLING COMPLETED SUCCESSFULLY\n")
  
  # Execution summary
  execution_time <- end_time - start_time
  cat("Execution time:", round(execution_time, 2), attr(execution_time, "units"), "\n")
  cat("Total samples retained:", format(n.chains * n.keep, big.mark = ","), "\n")
  cat("Effective sample size per parameter: ~", round(n.chains * n.keep / ncol(mcmc[[1]])), "\n\n")
  
  # Model fit results
  cat("MODEL SELECTION RESULTS:\n")
  cat("WAIC (lower is better):", round(WAIC_all_chains$WAIC, 2), "\n\n")
  
  # Next steps guidance
  cat("NEXT STEPS:\n")
  cat("1. Check convergence: gelman.diag(mcmc)\n")
  cat("2. Check effective sample sizes: effectiveSize(mcmc)\n")
  cat("3. Examine posterior summaries: summary(mcmc)\n")
  cat("4. Plot traces: plot(mcmc)\n")
  cat("5. Analyze factor loadings and latent trends\n")
  
  # Display individual chain WAIC for diagnostic purposes
  cat("\nIndividual chain WAIC values:\n")
  for(i in 1:length(WAIC_3chains_separate)) {
    cat("Chain", i, "WAIC:", round(WAIC_3chains_separate[[i]]$WAIC, 2), "\n")
  }
  
  # Final confirmation
  cat("\n✓ All objects successfully created in workspace:\n")
  cat("  - mcmc: Combined posterior samples from all chains\n")
  cat("  - WAIC_all_chains: Overall model fit statistic\n")
  cat("  - WAIC_3chains_separate: Individual chain WAIC values\n")
  saveRDS(WAIC_all_chains, file = file.path(path_results, "WAIC_all_chains.rds"))
  saveRDS(WAIC_3chains_separate, file = file.path(path_results, "WAIC_3chains_separate.rds"))
} # end para = "TRUE"

################################################################################
# END OF PARALLEL MCMC SCRIPT
################################################################################
#
# Output objects created:
# - mcmc: mcmc.list object containing posterior samples from all chains
# - WAIC_all_chains: Overall WAIC for model comparison
# - WAIC_3chains_separate: WAIC calculated separately for each chain
#
# The mcmc object can now be used for:
# - Convergence diagnostics (gelman.diag)
# - Posterior analysis (summary, quantiles, credible intervals)
# - Model validation (posterior predictive checks)
# - Scientific interpretation (factor loadings, latent trends)
#
################################################################################ 
