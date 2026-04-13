################################################################################
#                    PARALLEL BAYESIAN MCMC — COMMON TREND MODEL
#                    Model: E_x[t,i] = mu_x[i] + gamma[t]
################################################################################
#
# Available model files (change source() call below to switch):
#   - model/CommonTrendAR1.txt  : gamma[t] ~ AR1 process
#   - model/CommonTrendRW.txt   : gamma[t] ~ Random Walk
#   - model/CommonTrend.txt     : gamma[t] ~ White noise (iid)
#
################################################################################

library(nimble)
library(coda)
library(parallel)
library(readr)

# ------------------------------------------------------------------------------
# DATA LOADING
# ------------------------------------------------------------------------------

time_series <- read_csv("data/time_series_matrix.csv")
uncertainties <- read_csv("data/sigma_obs_matrix.csv")

y_obs <- as.matrix(time_series[-1])
omega_observed <- as.matrix(uncertainties[-1])

cat("Data dimensions:", nrow(y_obs), "years ×", ncol(y_obs), "series\n\n")

# ------------------------------------------------------------------------------
# MCMC CONFIGURATION
# ------------------------------------------------------------------------------

n.chains <- 3
n.keep <- 5000
n.thin <- 60
n.burnin <- 50000
n.iter <- n.keep * n.thin + n.burnin

cat("MCMC Setup:\n")
cat("  Total iterations per chain:", format(n.iter, big.mark = ","), "\n")
cat("  Burn-in period:",             format(n.burnin, big.mark = ","), "\n")
cat("  Final samples per chain:",    n.keep, "\n")
cat("  Total retained samples:",     n.chains * n.keep, "\n\n")

# ------------------------------------------------------------------------------
# DATA STRUCTURES FOR NIMBLE
# No groupes, no lambda_type — only n, nb_series, omega_obs
# ------------------------------------------------------------------------------

data.nimble <- list(
  y_obs = y_obs
)

const.nimble <- list(
  n  = dim(y_obs)[1],
  nb_series = dim(y_obs)[2],
  omega_obs = omega_observed
)

if (!dir.exists("data")) dir.create("data")
saveRDS(data.nimble,  file = "data/data.rds")
saveRDS(const.nimble, file = "data/const.rds")

cat("✓ data.rds and const.rds saved\n\n")

# ------------------------------------------------------------------------------
# INITIAL VALUES
# Simple initialization based on data empirical statistics
# ------------------------------------------------------------------------------

rhalfcauchy <- function(n, scale = 1) abs(rcauchy(n, location = 0, scale = scale))

inits <- function(chain_id = 1) {
  set.seed(chain_id * 42)
  list(
    mu_mu     = mean(y_obs, na.rm = TRUE),
    mu_x      = colMeans(y_obs, na.rm = TRUE),
    sd_mu     = rhalfcauchy(1, scale = 1),
    sd_x      = apply(y_obs, 2, sd, na.rm = TRUE),
    sd_gamma  = rhalfcauchy(1, scale = 1),
    phi       = runif(1, -0.5, 0.5)    # remove if using CommonTrend.txt or CommonTrendRW.txt
  )
}

inits.nimble.3chains <- list(inits(1), inits(2), inits(3))
inits.nimble.1chain  <- inits(1)

saveRDS(inits.nimble.1chain,  file = "data/inits.nimble.1chain.rds")
saveRDS(inits.nimble.3chains, file = "data/inits.nimble.3chains.rds")

inits.nimble.1chain  <- readRDS("data/inits.nimble.1chain.rds")
inits.nimble.3chains <- readRDS("data/inits.nimble.3chains.rds")

cat("✓ Initial values saved\n\n")

################################################################################
# PARALLEL MCMC EXECUTION
################################################################################

para <- "TRUE"

if (para == "TRUE") {
  
  cat("Starting parallel MCMC sampling for Common Trend model...\n\n")
  
  wd    <- getwd()
  my.cl <- makeCluster(n.chains, outfile = "")
  cat("✓ Created cluster with", n.chains, "worker processes\n")
  
  clusterExport(cl = my.cl, c('n.iter', 'n.burnin', 'n.thin', 'wd'),
                envir = .GlobalEnv)
  cat("✓ Exported MCMC parameters to all workers\n")
  
  clusterEvalQ(cl = my.cl, expr = {
    
    setwd(wd)
    library(nimble)
    library(coda)
    
    data.nimble  <- readRDS("data/data.rds")
    const.nimble <- readRDS("data/const.rds")
    
    #   "model/CommonTrendAR1.txt"  AR1 process on gamma
    #   "model/CommonTrendRW.txt"   Random Walk on gamma
    #   "model/CommonTrend.txt"     White noise on gamma
    source("model/CommonTrendAR1.txt")
    
    set.seed(123)
    model.nimble <- nimbleModel(code = model.nimble, name = 'model.nimble',
                                constants = const.nimble, data = data.nimble)
    compiled.model <- compileNimble(model.nimble)
    
    # Monitor: no factor/lambda — only common trend parameters
    monitor <- c(
      "mu_x", "mu_mu", "sd_mu",
      "sd_x", "sd_gamma",
      "phi",           # remove if using CommonTrend.txt or CommonTrendRW.txt
      "gamma",
      "E_x", "x"
    )
    
    conf.mcmc.model <- configureMCMC(model.nimble, thin = 1,
                                     monitors = monitor, enableWAIC = TRUE)
    MCMC.model      <- buildMCMC(conf.mcmc.model)
    compiled.MCMC.model <- compileNimble(MCMC.model, project = model.nimble,
                                         showCompilerOutput = TRUE)
  })
  
  cat("✓ NIMBLE models compiled on all workers\n")
  
  # ------------------------------------------------------------------------------
  # MCMC EXECUTION FUNCTION
  # ------------------------------------------------------------------------------
  
  mcmc_nimble_1chain <- function(inits.1chain) {
    runMCMC(compiled.MCMC.model,
            niter             = n.iter,
            nburnin           = n.burnin,
            nchains           = 1,
            thin              = n.thin,
            inits             = inits.1chain,
            progressBar       = TRUE,
            samples           = TRUE,
            samplesAsCodaMCMC = TRUE,
            summary           = FALSE,
            WAIC              = TRUE)
  }
  
  inits.nimble.3chains <- readRDS("data/inits.nimble.3chains.rds")
  list.values.cluster  <- inits.nimble.3chains
  
  # ------------------------------------------------------------------------------
  # PARALLEL MCMC SAMPLING
  # ------------------------------------------------------------------------------
  
  cat("Beginning intensive MCMC sampling...\n")
  cat("This will take considerable time (potentially hours).\n")
  
  start_time <- Sys.time()
  cat("Start time:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  mcmc_parallel <- parLapply(cl = my.cl, list.values.cluster, fun = mcmc_nimble_1chain)
  
  end_time <- Sys.time()
  
  # ------------------------------------------------------------------------------
  # PROCESS RESULTS
  # ------------------------------------------------------------------------------
  
  cat("\nMCMC sampling completed!\n")
  cat("End time:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  mcmc <- mcmc.list(list(mcmc_parallel[[1]]$samples,
                         mcmc_parallel[[2]]$samples,
                         mcmc_parallel[[3]]$samples))
  
  WAIC_3chains_separate <- list(mcmc_parallel[[1]]$WAIC,
                                mcmc_parallel[[2]]$WAIC,
                                mcmc_parallel[[3]]$WAIC)
  
  stopCluster(my.cl)
  cat("✓ Computational cluster shut down\n")
  
  saveRDS(mcmc, file = file.path(path_results, "mcmc_samples.rds"))
  cat("✓ MCMC samples saved to: results/mcmc_samples.rds\n")
  
  # ------------------------------------------------------------------------------
  # OVERALL WAIC
  # ------------------------------------------------------------------------------
  
  cat("Calculating overall WAIC...\n")
  
  data.nimble  <- readRDS("data/data.rds")
  const.nimble <- readRDS("data/const.rds")
  source("model/CommonTrendAR1.txt")   # ← same model as above
  
  myModel  <- nimbleModel(code = model.nimble, name = 'model.nimble',
                          constants = const.nimble, data = data.nimble)
  CmyModel <- compileNimble(myModel)
  samples  <- do.call(rbind, lapply(mcmc_parallel, function(x) x$samples))
  WAIC_all_chains <- calculateWAIC(samples, myModel)
  
  # ------------------------------------------------------------------------------
  # RESULTS SUMMARY
  # ------------------------------------------------------------------------------
  
  execution_time <- end_time - start_time
  cat("\nMCMC SAMPLING COMPLETED SUCCESSFULLY\n")
  cat("Execution time:", round(execution_time, 2), attr(execution_time, "units"), "\n")
  cat("Total samples retained:", format(n.chains * n.keep, big.mark = ","), "\n")
  cat("WAIC (lower is better):", round(WAIC_all_chains$WAIC, 2), "\n\n")
  
  cat("Individual chain WAIC values:\n")
  for (i in seq_along(WAIC_3chains_separate)) {
    cat("  Chain", i, "WAIC:", round(WAIC_3chains_separate[[i]]$WAIC, 2), "\n")
  }
  
  saveRDS(WAIC_all_chains,       file = file.path(path_results, "WAIC_all_chains.rds"))
  saveRDS(WAIC_3chains_separate, file = file.path(path_results, "WAIC_3chains_separate.rds"))
  
  cat("\n✓ All objects successfully created in workspace:\n")
  cat("  - mcmc: Combined posterior samples from all chains\n")
  cat("  - WAIC_all_chains: Overall model fit statistic\n")
  cat("  - WAIC_3chains_separate: Individual chain WAIC values\n")
  
} # end para = "TRUE"

################################################################################
# END OF COMMON TREND MCMC SCRIPT
################################################################################