# COMPETITION ON RUGGED LANDSCAPES
# RUN FILE

# Clear and set seed
rm(list = ls())
set.seed(0)

# Load libraries and config files
library(matlab)
source("config.r")
source("crl_simulation.r")
source("crl_analysis.r")

# Run simulation and store results
simulate()

# Load simulation results and analyze
analyse(simulation.results.name)