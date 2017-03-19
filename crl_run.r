# COMPETITION ON RUGGED LANDSCAPES
# RUN FILE

# Clear and set seed
rm(list = ls())
set.seed(0)

# Load libraries and config files
library(matlab)

# Load configuration
source("crl_config.r")

# Run simulation and store results
source("crl_simulation.r")
simulate()

# Load simulation results and analyze
source("crl_analysis.r")
analyse(simulation.results.name)

# Clear up
file.remove(simulation.results.name)