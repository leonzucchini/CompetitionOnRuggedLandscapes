# COMPETITION ON RUGGED LANDSCAPES
# CONFIGURATION FILE 

# Set WD
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #note this only works in RStudio

# Storage for simulation results
simulation.results.name = paste("SimulationResults_",Sys.Date(),".r",sep="")

# Define tunable parameters (these are the ones changed and compared in the simulation)
K.values = 0:9                          # interactions in landscape
K = length(K.values)                    # number of k values
Tau.values = c(2)                       # transportation costs
Tau = length(Tau.values)                # number of tau values
J.values = c(1,2,4,8)                   # number of firms in the landscape

# Define landscape and simulation parameters
S = 10                                 # number of simulation runs
T = 20                                  # number of periods
N = 10                                  # size (dimensionality) of landscape
LN = 2^N                                # length of landscape

# Define competition and search parameters
# Note firm seeding (with location and price) is done within loops in simulation
Phi = exp(1)                            # value of outside option
Alpha = 1+log(Phi)                      # pure utility of good
stick = 0.0                             # stickiness parameter (default 0 = unused)
price.step = 0                          # max step size for price (absolute, default 0 = unused)
location.step = 1                       # max step size for location (Hamming distance)