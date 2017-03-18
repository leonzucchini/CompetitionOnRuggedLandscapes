# COMPETITION ON RUGGED LANDSCAPES - SIMULATION 
# v14

# #################################################
# SETUP

# -------------------------------------------------
# SET WD, IMPORT LIBRARIES AND FUNCTIONS

# Clear and set WD
rm(list = ls())
setwd("/Users/Leon/Documents/02_Research_Learning/Research/CRL/")

# Load libraries and source functions
library(matlab)
source("FunctionPermissible.r")
source("FunctionLandscape.r")
source("FunctionPayoffs.r")
source("FunctionMove.r")

# Set seed and file storage
set.seed(0)
save.name = paste("SimulationOutput/SimulationResults_",Sys.Date(),".r",sep="")

# -------------------------------------------------
# DEFINE GLOBAL PARAMETERS, LOAD PRE-CALCULATED DATA

# Tunable parameters (these are the ones changed and compared in the simulation)
K.values = 0:9                          # interactions in landscape
K = length(K.values)                    # number of k values
Tau.values = c(2)                       # transportation costs
Tau = length(Tau.values)                # number of tau values
J.values = c(1,2,4,8)                   # number of firms in the landscape

# Landscape and simulation parameters
S = 100                                 # number of simulation runs
T = 80                                  # number of periods
N = 10                                  # size (dimensionality) of landscape
LN = 2^N                                # length of landscape

# Competition and search parameters
# Note firm seeding (with location and price) is done within loops
Phi = exp(1)                            # value of outside option
Alpha = 1+log(Phi)                      # pure utility of good
stick = 0.0                             # stickiness parameter
price.step = 0                          # max step size for price (absolute)
location.step = 1                       # max step size for location (Hamming distance)

# Load binary locations and identify permissible moves
# Note this part of the code laods pre-calculated locations for speed purposes
load("DataBinaries.r") 
if(N != N_bin) stop("N does not match N_bin!") # minor check
permissible.moves = FunctionPermissible( 
  distances, N, location.step
  ) #identify permissible moves

# -------------------------------------------------
# INITIATE GLOBAL STORAGE

# Customer landscape
lst = matrix(NA,prod(LN,K,Tau,S),5)     # landscape store
ind.l = 1:LN                            # initiate storage index for landscape
colnames(lst) = c("k", "tau", "s", "location", "mass")

# Locations, prices, and payoffs
rst = matrix(NA,prod(prod(J.values),T,K,Tau,S),9) #results store
colnames(rst) = c("j", "t", "k", "tau", "s", "id", "location", "price", "payoff")
r.shift = 0                              # shifts row indices for storage

# #################################################
# CALCULATION (LOOPS)

# -------------------------------------------------
# RUGGEDNESS LOOP
for (ind.k in 1:K){                      # ind.k=1;ind.tau=1;s=1 # for debugging
  k = K.values[ind.k]
  tic()

  # -------------------------------------------------
  # TRANSPORTATION COST LOOP
  for (ind.tau in 1:Tau){
    tau = Tau.values[ind.tau]

    # -------------------------------------------------
    # SIMULATION LOOP
    for (s in 1:S){
      
      # Initiate and store customer landscape
      customer.mass = FunctionLandscape(binaries,k) #initiate landscape
      customer.mass = customer.mass / mean(customer.mass)
      lst[ind.l,1] = rep(k, LN)
      lst[ind.l,2] = rep(tau, LN)
      lst[ind.l,3] = rep(s, LN)
      lst[ind.l,4] = 1:LN
      lst[ind.l,5] = customer.mass
      ind.l = ind.l + LN

      # -------------------------------------------------
      # FIRM-NUMBER LOOP
      # NOTE: This would be replaced by entry/exit function for selection mechanism
      for (J in J.values){ #J=3
      ind.store = (1:J) + r.shift #initiate competition storage index

        # -------------------------------------------------
        # TIME LOOP
        t = 1

          # Seed firms and calculate profits
          locations = sample(1:LN,J,TRUE) #initial location for firms
          prices = rep(1L,J) #initial price for firms
          payoffs = FunctionPayoffs(locations, prices, Alpha, tau, distances, customer.mass) #calculate payoffs
          new.positions = NA

          # Store location, prices, and payoffs results
          rst[ind.store,1] = rep(J, J)
          rst[ind.store,2] = rep(t, J)
          rst[ind.store,3] = rep(k, J)
          rst[ind.store,4] = rep(tau, J)
          rst[ind.store,5] = rep(s, J)
          rst[ind.store,6] = 1:J
          rst[ind.store,7] = locations
          rst[ind.store,8] = prices
          rst[ind.store,9] = payoffs
          ind.store = ind.store + J #update storate index

        for (t in 2:T){ #t=2
          # Update locations and prices, calculate payoffs
          new.positions = FunctionMove(locations, prices, payoffs, Alpha, tau, price.step, permissible.moves, distances, customer.mass)
          locations = new.positions$locations
          prices = new.positions$prices
          payoffs = FunctionPayoffs(locations, prices, Alpha, tau, distances, customer.mass) #calculate payoffs with new locations and prices

          # Store location, prices, and payoffs results
          rst[ind.store,1] = rep(J, J)
          rst[ind.store,2] = rep(t, J)
          rst[ind.store,3] = rep(k, J)
          rst[ind.store,4] = rep(tau, J)
          rst[ind.store,5] = rep(s, J)
          rst[ind.store,6] = 1:J
          rst[ind.store,7] = locations
          rst[ind.store,8] = prices
          rst[ind.store,9] = payoffs
          ind.store = ind.store + J #update storate index
          if (t==T){r.shift = max(ind.store)-J}
        } # end of time loop
      } # end of firm-number loop
    } # end of simulation loop  
  } # end of transportation cost loop
  toc()
} # end of ruggedness loop

# #################################################
# SAVE RESULTS
rst = rst[is.na(rst[,1])==FALSE,]
save.image(save.name)
source("crl_analysis_v14.r")