# COMPETITION ON RUGGED LANDSCAPES
# SIMULATION v14

# Note this script requires parameters to be previously read from config file

simulate <- function(){
  
  # Import routines for simulation
  source("simulation_routines/FunctionPermissible.r")
  source("simulation_routines/FunctionLandscape.r")
  source("simulation_routines/FunctionPayoffs.r")
  source("simulation_routines/FunctionMove.r")
  
  # Load pre-calculated binary locations and identify permissible moves (calculating binaries is costly)
  load("simulation_routines/DataBinaries.r")
  if(N != N_bin) stop("N does not match N_bin!") # minor check
  permissible.moves = FunctionPermissible( 
    distances, N, location.step
  ) #identify permissible moves
  
  # -------------------------------------------------
  # INITIATE STORAGE
  
  # Customer landscape
  lst = matrix(NA,prod(LN,K,Tau,S),5) # landscape store
  ind.l = 1:LN # initiate storage index for landscape
  colnames(lst) = c("k", "tau", "s", "location", "mass")
  
  # Locations, prices, and payoffs
  rst = matrix(NA,prod(prod(J.values),T,K,Tau,S),9) #results store
  colnames(rst) = c("j", "t", "k", "tau", "s", "id", "location", "price", "payoff")
  r.shift = 0 # shifts row indices for storage
  
  # -------------------------------------------------
  # RUGGEDNESS LOOP
  for (ind.k in 1:K){ # ind.k=1;ind.tau=1;s=1 # for debugging
    k = K.values[ind.k]
    tic()
    
    # -------------------------------------------------
    # TRANSPORTATION COST LOOP (default = inactive)
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
          payoffs = FunctionPayoffs(J, locations, prices, Alpha, tau, distances, customer.mass) #calculate payoffs
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
            new.positions = FunctionMove(J, locations, prices, payoffs, Alpha, tau, price.step, stick, permissible.moves, distances, customer.mass)
            locations = new.positions$locations
            prices = new.positions$prices
            payoffs = FunctionPayoffs(J, locations, prices, Alpha, tau, distances, customer.mass) #calculate payoffs with new locations and prices
            
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
  
  # -------------------------------------------------
  # STORE RESULTS
  rst = rst[is.na(rst[,1])==FALSE,] #remove empty rows
  save(list = ls(), file = simulation.results.name)
  print("Simulation complete")
}