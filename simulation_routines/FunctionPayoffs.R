FunctionPayoffs = function (J, current.locations, current.prices, 
                            Alpha, tau.val,
                            distances, customer.mass){
  
  # DESCRIPTION
  # Calculate firm payoffs at current position
  # One calculation for single configuration (for final payoffs)
  # Seperate calculation for set of configurations (efficient calculation for testing new positions)
  
  # current.locations = locations[,t]; current.prices = prices[,t];
  # current.locations = locations; current.prices = prices; tau.val = tau
  
  numLocations = dim(distances)[1] #calculate number of 2^N locations (avoids import)
  FJ = J
  
  if (is.null(dim(current.locations))){# Calculate for single configuration (for final positions)
    
    # Determine customers' choices using utility function and calculate firm profits
    dist = matrix(distances[current.locations,],FJ,LN) #distances from current position to all points (lookup table)
    e.utility.matrix = exp(Alpha - tau.val * dist - current.prices) #calculate customers' utility from each firm
    if(FJ>1){divisors  = colSums(e.utility.matrix)}else{divisors = c(e.utility.matrix)}
    shares = t (t (e.utility.matrix) / (divisors + Phi))
    customers = shares %*% customer.mass
    payoffs = customers * current.prices
    return(payoffs)
    
  }else{ # Calculate for set of configurations (testing for moves)
    
    # Determine customers' choices using utility function
    dist = distances[t(current.locations),] #rows 1:FJ = firm 1's beliefs on distances
    price.mat = c(t(current.prices)) %o% rep(1,LN) #rows = firms' beliefs
    e.utility.matrix = exp(Alpha - tau.val * dist - price.mat) #calculate customers' utility from each firm
    e.utility.array = array(e.utility.matrix, c(FJ, FJ, LN)) #first column (2nd dim) is firm 1's beliefs
    divisors = apply(e.utility.array, c(2,3), sum) #sum of all e(.) terms for each customer, according to each firm's beliefs
    shares = e.utility.array / rep(1,FJ) %o% divisors #share of customers for each firm, according to each firm's beliefs
    #Note: shares[1,2,3] = firm 2's belief about firm 1's share of customer 3
    shares.mat = matrix(shares,F^2) #transform back into matrix form ...
    counter = (0:(FJ-1))*FJ+(1:FJ) # ...to select own payoffs (only these are relevant for move decision)
    own.shares = shares.mat[counter,]
    customers = own.shares %*% customer.mass #number of customers each firm believes it will receive
    payoffs = customers * diag(current.prices) #calculate payoffs = customers x price
    return(payoffs)
  }
}