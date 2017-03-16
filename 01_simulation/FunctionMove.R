FunctionMove = function(J, current.locations, current.prices, current.payoffs,
                        Alpha, Tau, price.step, stick,
                        permissible.moves, distances, customer.mass){
  # DESCRIPTION
  # Firms calculate profits and decide on new positions
  # Note: Simultaneous moves
  # Note: Code allowing for global search is in "move" in version V1
 
  # current.locations = locations[,t-1]; current.prices = prices[,t-1]; current.payoffs = payoffs[,t-1]


  F = J
  # Declare storage for results
  new.locations = current.locations;
  new.prices = current.prices;

  # Select test locations (local search)
  rand.pick = ceiling(runif(F,0,dim(permissible.moves)[2])) #select column of permissible.moves)
  test.locations = permissible.moves[cbind(current.locations,rand.pick)] #select test location
  configs.locations = rep(1,F) %o% current.locations #duplicate current locations
  diag(configs.locations) = test.locations #update diag to test location (firm i's belief in ROW i)

  # Select test prices (constrained by priceStep)
  test.prices = current.prices + price.step * runif(F,-1,1) #select test price within price.step
  test.prices[test.prices<0] = 0
  configs.prices = rep(1,F) %o% current.prices #duplicate
  diag(configs.prices) = test.prices #row i = firm i's beliefs of prices (NOT columns)

  # Test attractiveness of new positions (assuming no change by others = simultaneous moves)
  test.payoffs = FunctionPayoffs(test.locations, test.prices, 
                                 Alpha, Tau, distances, customer.mass)

  # Assign new locations and prices to all firms
  index = test.payoffs > (1+stick) * current.payoffs #select firms for whom new position increases payoff
  new.locations[index] = test.locations[index]
  new.prices[index] = test.prices[index]

  # Return new prices and locations
  new.positions = new.env()
  new.positions$locations = new.locations
  new.positions$prices = new.prices
  return(new.positions)
}

