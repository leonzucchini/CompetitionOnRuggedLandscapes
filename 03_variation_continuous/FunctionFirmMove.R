FunctionFirmMove = function(N, J, this.c.loc, this.f.loc, Alpha, Tau, Phi, 
                            this.payoff, firm.step){

  # Function calculating which firms move depending on turn and anticipated benefits from customers
  # this.c.loc = c.loc; this.f.loc = f.loc; this.payoff = payoff

  # Firms pick new location, calculate profits, and decide on move
  f.loc.potential = this.f.loc #placeholder for test locations
  f.loc.change.1 = runif(J, 0, firm.step) #draw change in 1st dimension
  f.loc.change.2 = sqrt(firm.step^2 - f.loc.change.1^2)#*runif(1) #calculate change in 2nd dimension (constrained by firm step)
  signs = sample(c(-1,1),2,replace=TRUE) #determines in which direction the change happens
  f.loc.potential[, 1] = this.f.loc[,1] + signs[1]*f.loc.change.1
  f.loc.potential[, 2] = this.f.loc[,2] + signs[2]*f.loc.change.2 #new test positions
  f.loc.potential[f.loc.potential< 0] = 0; f.loc.potential[f.loc.potential > 1] = 1 #constrain firm locations to [0,1]

  # Firms draw turns and sequentially chose to move or not
  turns = sample.int(J, replace=F)
  f.loc.new = this.f.loc #list of new locations
  payoff.test = this.payoff
  for(id.j in 1:J){
    j = turns[id.j]
    f.loc.new[j, ] = f.loc.potential[j, ] #update focal firm's location to test location
    test.payoff = FunctionConsumerChoice(N, J, this.c.loc, f.loc.new, Alpha, Tau, Phi)$payoff #calculate payoff in new configuration
    if(test.payoff[j] <= this.payoff[j]){ f.loc.new[j,] = this.f.loc[j,] } #if revenues don't increase, revert to old position
  }

  ret.move = new.env()
  ret.move$loc = f.loc.new
  ret.move$turns = turns
  return(ret.move)
}