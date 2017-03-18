FunctionConsumerMove = function(N, this.c.loc, a_int, a_slope, this.choice, this.payoff, c.step){
  # Calculates new consumer taste positions given and consumer positions, 
  # attractiveness parameters, and consumer step length
  
  #this.c.loc = c.loc; this.choice = choice; this.payoff = payoff

  # Determine where fads are located
  #fads = matrix(NA,J,2)
  #for (j in 1:J){
  #  fads[j,1] = mean(this.c.loc[choice==j,1]) #fad dim 1
  #  fads[j,2] = mean(this.c.loc[choice==j,2]) #fad dim 2
  #}
  
  # Pick fad for each consumer and determine direction of movement
  dist.all = aperm(rep(1,N) %o% f.loc, c(1,3,2)) - (this.c.loc %o% rep(1,J)) #distance between each consumer and firm
  closest.firm = apply(apply(dist.all^2,c(1,3),sum),1,which.min) #select the closest firm for each consumer
  diff = matrix(NA,N,2)
  for(i in 1:N){diff[i,] = dist.all[i,,closest.firm[i]]} #distances (in 2-d) to nearest fad
 
  # Determine beauty of fad and calculate consumer movement
  beauty = a_int - (a_slope * this.payoff) #calculate fad's beauty
  #beauty = numeric(J)
  #beauty[this.payoff<a3]=a1; beauty[this.payoff>a3]=a2
  c.move = diff * beauty[closest.firm] %o% c(1,1) #calculate consumer movements
  new.c.loc = this.c.loc + c.step * c.move
  #new.c.loc[new.c.loc>1] = 1; new.c.loc[new.c.loc<0] = 0 #limit movement to within boundaries
  cmove = new.env()
  cmove$new.c.loc = new.c.loc
  cmove$beauty = beauty
  return(cmove)
}