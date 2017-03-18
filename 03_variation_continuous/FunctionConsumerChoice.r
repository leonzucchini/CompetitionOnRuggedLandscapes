FunctionConsumerChoice = function(N, j, this.c.loc, this.f.loc, Alpha, Tau, Phi){
	# Function calculating consumer choice based on current consumer and firm locations
	# Uses discrete choice model
	
	# this.c.loc = c.loc; this.f.loc = f.loc
	
	# Calculate distance between customers and firms (checked 130119)
	foo = aperm( rep(1, N) %o% this.f.loc, c(2, 1, 3)) #repeated firm locations
	bar = rep(1, j) %o% this.c.loc #repeated customer locations
	baz = (foo - bar)^2
	dist = sqrt(baz[,,1] + baz[,,2]) #distance between customers and firms
	
	# Calculate consumers' utilities and probability of choice from each firm
	e.u = exp(Alpha - Tau * dist) #exp utilities of each consumer (cols) from each firm (rows)
	if(j==1) #probability of choice
	{ 
	  prob = e.u / (e.u + Phi) #only one firm
	}else{
	  prob = e.u / ( rep(1, j) %o% ( apply(e.u, 2, sum) + Phi ) ) #many firms
	}
	
	# Assign consumers to one of the firms (based on probability distribution)
	if(j>1){cumprob = apply(prob, 2, cumsum)} #cumulated probabilities (within consumer over firms) only if many firms
	rand = rep(1, j) %o% runif(N) #random draw per consumer
	bigger = if(j==1){rand > prob}else{rand > cumprob} #check where random draw lands
	choice = colSums(bigger) +1 #assign to firm => customers with choice == j+1 do not purchase
	choice[choice == j+1] = 0
  
	# Calculate firm profits
	payoff = numeric(j)
  for(j in 1:j){ payoff[j] = sum(choice==j) }
  
	ret.choice = new.env()
	ret.choice$choice = choice
	ret.choice$payoff = payoff
	return(ret.choice)
}