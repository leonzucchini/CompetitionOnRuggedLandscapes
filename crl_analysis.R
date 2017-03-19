# COMPETITION ON RUGGED LANDSCAPES
# ANALYSIS v14

analyse <- function(simulation.results.name){
  
  # Load simulation results
  load(simulation.results.name)
  
  #def_par = par() #save default graphics parameters for resetting
  
  # Define legends for plots
  j.leg = vector("list", length(J.values)); for (j in 1:length(J.values)){j.leg[[j]] = bquote(F == .(J.values[j]))}
  tau.leg = vector("list", Tau); for (tau in 1:Tau){tau.leg[[tau]] = bquote(tau== .(Tau.values[tau]))}
  k.leg = vector("list", K); for (k in 1:K){k.leg[[k]] = bquote(k== .(K.values[k]))}
  
  # #################################################
  # STRAIGHT COMPETITION
  # #################################################
  
  # Select parameter values for plots
  tau.pick = 2
  k.pick = 0
  j.pick = c(1,2,4,8)
  
  # ///////////////////////////
  # >>> FIGURE 2A: Mean performance over time
  # ///////////////////////////
  
  # Calculate payoff mean and sd over time and j
  payoff.mean = matrix(NA,T,length(j.pick))
  for (id.j in 1:length(j.pick)){
    j = j.pick[id.j]
    tic()
    for (t in 1:T){
      payoff.mean[t,id.j] = mean(rst[rst[,1]==j & rst[,2]==t & rst[,3]==k.pick & rst[,4]==tau.pick,9])  }
    toc()
  }
  
  # ///////////////////////////
  # >>> FIGURE 2B: Mean distance to nearest peak over time
  # ///////////////////////////
  
  # Identify peaks
  # NOTE: For large k.pick and tau.pick vectors this can take several minutes
  lst.short = lst[lst[,1]==k.pick & lst[,2]==tau.pick,]
  is.peak = numeric(dim(lst.short)[1]); id.peak = 1
  for (id.tau in 1:length(tau.pick)){
    for (id.k in 1:length(k.pick)){
      tic()
      for (s in 1:S){
        tau = tau.pick[id.tau]; k = k.pick[id.k]
        cust = lst.short[lst.short[,1]==k & lst.short[,2]==tau & lst.short[,3]==s,5] #customers for this run
        customer.mat = t(rep(1,LN) %o% cust)
        neighbours = t(matrix(customer.mat[distances<=1], N+1))
        is.peak[id.peak:(id.peak+LN-1)] = cust == apply(neighbours,1,max)
        id.peak = id.peak + LN
      }
      toc()
    }
  }
  
  # WARNING: The current version only works for a single peak (k=0)!!
  pl = lst.short[is.peak==TRUE,]
  rst.short = rst[rst[,3]==k.pick & rst[,4]==tau.pick,]
  this.peak = numeric(dim(rst.short)[1])
  for (id.k in 1:length(k.pick)){
    for (id.tau in 1:length(tau.pick)){
      tic()
      for (s in 1:S){
        tau = tau.pick[id.tau]; k = k.pick[id.k]
        peak = pl[pl[,1]==k & pl[,2]==tau & pl[,3]==s,4]
        this.peak[rst.short[,3]==k & rst.short[,4]==tau & rst.short[,5]==s] = peak
      }
      toc()
    }
  }
  dist.peak = numeric(dim(rst.short)[1])
  for (i in 1:length(dist.peak)){ dist.peak[i] = distances[rst.short[i,7],this.peak[i]] }
  
  # Calculate average distance to nearest peak (average for s and j, over k, tau and j)
  # WARNING: This only works because there are unique k and tau values!
  pd = matrix(NA,T,length(j.pick))
  for (id.j in 1:length(j.pick)){
    for (t in 1:T){
      j = j.pick[id.j]
      pd[t,id.j] = mean(dist.peak[rst.short[,1]==j & rst.short[,2]==t])
    }
  }
  
  # ///////////////////////////
  # >>> FIGURE 2C: Average movement over time
  # ///////////////////////////
  
  # Determine moves
  len = dim(rst)[1]
  rst.m = rst[order(rst[,1],rst[,3],rst[,4],rst[,5],rst[,6]),]
  same.k = rst.m[2:len,3] == rst.m[1:(len-1),3]
  same.tau = rst.m[2:len,4] == rst.m[1:(len-1),4]
  same.s = rst.m[2:len,5] == rst.m[1:(len-1),5]
  same.j = rst.m[2:len,6] == rst.m[1:(len-1),6]
  same.firm = same.k & same.tau & same.s & same.j
  move = rst.m[2:len,7] != rst.m[1:(len-1),7]
  move = move & same.firm
  
  # Calculate mean moves over time (over j and k)
  k.pick = c(0)
  rst.m.short = rst.m[1:(len-1),]
  move.mean = array(NA,c((T-1),length(j.pick),length(k.pick)))
  for (id.j in 1:length(j.pick)){
    j = j.pick[id.j]
    tic()
    for (id.k in 1:length(k.pick)){
      k = k.pick[id.k]
      for (t in 1:(T-1)){
        move.mean[t,id.j,id.k] = sum(move[rst.m.short[,1]==j & rst.m.short[,2]==t & rst.m.short[,3]==k & rst.m.short[,4]==tau.pick])
      }
      move.mean[,id.j,id.k] = move.mean[,id.j,id.k] / (j*S)
    }
    toc()
  }
  
  # ///////////////////////////
  # >>> FIGURE 2D: Changes in market leadership over time
  # ///////////////////////////
  
  # Probability the market leader will be dethroned over time
  #WARNING THIS DOES NOT WORK FOR VARIABLE TAU BUT IT DOES CONTAIN ANALYSES FOR BOTH SMOOTH AND RUGGED LANDSCAPES
  
  # Identify games (unique run identified by firms, k and s)
  game.no = cumsum( #identifier for "games" (inside-simulation runs)
    as.numeric( c(0, diff(rst[,1])) != 0 ) | #new number of firms
      as.numeric( c(0, diff(rst[,3])) != 0 ) | #new k
      as.numeric( c(0, diff(rst[,5])) != 0 ) #new s
  ) + 1
  
  #id of market leader (best performance) for each period and game
  market.leader = matrix(NA, max(game.no), T) 
  for (i in 1:max(game.no)){
    this.game = rst[game.no==i,c(2,6,9)] #periods, also ids and payoffs of firms in game
    this.t = this.game[,1]
    for (t in 1:T){
      market.leader[i,t] = this.game[this.t==t, 2][this.game[this.t==t, 3]==max(this.game[this.t==t,3])][1]
    }
  }
  
  # Identify when market leader changed
  leader.change = market.leader[,-T]!=market.leader[,-1]
  
  # Assign changes to simulation run, k and j, and calculate averages over s
  lut = matrix(NA,max(game.no),4) #lookup table to translate from game.no to j, k and s
  foo = unique(game.no)
  for (i in 1:max(game.no)){
    lut[i,] = c(foo[i],rst[which(game.no==foo[i])[1], c(1,3,5)])
  }
  colnames(lut) = c("game.no",colnames(rst)[c(1,3,5)])
  
  changes = array(NA,c(T-1,length(J.values),length(K.values))) #array: mean number of changes between all periods over j and k
  for (t in 1:(T-1)){ changes[t,,] = tapply(leader.change[,t],list(lut[,2],lut[,3]),mean) }
  
  # ///////////////////////////
  # >>> PLOT FIGURE 2
  # ///////////////////////////
  
  pdf("Figure2_Competition on Smooth Landscapes.pdf", pointsize = 10)
  par(mfrow=c(2,2),las=1, family="serif", oma=c(0,0,0,0))
  id.k = 1
  
  # >>> FIGURE 2A: Average performance
  mean.ylim= c(0,4); 
  plot(1:T, payoff.mean[,1], type="n", 
       ylim=mean.ylim,
       ylab = "Performance", 
       xlab="Periods",
       main = "(A) Average performance", cex.main = 1
  )
  for (id.j in 1:length(j.pick)){
    lines(1:T, payoff.mean[,id.j], lty = id.j)
  }
  legend(0,2, as.expression(j.leg[J.values==j.pick]), lty=(1:length(j.pick)), bty="n", border=0)
  
  # >>> FIGURE 2B: Average distance to peak
  pd.ylim = c(0,6)
  plot(1:T, pd[,1], type="n", 
       ylim=pd.ylim,
       ylab = "Distance to peak", 
       xlab="Periods",
       main = "(B) Average distance to peak", cex.main = 1
  )
  for (id.j in 1:length(j.pick)){
    lines(1:T, pd[,id.j], lty = id.j)
  }
  
  # >>> FIGURE 2C: Average moves per firm and period
  moves.ylim = c(0,0.6)
  plot(1:(T-1), move.mean[,1,id.k], type="n", 
       ylim=moves.ylim,
       ylab = "Moves per firm and period", 
       xlab="Periods",
       main = "(C) Average moves per firm and period", cex.main = 1
  )
  for (id.j in 1:length(j.pick)){
    lines(1:(T-1), move.mean[,id.j,id.k], lty = id.j)
  }
  
  # >>> FIGURE 2D: Proportion of market leaders dethroned
  mlchange.ylim = c(0,0.6)
  plot(1:(T-1), changes[,1,id.k], type="n", 
       ylim=mlchange.ylim,
       ylab = "Proportion of change in market leaders dethroned", 
       xlab="Periods",
       main = "(D) Average proportion of market leaders dethroned", cex.main = 1
  )
  for (id.j in 1:length(j.pick)){
    lines(1:(T-1), changes[,id.j,id.k], lty = id.j)
  }
  
  dev.off()
  
  # #################################################
  # COMPETITION ON RUGGED LANDSCAPES
  # #################################################
  
  # Select parameter values for plots
  tau.pick = 2
  k.pick = 0:9
  j.pick = c(1,2,4,8)
  
  # ///////////////////////////
  # >>> FIGURE 3A: Average performance in final period over j and k
  # ///////////////////////////
  
  # -------------------------------------------------
  # Calculate payoff mean in T over j and k
  payoff.mean = matrix(NA,length(k.pick),length(j.pick))
  for (id.j in 1:length(j.pick)){
    j = j.pick[id.j]
    for (id.k in 1:length(k.pick)){
      k = k.pick[id.k]
      payoff.mean[id.k,id.j] = mean(rst[rst[,1]==j & rst[,2]==T & rst[,3]==k & rst[,4]==tau.pick,9])
    }
  }
  
  # ///////////////////////////
  # >>> FIGURE 3B: Average distance to nearest peak in final period over j and k
  # ///////////////////////////
  
  # Identify peaks over j, k and s, calculate average number over j and k
  # NOTE: For large k.pick and tau.pick vectors this can take several minutes
  lst.short = lst[lst[,1]%in%k.pick & lst[,2]%in%tau.pick,]
  is.peak = numeric(dim(lst.short)[1]); id.peak = 1
  num.peaks = array(NA,c(length(k.pick),length(tau.pick),S))
  for (id.tau in 1:length(tau.pick)){
    for (id.k in 1:length(k.pick)){
      tic()
      for (s in 1:S){
        tau = tau.pick[id.tau]; k = k.pick[id.k]
        cust = lst.short[lst.short[,1]==k & lst.short[,2]==tau & lst.short[,3]==s,5] #customers for this run
        customer.mat = t(rep(1,LN) %o% cust)
        neighbours = t(matrix(customer.mat[distances<=1], N+1))
        is.peak[id.peak:(id.peak+LN-1)] = cust == apply(neighbours,1,max)
        num.peaks[id.k,id.tau,s] = sum(is.peak[id.peak:(id.peak+LN-1)] )
        id.peak = id.peak + LN
      }
      toc()
    }
  }
  #peaks.mean = apply(num.peaks,c(1,2),mean)
  #peaks.sd = apply(num.peaks,c(1,2),sd)
  
  # Identify nearest x peaks in periods "t.pick"
  x = 1
  t.pick = c(T)
  np = max(num.peaks) #maximal number of peaks
  pl = lst.short[is.peak==TRUE,]
  rst.short = rst[rst[,2]%in%t.pick & rst[,3]%in%k.pick & rst[,4]%in%tau.pick,]
  rst.short = rst.short[order(rst.short[,1],rst.short[,3],rst.short[,4],rst.short[,5],rst.short[,2]),]
  my.peaks = my.dist = matrix(NA,dim(rst.short)[1],np)
  for (id.k in 1:length(k.pick)){
    for (id.tau in 1:length(tau.pick)){
      tic()
      for (s in 1:S){
        tau = tau.pick[id.tau]; k = k.pick[id.k]
        which.rows = rst.short[,3]==k & rst.short[,4]==tau & rst.short[,5]==s
        these.peaks =  pl[pl[,1]==k & pl[,2]==tau & pl[,3]==s,4]
        my.peaks[which.rows,] = rep(1,sum(which.rows)) %o% c(these.peaks, rep(NA,dim(my.peaks)[2]-length(these.peaks)))
      }
      toc()
    }
  }
  
  # Calculate mean distance to peaks in "t.pick" for each company in each run
  avg.dist = numeric(dim(my.peaks)[1])
  for (i in 1:dim(my.peaks)[1]){
    my.dist[i,] = distances[rst.short[i,7],my.peaks[i,]]
    avg.dist[i] = mean(my.dist[i,][rank(my.dist[i,],ties.method="first")<=x],na.rm=TRUE)
  }
  
  # Summarize to means over runs (j and k) # NOTE THIS DOES NOT WORK WITH VARYING TAU
  avg.distances = tapply(avg.dist, list(rst.short[,1],rst.short[,3]), mean)
  
  # ///////////////////////////
  # >>> FIGURE 3C: Movement in final period over j and k
  # ///////////////////////////
  
  # Average moves in final period over j and k
  rst.m.short = rst.m[1:(len-1),]
  move.mean = array(NA,c(length(j.pick),length(k.pick)))
  for (id.j in 1:length(j.pick)){
    j = j.pick[id.j]
    tic()
    for (id.k in 1:length(k.pick)){
      k = k.pick[id.k]
      move.mean[id.j,id.k] = sum(move[rst.m.short[,1]==j & rst.m.short[,2]==(T-1) & rst.m.short[,3]==k & rst.m.short[,4]==tau.pick])
      move.mean[id.j,id.k] = move.mean[id.j,id.k] / (j*S)
    }
    toc()
  }
  
  # ///////////////////////////
  # >>> FIGURE 3D: Change of market leader in final period over j and k
  # ///////////////////////////
  
  # Note: Already calculated above for Figure 2d
  changes.last = changes[(T-1),,]
  
  
  # ///////////////////////////
  # >>> PLOT FIGURE 3
  # ///////////////////////////
  
  pdf("Figure3_Competition on Rugged Landscapes.pdf", pointsize = 10)
  par(mfrow=c(2,2),las=1, family="serif", oma=c(0,0,0,0))
  
  # >>> FIGURE 3A: Average performance in final period over j and k
  ylim.mean = c(0,4)
  plot(k.pick, payoff.mean[,1], type="n", 
       ylim=ylim.mean,
       ylab = "Performance", 
       xlab="Ruggedness (K)",
       main = "(A) Average performance (final period)", cex.main = 1, xaxt = "n"
  )
  for (id.j in 1:length(j.pick)){
    lines(k.pick, payoff.mean[,id.j], lty = id.j)
    points(k.pick, payoff.mean[,id.j], pch = 20)
  }
  legend(0,2, as.expression(j.leg[J.values==j.pick]), lty=(1:length(j.pick)), bty="n", border=0)
  axis(1, k.pick)
  
  # >>> FIGURE 3B: Average distance to nearest peak in final period over j and k
  dist.ylim = c(0,4)
  plot(k.pick, avg.distances[1,], type="n", 
       ylim=dist.ylim,
       ylab = "Distance to closest peak", 
       xlab="Ruggedness (K)",
       main = "(B) Average distance to closest peak (final period)", cex.main = 1, xaxt = "n"
  )
  for (id.j in 1:length(j.pick)){
    lines(k.pick, avg.distances[id.j,], lty = id.j)
    points(k.pick, avg.distances[id.j,], pch=20)
  }
  axis(1, k.pick)
  
  # >>> FIGURE 3C: Average moves (final period) over j and k
  moves.ylim = c(0,0.5)
  plot(k.pick, move.mean[1,], type="n", 
       ylim=moves.ylim,
       ylab = "Moves per firm and period", 
       xlab="Ruggedness (K)",
       main = "(C) Average moves per firm (final period)", cex.main = 1, xaxt = "n"
  )
  for (id.j in 1:length(j.pick)){
    lines(k.pick, move.mean[id.j,], lty = id.j)
    points(k.pick, move.mean[id.j,], pch=20)
  }
  axis(1, k.pick)
  
  # >>> FIGURE 3D: Average probability of change of leader (final period) over j and k
  changes.ylim = c(0,0.5)
  plot(k.pick, changes.last[1,], type="n", 
       ylim=changes.ylim,
       ylab = "Proportion of market leaders dethroned", 
       xlab="Ruggedness (K)",
       main = "(D) Proportion of market leaders dethroned (final period)", cex.main = 1, xaxt = "n"
  )
  for (id.j in 1:length(j.pick)){
    lines(k.pick, changes.last[id.j,], lty = id.j)
    points(k.pick, changes.last[id.j,], pch=20)
  }
  axis(1, k.pick)
  
  dev.off()
}