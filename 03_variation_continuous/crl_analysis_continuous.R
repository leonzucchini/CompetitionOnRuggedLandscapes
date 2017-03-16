# COMPETITION ON RUGGED LANDSCAPES - CONTINUOUS VERSION
# ANALYSIS
# V1

# #################################################
# SET WORKING DIRECTORY, IMPORT SIMULATION RESULTS, DEFINE LEGENDS
# #################################################
analysis = function(){
  
  # Set working directory, load libraries
  #rm(list = ls())
  #setwd("P:\\Leon_TempSimulation")
  setwd("C:\\Users\\zucchini\\Dropbox\\01_Projects\\CRL\\01_Model\\CRL_Continuous\\") 
  #setwd("/Users/leonzucchini/Dropbox/01_Projects/CRL/01_Model/CRL_Continuous/")
  library(matlab)
  #def_par = par() #save default graphics parameters for resetting
  
  # Import data
  savedate = "SimulationResults_smooth_130709"
  save.name = paste(savedate,".r",sep="")
  load(save.name)
  
  # Define legends for plots
  j.leg = vector("list", length(J.values)); for (j in 1:length(J.values)){j.leg[[j]] = bquote(F == .(J.values[j]))}
  k.leg = vector("list", K); for (k in 1:K){k.leg[[k]] = bquote(k== .(K.values[k]))}
  
  # Identify peaks
  bivb1 = 10
  bivb2 = 4
  bp = t(matrix(c(bivb2,bivb2,bivb2,bivb1,bivb1,bivb2,bivb2,bivb1,bivb1,bivb2),2,5))
  mode = function(a){ans = (a[1]-1)/(a[1]+a[2]-2); return(ans)} #calculate mode of beta distribution with parameters as specified in customer landscape distribution
  pl = apply(bp,1,mode)
  peaks1 = c(pl[1],pl[1])
  peaks2 = matrix(c(pl[2],pl[3],pl[3],pl[2]),2,2)
  peaks4 = t(matrix(c(pl[4],pl[4],pl[4],pl[5],pl[5],pl[4],pl[5],pl[5]), 2,4))
  peaks = list(peaks1,peaks2,peaks4) #list object containing locations of peaks for 1, 2 and 4 peak landscapes
  
  # #################################################
  # STRAIGHT COMPETITION
  # #################################################
  
  # Select parameter values for plots
  k.pick = 1
  j.pick = c(1,2,4)
  
  # ///////////////////////////
  # >>> FIGURE 2A: Mean performance over time
  # ///////////////////////////
  
  # Calculate payoff mean and sd over time and j
  payoff.mean = matrix(NA,T,length(j.pick))
  for (id.j in 1:length(j.pick)){
    j = j.pick[id.j]
    tic()
    for (t in 1:T){
      payoff.mean[t,id.j] = mean(rst[rst[,1]==j & rst[,2]==t & rst[,3]==k.pick, 8])  }
    toc()
  }
  
  # ///////////////////////////
  # >>> FIGURE 2B: Mean distance to nearest peak over time
  # ///////////////////////////
  
  # Distances to (approximate) peak for one-peak landscape
  rst.short = rst[rst[,3]==k.pick,] #select results for quicker calculation
  this.peak = peaks[[1]]
  
  foo = rst.short[,6:7] # firm locations
  bar = rep(1, length(rst.short[,6])) %o% this.peak #repeated customer locations
  baz = (foo - bar)^2
  dist = sqrt(baz[,1] + baz[,2]) #distance between customers and firms
  
  # Calculate average distance to nearest peak (average for s, over j, for one peak)
  pd = matrix(NA,T,length(j.pick))
  for (id.j in 1:length(j.pick)){
    for (t in 1:T){
      j = j.pick[id.j]
      pd[t,id.j] = mean(dist[rst.short[,1]==j & rst.short[,2]==t])
    }
  }
  
  # ///////////////////////////
  # >>> FIGURE 2C: Average movement over time
  # ///////////////////////////
  
  # Determine moves
  len = dim(rst)[1]
  rst.m = rst[order(rst[,1],rst[,3],rst[,4],rst[,5],rst[,2]),]
  same.j = rst.m[2:len,1] == rst.m[1:(len-1),1]
  same.k = rst.m[2:len,3] == rst.m[1:(len-1),3]
  same.s = rst.m[2:len,4] == rst.m[1:(len-1),4]
  same.id = rst.m[2:len,5] == rst.m[1:(len-1),5]
  same.firm = same.k & same.s & same.j & same.id
  move = ( rst.m[2:len,6] != rst.m[1:(len-1),6] ) & ( rst.m[2:len,7] != rst.m[1:(len-1),7] )
  move = move & same.firm
  
  # Calculate mean moves over time (avg over s, over j, for one peak)
  rst.m.short = rst.m[1:(len-1),]
  move.mean = array(NA,c((T-1),length(j.pick),length(k.pick)))
  for (id.j in 1:length(j.pick)){
    j = j.pick[id.j]
    tic()
    for (id.k in 1:length(k.pick)){
      k = k.pick[id.k]
      for (t in 1:(T-1)){
        move.mean[t,id.j,id.k] = sum(move[rst.m.short[,1]==j & rst.m.short[,2]==t & rst.m.short[,3]==k])
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
      as.numeric( c(0, diff(rst[,4])) != 0 ) #new s
  ) + 1
  
  #id of market leader (best performance) for each period and game
  market.leader = matrix(NA, max(game.no), T) 
  for (i in 1:max(game.no)){
    this.game = rst[game.no==i,c(2,5,8)] #periods, also ids and payoffs of firms in game
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
    lut[i,] = c(foo[i],rst[which(game.no==foo[i])[1], c(1,3,4)])
  }
  colnames(lut) = c("game.no",colnames(rst)[c(1,3,4)])
  
  changes = array(NA,c(T-1,length(J.values),length(K.values))) #array: mean number of changes between all periods over j and k
  for (t in 1:(T-1)){ changes[t,,] = tapply(leader.change[,t],list(lut[,2],lut[,3]),mean) }
  
  # ///////////////////////////
  # >>> PLOT FIGURE 2
  # ///////////////////////////
  
  win.metafile("Figure2_Competition on Smooth Landscapes_continuous.wmf")
  par(mfrow=c(2,2),las=1, family="serif", oma=c(0,0,0,0))
  id.k = 1
  
  # >>> FIGURE 2A: Average performance
  mean.ylim= c(0,110); 
  plot(1:T, payoff.mean[,1], type="n", 
       ylim=mean.ylim,
       ylab = "Performance", 
       xlab="Periods",
       main = "(A) Average performance", cex.main = 1
  )
  for (id.j in 1:length(j.pick)){
    lines(1:T, payoff.mean[,id.j], lty = id.j)
  }
  legend(40,50, as.expression(j.leg[J.values==j.pick]), lty=(1:length(j.pick)), bty="n", border=0)
  
  # >>> FIGURE 2B: Average distance to peak
  pd.ylim = c(0,0.4)
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
  moves.ylim = c(0,0.4)
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
  mlchange.ylim = c(0,1)
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
  k.pick = c(1,2,4)
  j.pick = c(1,2,4)
  
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
      payoff.mean[id.k,id.j] = mean(rst[rst[,1]==j & rst[,2]==T & rst[,3]==k])
    }
  }
  
  # ///////////////////////////
  # >>> FIGURE 3B: Average distance to nearest peak in final period over j and k
  # ///////////////////////////
  
  # Identify nearest peak in periods "t.pick"
  t.pick = c(T) #period for which peaks are selected
  rst.short = rst[rst[,2]%in%t.pick & rst[,3]%in%k.pick,] #subset of observations for relevant periods and ruggedness
  rst.short = rst.short[order(rst.short[,1],rst.short[,3],rst.short[,4],rst.short[,5],rst.short[,2]),]
  
  my.peaks = array(NA,c(2,4,dim(rst.short)[1])) #repeated peak locations in array for easier calculation
  my.peaks[ , 1, rst.short[,3] == 1] = peaks[[1]]
  my.peaks[, 1:2, rst.short[,3] == 2] = peaks[[2]]
  my.peaks[, , rst.short[,3] == 4] = peaks[[3]]  #note: 4 is the maximal number of peaks
  my.peaks = aperm(my.peaks, c(3,1,2))
  
  rep.floc = rst.short[,6:7] %o% rep(1,4)
  
  foo = (rep.floc - my.peaks)^2
  distances = sqrt(foo[,1,] + foo[,2,])
  my.distance = apply(distances,1,min,na.rm=TRUE)
  
  avg.distances = matrix(NA,length(j.pick),length(k.pick))
  for (id.k in 1:length(k.pick)){
    k = k.pick[id.k]
    for (id.j in 1:length(j.pick)){
      j = j.pick[id.j]
      avg.distances[id.j,id.k] = mean(my.distance[rst.short[,1]==j & rst.short[,3]==k])
    }
  }
  
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
      move.mean[id.j,id.k] = sum(move[rst.m.short[,1]==j & rst.m.short[,2]==(T-1) & rst.m.short[,3]==k])
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
  
#  win.metafile("Figure3_Competition on Rugged Landscapes_continuous.wmf")
  par(mfrow=c(2,2),las=1, family="serif", oma=c(0,0,0,0))
  
  # >>> FIGURE 3A: Average performance in final period over j and k
  ylim.mean = c(0,35)
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
  legend(1,15, as.expression(j.leg[J.values==j.pick]), lty=(1:length(j.pick)), bty="n", border=0)
  axis(1, k.pick)
  
  # >>> FIGURE 3B: Average distance to nearest peak in final period over j and k
  dist.ylim = c(0,0.5)
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
  moves.ylim = c(0,0.3)
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
  changes.ylim = c(0,0.8)
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
  
#  dev.off()
    
#   
#   # #################################################
#   # ILLUSTRATIVE LANDSCAPES
#   # #################################################
  
  # ------------
  # Set up plot
  cols = c("grey60", "black", "black", "black")
  par(mfrow=c(1,2),las=1, family="serif", oma=c(0,0,0,0))
  
  # ------------
  # Panel 1
  s = 1; j = 1; k = 1; t= T
  
  ind = rst[,1] == j & rst[,3] == k &  rst[,4] == s; ind[is.na(ind)==TRUE]=FALSE
  choice.ind = choices[,1] == j & choices[,3] == k & choices[,4] == s
  customers.ind = customers[,1] == j & customers[,2] == k & customers[,3] == s
  plot.firm = cbind(rst[rst[,2]==t & ind == 1, 6], rst[rst[,2]==t & ind == 1, 7])
  plot.cust = cbind(customers[customers.ind == 1, 4], customers[customers.ind == 1,5] )
  choice = choices[choices[,2]==t & choice.ind == 1,5]
  
  plot(plot.cust, ylim = c(0,1), xlim = c(0,1), pch=(21+choice),cex=1, col=cols[choice+1], main="One firm",
       ylab="Taste Dimension 2", xlab="Taste Dimension 1")
  points(plot.firm,  pch=(21+(1:j)), cex=2, lwd=2, bg=1)
  
  # ------------
  # Panel 2
  s = 1; j = 2; k = 1; t= T
  
  ind = rst[,1] == j & rst[,3] == k &  rst[,4] == s; ind[is.na(ind)==TRUE]=FALSE
  choice.ind = choices[,1] == j & choices[,3] == k & choices[,4] == s
  customers.ind = customers[,1] == j & customers[,2] == k & customers[,3] == s
  plot.firm = cbind(rst[rst[,2]==t & ind == 1, 6], rst[rst[,2]==t & ind == 1, 7])
  plot.cust = cbind(customers[customers.ind == 1, 4], customers[customers.ind == 1,5] )
  choice = choices[choices[,2]==t & choice.ind == 1,5]
  
  plot(plot.cust, ylim = c(0,1), xlim = c(0,1), pch=(21+choice),cex=1, col=cols[choice+1],main = "Two firms",
       ylab="Taste Dimension 2", xlab="Taste Dimension 1")
  points(plot.firm,  pch=(21+(1:j)), cex=2, lwd=2, bg=1)
  
}