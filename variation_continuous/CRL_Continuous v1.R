# COMPETITION ON RUGGED LANDSCAPES - CONTINUOUS VERSION 
# SIMULATION
# v1

# #################################################
# SETUP

# -------------------------------------------------
# SET WD, IMPORT LIBRARIES AND FUNCTIONS

# Clear and set WD
rm(list = ls())
setwd("C:\\Users\\zucchini\\Dropbox\\01_Projects\\CRL\\01_Model\\CRL_Continuous")
#setwd("/Users/leonzucchini/Dropbox/01_Projects/Fads/02_Model")

# Load libraries and source functions 
library(matlab) #for tictoc functionality
library(copula)#bivariate beta (customer distribution in landscape)
source("FunctionConsumerChoice.R")
source("FunctionFirmMove.R")
source("FunctionConsumerMove.R")
source("crl_analysis_continuous.R")

# Set seed and file storage
set.seed(1) #set random seed
savedate = "130709_small"
save.name = paste("SimulationResults_smooth_",savedate,".r",sep="")

# -------------------------------------------------
# DEFINE GLOBAL PARAMETERS

# Landscape and simulation parameters
S = 4 #simulation runs
T = 60 #periods
N = 1000 #consumers
J.values = c(1,2,4) #number of firms in the landscape
K.values = c(1,2,4); K = length(K.values)

# Parameters for beta distributions
bivb1 = 10
bivb2 = 4

# Utility parameters (note: firm seeding with location is done within loops)
Tau = 20
Phi = exp(1) #value of outside option
Alpha = 1+log(Phi) #pure utility of good

# Firm and consumer search parameters
firm.step = 0.2
sq.firm.step = sqrt(firm.step) #max step size for firms' location (Euclidian distance)

# -------------------------------------------------
# INITIATE GLOBAL STORAGE

# Firm Locations and payoffs
rst = matrix(NA,prod(sum(J.values),T,K,S), 8) #results store
colnames(rst) = c("j", "t", "k", "s", "id", "location1", "location2", "payoff")
r.shift = 0 #shifts row indices for storage

# Customer positions and choices
customers = matrix(NA,prod(length(J.values),K,S,N),5) #customer locations for each simulation run
colnames(customers) = c("j", "k", "s", "c.loc1", "c.loc1")
c.shift = 0

choices = matrix(NA,prod(length(J.values),K,S,N,T),5) #customer choices in each period
colnames(choices) = c("j", "k", "t", "s", "choice")
choice.shift = 0
 
# #################################################
# CALCULATION (LOOPS)

# -------------------------------------------------
# CUSTOMER PEAKS
for (ind.k in 1:K){ # k=1; s=1; t=1; j=2
  k = K.values[ind.k]
  
  # -------------------------------------------------
  # FIRM NUMBER LOOP
  for (ind.j in 1:length(J.values)){
    j = J.values[ind.j]
    ind.store = (1:j) + r.shift #initiate competition storage index
    
    print(paste("k =",k,", j =",j))
    tic()
    
    # -------------------------------------------------
    # SIMULATION LOOP
    for (s in 1:S){
      
      # -------------------------------------------------
      # CUSTOMER LANDSCAPE
      
      c.loc = matrix(NA,N,2)
      peaks = k
      if(peaks==1){
        bivBeta = mvdc(normalCopula(0), c("beta", "beta"),paramMargins=list(list(bivb2,bivb2),list(bivb2,bivb2)))
        c.loc = rMvdc(N,bivBeta)
        rm(bivBeta)
      }else{
        if (peaks==2){
          bivBeta1 = mvdc(normalCopula(0), c("beta", "beta"),paramMargins=list(list(bivb2,bivb1),list(bivb2,bivb1)))
          bivBeta2 = mvdc(normalCopula(0), c("beta", "beta"),paramMargins=list(list(bivb1,bivb2),list(bivb1,bivb2)))
          c.loc[1:(N/2),] = rMvdc(N/2,bivBeta1)
          c.loc[(N/2+1):N,] = rMvdc(N/2,bivBeta2)
          rm(bivBeta1, bivBeta2)
        }else{
          bivBeta1 = mvdc(normalCopula(0), c("beta", "beta"),paramMargins=list(list(bivb2,bivb1),list(bivb2,bivb1)))
          bivBeta2 = mvdc(normalCopula(0), c("beta", "beta"),paramMargins=list(list(bivb1,bivb2),list(bivb1,bivb2)))
          bivBeta3 = mvdc(normalCopula(0), c("beta", "beta"),paramMargins=list(list(bivb2,bivb1),list(bivb1,bivb2)))
          bivBeta4 = mvdc(normalCopula(0), c("beta", "beta"),paramMargins=list(list(bivb1,bivb2),list(bivb2,bivb1)))
          c.loc[1:(N/4),] = rMvdc(N/4, bivBeta1)
          c.loc[(N/4+1):(2*N/4),] = rMvdc(N/4, bivBeta2)
          c.loc[(2*N/4+1):(3*N/4),] = rMvdc(N/4, bivBeta3)
          c.loc[(3*N/4+1):N,] = rMvdc(N/4, bivBeta4)
          rm(bivBeta1, bivBeta2, bivBeta3, bivBeta4)
        }
      }
      
      # Store customer locations
      customers[(1:N)+c.shift,1] = j #number of firms
      customers[(1:N)+c.shift,2] = k #ruggedness
      customers[(1:N)+c.shift,3] = s #simulation run
      customers[(1:N)+c.shift,4:5] = c.loc  #store customer locations
      c.shift = c.shift + N
      
      # -------------------------------------------------
      # TIME LOOP
      for (t in 1:T){
        
        # First period: Seed consumer and firm locations
        if (t == 1){
          
          # Seed consumer and firm locations
          f.loc = matrix(runif(2*j), j, 2) #initial firm locations
          
          # Calculate Customer choices and firm payoffs
          choice.env = FunctionConsumerChoice(N, j, c.loc, f.loc, Alpha, Tau, Phi)
          choice = choice.env$choice
          payoff = choice.env$payoff
          
        }else{  # Subsequent periods
          
          # Firms move
          f.move.env = FunctionFirmMove(N, j, c.loc, f.loc, Alpha, Tau, Phi, payoff, firm.step)
          f.loc = f.move.env$loc
          
          # Calculate Customer choices and firm payoffs
          choice.env = FunctionConsumerChoice(N, j, c.loc, f.loc, Alpha, Tau, Phi)
          choice = choice.env$choice
          payoff = choice.env$payoff
        }
        
        # Store firm location and payoffs
        rst[ind.store,1] = rep(j, j) #number of firms
        rst[ind.store,2] = rep(t, j) #period
        rst[ind.store,3] = rep(k, j) #ruggedness
        rst[ind.store,4] = rep(s, j) #simulation run
        rst[ind.store,5] = 1:j #firm id
        rst[ind.store,6] = f.loc[,1] #firm location dim 1
        rst[ind.store,7] = f.loc[,2] #firm loation dim 2
        rst[ind.store,8] = payoff #firm payoff
        ind.store = ind.store + j #update storate index
        if (t==T){r.shift = max(ind.store)-j}
        
        # Store customer locations
        choices[(1:N)+choice.shift,1] = rep(j, N) #number of firms
        choices[(1:N)+choice.shift,2] = rep(t, N) #period
        choices[(1:N)+choice.shift,3] = rep(k, N) #ruggedness
        choices[(1:N)+choice.shift,4] = rep(s, N) #simulation run
        choices[(1:N)+choice.shift,5] = choice #customer choices
        choice.shift = choice.shift + N
      }
    }
    toc()
  }
}

# #################################################
# SAVE RESULTS
rst = rst[is.na(rst[,1])==FALSE,] #the discrete version has NAs, this one I don't think so but it can't hurt
save.image(save.name)
#source("crl_analysis_v14.r")
#analysis()


# #################################################
# # Plot
# 
# s = 4
# k = 1
# j = 4
# 
# ind = rst[,1] == j & rst[,3] == k &  rst[,4] == s; ind[is.na(ind)==TRUE]=FALSE
# choice.ind = choices[,1] == j & choices[,3] == k & choices[,4] == s
# customers.ind = customers[,1] == j & customers[,2] == k & customers[,3] == s
# 
# for (t in 1:T){
#   
#   plot.firm = cbind(rst[rst[,2]==t & ind == 1, 6], rst[rst[,2]==t & ind == 1, 7])
#   plot.cust = cbind(customers[customers.ind == 1, 4], customers[customers.ind == 1,5] )
#   choice = choices[choices[,2]==t & choice.ind == 1,5]
#   
#   #  jpeg(paste("plots\\plot_",t,".jpg",sep=""))
#   plot(plot.cust, ylim = c(0,1), xlim = c(0,1), col=choice+1, pch=20,cex=1, main=paste("Period",t),
#        ylab="Taste Dimension 2", xlab="Taste Dimension 1")
#   points(plot.firm, col=(c(1:j)+1), pch=15, cex=2)
#   #dev.off()
#   Sys.sleep(0.5)
# }
