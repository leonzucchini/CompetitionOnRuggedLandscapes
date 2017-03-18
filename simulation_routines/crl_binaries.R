# GENERATE BINARY LOCATIONS, CALCULATE DISTANCES

rm(list = ls())
set.seed(0)
#setwd("C:\\Users\\zucchini\\Dropbox\\01_Projects\\CRL\\01_Model")
setwd("/Users/leonzucchini/Dropbox/01_Projects/CRL/01_Model")

N_bin = 10

source("FunctionBinaries.r")
binaries = FunctionBinaries(0:((2^N_bin)-1))
tbinaries = t(binaries)

distances = matrix(NA,2^N_bin,2^N_bin)

for (i in 1:(2^N_bin)){ #THIS IS THE BAD ONE
  distances[i,] = colSums(xor(binaries[i,],tbinaries))
}

rm(list= ls()[!(ls() %in% c('distances','binaries','N_bin'))]) 

save.image("DataBinaries.r")