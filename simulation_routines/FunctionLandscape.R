FunctionLandscape = function(binaries, k){
  # DESCRIPTION 
  # Initiate NK landscape

  N = dim(binaries)[2]

  # Generate influence matrix (random allocation)
  influenceMatrix = diag(N);
  for (i in 1:N){
    rands = runif(N); rands[i] = 0;
    influenceMatrix[i,rank(rands,ties.method="random")>N-k]=1;
  }
  #Notes:
  # Checked extensively 120806
  # Alternative methods for influence matrices in "Landscape.m" (V1)
  # Interpretation: 
  #  Rows = "This element is influenced by xx others"
  #  Columns = "This element influences xx other elements"
  
  # Calculate fitness level at each point in the landscape
  multMatrix = influenceMatrix #calculate correct row index for lookup table
  for (i in 1:N){
    mult = 1
    for (j in 1:N){
      if (multMatrix[i,j] == 1){
        multMatrix[i,j] = mult
        mult = mult * 2
      }
    }
  }
  rowindex = binaries %*% t(multMatrix) + 1 #row index (result of multMatrix)
  colindex = t(matrix(rep((0:(N-1))*2^(k+1), 2^N), N, 2^N)) #column index

  table = matrix(runif(2^(k+1)*N),2^(k+1),N)
  payoffMat = matrix(table[rowindex+colindex], 2^N, N)
  customers = rowSums(payoffMat) / N;
  return(customers)
}

