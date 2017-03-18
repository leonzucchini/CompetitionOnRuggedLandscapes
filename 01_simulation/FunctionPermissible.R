FunctionPermissible = function(distances, N, location.step){
  # DESCRIPTION
  # Calculates lookup table of permissible positions from distance table 
  # and location step length
  
  cols = sum(distances[1,]<=location.step) #no of permissible moves (colums)
  permissible.moves = matrix(
    which(distances<=location.step, arr.ind=TRUE)[,1], 
    cols, 2^N)
  permissible.moves = t(permissible.moves)

  # NOTE: the additional condition "distances(i,:)>0" 
  # would mean the old position is not permissible => Removed cf. log 120802
  
}