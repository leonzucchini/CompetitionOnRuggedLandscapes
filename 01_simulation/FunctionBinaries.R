FunctionBinaries = function (x, b=2){ 
  # DESCRIPTION
  # Converts vector of integers into binaries (matrix)
  # Written by Spencer Graves (with slight modifications)
  # Retrieved from on 120813 from:
  # https://stat.ethz.ch/pipermail/r-help/2003-September/038978.html
    N = length(x)
    xMax = max(x)	
    ndigits = (floor(logb(xMax, base=2))+1)
    Base.b = array(NA, dim=c(N, ndigits))
    for(i in 1:ndigits){#i <- 1
      Base.b[, ndigits-i+1] <- (x %% b)
      x <- (x %/% b)
    }
    if(N ==1) Base.b[1, ] else Base.b
  }

