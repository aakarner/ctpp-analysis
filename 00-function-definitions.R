#################################################################################
## Alaska Department of Labor and Workforce Development, 2009 
## (Updated April 2011)
## There is not warranty for this code.
#################################################################################

ipf3df <- function(rowcontrol, colcontrol, stackcontrol, 
                   seed = array(1, dim = c(length(rowcontrol),
                                           length(colcontrol),
                                           length(stackcontrol))), 
                   maxiter = 50, closure = 0.0001) {
  # Input data checks: sum of marginal totals equal and no zeros in 
  # marginal totals
  if((sum(rowcontrol) != sum(colcontrol)) || 
     (sum(rowcontrol) != sum(stackcontrol))) {
  warning(paste("Row control=",sum(rowcontrol),
              " Colcontrol=",sum(colcontrol),
              " stackcontrol=",sum(stackcontrol)))
  stop("Marginal control vectors must sum to the same grand total.")}
  
  if(any(rowcontrol == 0)){
    numzero <- sum(rowcontrol == 0)
    rowcontrol[rowcontrol == 0] <- 0.001
    warning(paste(numzero, "zeros in rowcontrol argument replaced with 0.001", 
                  sep=" "))
  }
  
  if(any(colcontrol == 0)){
    numzero <- sum(colcontrol == 0)
    colcontrol[colcontrol == 0] <- 0.001
    warning(paste(numzero, "zeros in colcontrol argument replaced with 0.001", 
                  sep=" "))
  }
  
  if(any(stackcontrol == 0)){
    numzero <- sum(stackcontrol == 0)
    stackcontrol[stackcontrol == 0] <- 0.001
    warning(paste(numzero, "zeros in stackcontrol argument replaced with 0.001", 
                  sep=" "))
  }
  
  if(any(seed == 0)){
    numzero <- sum(seed == 0)
    seed[seed == 0] <- 0.001
    warning(paste(numzero, "zeros in seed argument replaced with 0.001", 
                  sep=" "))
  }
  
   result <- seed
   checksum <- 1
   iter <- 0
   while((checksum > closure) & (iter < maxiter)) {
      # First adjust the first dimension (rows)
      rowtotal <- apply(result,c(3,2),sum)
      rowfactor <- rowcontrol/rowtotal
      result <- sweep(result, c(3,2), rowfactor, "*")
      
      # Then adjust the second dimension (columns)
      coltotal <- apply(result,c(3,1),sum)
      colfactor <- colcontrol/coltotal
      result <- sweep(result, c(3,1), colfactor, "*")
      
      # Finally adjust the third dimension (slices)
      stacktotal <- apply(result, c(2, 1),sum)
      stackfactor <- stackcontrol/stacktotal
      result <- sweep(result, c(2, 1), stackfactor, "*")
      
      # Check whether convergence has been achieved
      checksum <- max(sum(abs(1 - rowfactor)), 
                      sum(abs(1 - colfactor)),
                      sum(abs(1 - stackfactor)))
      iter <- iter + 1
   }
   
   result <- list(fitted.table = result, number.iterations = iter, 
                  tolerance = checksum)
   result
   }
