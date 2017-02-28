# The below set of fucntions are written to calculate the inverse of a matrix, cache the results and return the inverse value from cache during the next iterations, provided there is change in the input matrix. If a different matrix is provided as input, the inverse has to be recalculated and the cache need to be updated

# makeCacheMatrix is for declaring and initializng the cache variables
makeCacheMatrix <- function() {
  if(exists("mCopy") == FALSE) {
    mCopy <<- NULL
  }
  
  if(exists("inv") == FALSE) {
    inv <<- NULL
  }
}


# cacheSolve function is the parent fuction for the inverse calculation
cacheSolve <- function(x) {
  # Initializing vFlag vector to 0
  vFlag <- 0
  # Verifying whether the cache variables are existing
  makeCacheMatrix()
  # Comparing matrix with the values in cache
  vFlag <- setM(x)
  # Function to calculate the inverse
  calcInv(x,vFlag)
}

# Function to identify whether the matrix provided in the previous iteration is the same as the current one
setM <- function(x) {
   
  if(is.null(mCopy)) {
      mCopy <<- x
      return(1)
  }
  else if(identical(mCopy, x) == FALSE) {
      mCopy <<- x
      return(1)
  }
  else {
    return(0)
  }
}

# Function to calculate inverse of a matrix
calcInv <- function(x, flg) {
  if(!is.null(inv) & flg == 0) {
    return(inv)
  }
  else {
    inv <<- solve(x)
    return(inv)
  }
}

