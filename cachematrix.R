
##creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inv) inv_matrix <<- inv
  getInverseMatrix <- function() inv_matrix
  list(set=set, get=get, setInverseMatrix=setInverseMatrix, getInverseMatrix=getInverseMatrix)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inv_matrix <- x$getInverseMatrix()
  if(!is.null(inv_matrix)) {
    message("getting cached data.")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data)
  x$setInverseMatrix(inv_matrix)
  inv_matrix
}

#Sample usage
#Make huge matrix and use the getter to get the matrix
NCols=1000
NRows=1000 

myMat<-matrix(runif(NCols*NRows), ncol=NCols) 
#myMat 
m = makeCacheMatrix(myMat)
m$get()

# Start the clock!
ptm <- proc.time()

# Inverse was not computed, there expected takes longer time
cacheSolve(m)

proc.time() - ptm

#Takes longer time
#user  system elapsed 
#2.40    0.03    2.54 


# Start the clock!
ptm <- proc.time()

# Inverse was cached, there expected takes less time
cacheSolve(m)

#user  system elapsed 
#0.55    0.00    0.56 
proc.time() - ptm