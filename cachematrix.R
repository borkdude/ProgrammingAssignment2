# Set of functions that enable caching of inverse matrix calculation
# Use makeCacheMatrix to create a cached matrix
# and call cacheSolve on it, to get cached results.
# 
# Example session:
#
# > source('ProgrammingAssignment2/cachematrix.R')
# > m <-makeCacheMatrix(matrix(c(1,.5,0,.5),2,2))
# > cacheSolve(m)
# [,1] [,2]
# [1,]    1    0
# [2,]   -1    2
# > cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,]    1    0
# [2,]   -1    2

## Creates a matrix object with cachable inverse 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Returns calculates inverse of matrix object, 
## or returns inverse immediately if it has already been calculated
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
