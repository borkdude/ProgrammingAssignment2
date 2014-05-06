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

# Creates a matrix object with cachable inverse
# - x is a matrix argument
# - inv is the variable in which the inverse will be saved
# - set and get are methods that allow the stored matrix to be saved or retrieved
# - setInverse and getInverse are methods that allow the stored inverse of the matrix 
#   to be saved or retrieved
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

# Returns calculates inverse of matrix object, 
# or returns inverse immediately if it has already been calculated
cacheSolve <- function(x, ...) {
  # get the inverse from the cachable matrix
  inv <- x$getInverse()
  # check if it was already calculated, if so, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # else we are going to calculate the inverse, store it in the cachable
  # matrix and return the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
