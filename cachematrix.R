## Set of functions that implement caching of inverse matrix calculation
## Use makeCacheMatrix to create a cached matrix
## and call cacheSolve on it, to get cached results.
## Example: 
## m <-makeCacheMatrix(matrix(c(1,.5,0,.5),2,2))
## cacheSolve(m) ## will compute the inverse, cache it and return it
## cacheSolve(m) ## will return the cached result immediately

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


## Calculates inverse of matrix object, 
## or returns inverse if it has already been calculated
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
