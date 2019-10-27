## Compute the inverse of a matrix

## The first function, makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(q) {
    m <<- q
    inv <<- NULL
  }
  get <- function() m
  setInverse <- function(n) inv <<- n
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The following function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(m, ...) {
  inv <- m$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data, ...)
  m$setInverse(inv)
  inv
}
