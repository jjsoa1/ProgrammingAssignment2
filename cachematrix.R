## The following functions create special "matrix" objects that can calculate 
## and cache its inverse.

## Creates a special matrix object that is able to cache its inverse. Also
## contains functions to reassign and access the matrix itself.

makeCacheMatrix <- function(matrixValue = matrix()) {
  inverse <- NULL
  set <- function(newMatrixValue) {
    matrixValue <<- newMatrixValue
    inverse <<- NULL
  }
  get <- function() matrixValue
  setInverse <- function(setInverse) inverse <<- setInverse
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of a matrix created by makeCacheMatrix(). If the inverse
## is already calculated, then return the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
