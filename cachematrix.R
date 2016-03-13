## Put comments here that give an overall description of what your
## functions do: functions will calculate the inverse of a matrix only if it has not already been calculated.

## Write a short comment describing this function: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(X = matrix()) {
  m <- NULL
  set <- function(y) {
    X <<- y
    m <<- NULL
  }
  get <- function() X
  setmatrixinverse <- function(solve) m <<- solve
  getmatrixinverse <- function() m
  list(set = set, get = get,
       setmatrixinverse = setmatrixinverse,
       getmatrixinverse = getmatrixinverse)
}


## Write a short comment describing this function:  This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(X, ...) {
  m <- X$getmatrixinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- X$get()
  m <- solve(data, ...)
  X$setmatrixinverse(m)
  m
}