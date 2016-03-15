## Put comments here that give an overall description of what your
## functions do: functions will calculate the inverse of a matrix only if it has not already been calculated.

## Write a short comment describing this function: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(X = matrix()) {

# holds the cached value or NULL if nothing is cached
# initially nothing is cached so set it to NULL

  m <- NULL

# store a matrix

  set <- function(y) {
    X <<- y
    m <<- NULL
  }

# returns the stored matrix

  get <- function() X

# cache the given argument

  setmatrixinverse <- function(solve) m <<- solve

# get the cached value

  getmatrixinverse <- function() m

# return a list. Each named element of the list is a function

  list(set = set, get = get,
       setmatrixinverse = setmatrixinverse,
       getmatrixinverse = getmatrixinverse)
}


## Write a short comment describing this function:  This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(X, ...) {

# get the cached value

  m <- X$getmatrixinverse()

# if a cached value exists return it

  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

 # otherwise get the matrix, caclulate the inverse and store it in the cache

  data <- X$get()
  m <- solve(data, ...)
  X$setmatrixinverse(m)
 # return the inverse of the matrix
  m
}