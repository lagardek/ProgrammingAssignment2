##  Below are two functions that are used to create a special object that
##  stores a matrix and cache's its inverse.

## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  p <- NULL
  set <- function(y) {
    x <<- y
    p <<- NULL
  }
  get <- function() x
  setinverse <- function(solve)
  p <<- solve
  getinverse <- function() p
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  p <- x$getinverse()
  if (!is.null(p)) {
    message("getting cached data")
    return(p)
  }
  data <- x$get()
  p <- solve(data, ...)
  x$setinverse(p)
  p
}


