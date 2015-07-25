## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly.
## These are a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseX <- NULL
  set <- function(y) {
    x <<- y
    inverseX <<- NULL
  }
  get <- function() x
  setinverseX <- function(inversevalue) inverseX <<- inversevalue
  getinverseX <- function() inverseX
  list(set = set, get = get,
       setinverseX = setinverseX,
       getinverseX = getinverseX)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseX <- x$getinverseX()
  if(!is.null(inverseX)) {
    message("getting cached data")
    return(inverseX)
  }
  data <- x$get()
  inverseX <- solve(data)
  x$setinverseX(inverseX)
  inverseX
}
