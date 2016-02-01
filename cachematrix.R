## There will be a pair of functions that cache the inverse of a matrix.
## In particular, it allows use to calculate in inverse of a matrix and
## store it in the cache, so that in the case of multiple uses, 
## the computation will be faster and take less space on the RAM.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) { # if the matrix has already been calculated, then return the cache
      message("getting cached data")
      return(m)
    } # otherwise, calculate the inverse of the matrix using 'solve' function
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
