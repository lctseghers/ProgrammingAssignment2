## These functions will first create a 'matrix' object that caches the inverse of a
## given matrix, and then computes the inverse of a given matrix or returns a previously 
## cached inverse of that matrix if one exists.
## Note: Assumes all given matrices are invertible.

## The makeCacheMatrix creates an inverse of a matrix and caches it
## Note: assign results of makeCacheMatrix to a variable

makeCacheMatrix <- function(x = matrix()) {
    mx <- NULL
    set <- function(y) {
        x <<- y
        mx <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) mx <<- solve(x)
    getinverse <- function() mx
    list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function first determines if a cached inverse exists for a given
## matrix and if so, returns that, otherwise creates and returns the inverse.
## Note: use the variable with makeCacheMatrix result as argument to cacheSolve
## Note: to see cached inverse message, call cacheSolve twice for the same matrix

cacheSolve <- function(x, ...) {
    mx <- x$getinverse()
    if (!all(is.na(mx))) {
      message ("getting cached inverse")
      return(mx)
    }
    data <- x$get()
    mx <- solve(data, ...)
    x$setinverse(mx)
    mx
}
