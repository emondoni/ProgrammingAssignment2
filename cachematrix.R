## The two following functions provide a framework for caching the inverse of a
## matrix, so that it must not be recomputed every time it is needed. They are
## based on a data structure that encloses the matrix to be inverted, its
## inverse, and helper functions.

## makeCacheMatrix takes any matrix as an input and returns a list constituting
## the data structure allowing its inverse to be cached.

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
         setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve takes an "enhanced" matrix as an input (in the form output by 
## makeCacheMatrix) and returns its inverse. If the matrix has already been
## inverted before, cacheSolve() simply returns the cached value; otherwise,
## it computes it via solve(), stores the inverse in the data structure passed
## as an input and returns it.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    if (!is.null(inv)) {
        message("Returning cached inverse")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
