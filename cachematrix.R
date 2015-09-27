## Functions for storing and retrieving a matrix and its inverse for
## faster repeated retrieval of the inverse

## Creates list of functions for storing and getting matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Gets inverse from a CacheMatrix.
## If first time getting inverse, it is found and set in the CacheMatrix.
## If inverse was gotten earlier, it is taken from the cache instead of solved.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    message("Solving data")
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
