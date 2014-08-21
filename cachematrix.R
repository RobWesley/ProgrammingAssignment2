## Two Functions makeCacheMatrix and cacheSolve to implement a rudimentary
## cacheable matrix and solving for the inverse of the matrix or retrieving
## the cached inverse if it has already been calculated

## makeCacheMatrix is a function (class) that provides a cacheable matrix and inverse matrix:
## set matrix
## get cached matrix
## set inverse of matrix
## get cached inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve takes a cacheable matrix, determines if it already has an inverse matrix cached
## if so, it returns the cached inverse
## else it calculates the inverse and caches it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
