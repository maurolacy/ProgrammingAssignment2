## A couple of functions to compute and store the inverse of a matrix.
## As computing  the inverse of a matrix can be time consuming,
## it is a good idea to cache the result of the computation, to be
## used later if requested again.

## makeCacheMatrix build an object used to store and compute the
## inverse of a matrix (or the solution of the linear system)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    set(x)
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve solves the equation a %*% x = b for x, where b can be either a vector or a matrix.
## If missing, b is taken to be an identity matrix and solve will return the inverse of a.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
