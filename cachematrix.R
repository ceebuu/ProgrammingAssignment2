## This is an R function for caching (potentially) time-consuming computations;
## in this case: inverting a matrix.
## It contains two functions that cache the inverse of a matrix:
## 1: makeCachematrix (to create one), 2: cacheSolve (to compute the inverse
## or retrieve it from the cache)

## The first function builds the matrix; it contains a list of 4 functions:
## set, get, setinv, getinv.
## To invert the matrix (setinv), the "solve" function is used (1).

## We assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
               x <<- y
               m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve		## (1)
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function calculates the inverse of the matrix if the cache is empty, otherwise it retrieves the cached matrix (2).

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {							## (2)
                message("getting cached data")		## (2)
                return(m)							## (2)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
