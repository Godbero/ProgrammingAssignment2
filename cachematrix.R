## makeCacheMatrix and cacheSolve work together to efficiently invert a square matrix
## by returning the inverse matrix from cache if it was previously calculated


## This function takes a square matrix and returns a list of set, get setinv, getinv variables
## that allow the cacheSolve function to find the inverse of the matrix or get it from cache.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# This function uses the list returned by makeCacheMatrix to find the inverse of
# a matrix on get it from cache, if it was already calculated.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

