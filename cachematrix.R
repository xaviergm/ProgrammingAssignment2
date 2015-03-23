## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that we will not
## discuss here). Your assignment is to write a pair of functions that cache the
## inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## This function creates a special "matrix" object that can cache its
        ## inverse.
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinv <- function(x) invm <<- solve(x)
        getinv <- function() invm
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from the
## cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        m <- x$get()
        x$setinv(m)
        x$getinv()        
}

## Test Matrix #1: m <- matrix(c(-1, -2, 1, 1), 2,2)
## Test Matrix #2: m=rbind(c(1, -1/4), c(-1/4, 1))
## Test Matrix #3: m <- replicate(20, rnorm(20)) 
