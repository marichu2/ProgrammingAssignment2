## makeCacheMatrix accepts a matrix as its input.  cacheSolve function returns an inverted
## matrix that was returned by makeCacheMatrix

## makeCacheMatrix is a function that stores a matrix and its inverse.  It provides setter
## and getter functions for both matrices
##

makeCacheMatrix <- function(x = matrix()) {
    mx <- NULL
    setmx <- function(y = matrix()) {           
        x <<- y                            ## x is original matrix
        mx <<- NULL                        ## mx is inverted matrix
    }
    getmx <- function() x
    setInverse <- function(solve) mx <<- solve   ## set input solve to inverted matrix, mx
    getInverse <- function() mx
    list(setmx = setmx, getmx = getmx,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve accepts a list output from makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' entered previously
    mx <- x$getInverse()
    if(!is.null(mx)) {
        message("getting cached inverse matrix")
        return(mx)
    }
    
    mdata <- x$getmx()
    mx <- solve(mdata)
    x$setInverse(mx)
    mx
}
