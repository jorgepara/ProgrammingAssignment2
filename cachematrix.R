## makeCacheMatrix() and cacheSolve() functions works together to provide
## caching of the inverse of a matrix. Call makeCacheMatrix() with your source
## matrix as argument and use returned object to call cacheSolve() as many times
## as you need. 

## This function creates an object from a matrix, and returns alternatively
## inverted matrix and original matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        # Just to know if we should return inverted matrix or original matrix
        returnInverted <- TRUE
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverted <- function(inverted) i <<- inverted
        getinverted <- function() {
                if (returnInverted) {
                        # Use <<- operator here! Following line is duplicated
                        # with else block due to legibility.
                        returnInverted <<- !returnInverted
                        i
                } 
                else {
                        # Use <<- operator here! Following line is duplicated
                        # with if block due to legibility.
                        returnInverted <<- !returnInverted
                        x
                }
        }
        list(set = set, get = get,
             setinverted = setinverted,
             getinverted = getinverted)
}

## This function receives an object created with makeCacheMatrix() and 
## returns alternatively inverted matrix and original matrix.
## It takes care of avoiding recomputation of the inverse of a matrix if it is 
## not needed. 

cacheSolve <- function(x, ...) {
        i <- x$getinverted()
        if(!is.null(i)) {
                return(i)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverted(m)
        m
}
