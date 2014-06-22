## These functions allow us to cache the result of the time-consuming operation 
## of matrix inversion. It stores the matrix inverse and returns it if computed 
## and computes it, caches it, and returns it if not computed.

## This sets up the "special" matrix that can store the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setmatrix <- function(newmatrix) {
        x <<- newmatrix
        m <<- NULL
    }
    getmatrix <- function() {
        x
    }
    setinverse <- function(matrixinverse) {
        inverse <<- matrixinverse
    }
    getinverse <- function() {
        inverse
    }
    list(setmatrix = setmatrix, getmatrix = getmatrix, 
        setinverse = setinverse, getinverse = getinverse)
}


## If the matrix inverse has already been computed, simply return it, and if
## not, compute, store, and return it.

cacheSolve <- function(x, ...) {
    matrixinverse <- x$getinverse()
    if(!is.null(matrixinverse)) {
        message("Getting cached inverse!")
        return(matrixinverse)
    }
    matrix <- x$getmatrix()
    matrixinverse <- solve(matrix, ...)
    x$setinverse(matrixinverse)
    matrixinverse
}
