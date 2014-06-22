## These functions allow us to cache the result of the time-consuming operation 
## of matrix inversion. It stores the matrix inverse and returns it if computed 
## and computes it, caches it, and returns it if not computed. Essentially, we
## return a list of functions with which we can set the matrix, get the matrix,
## set the matrix's inverse, and get the matrix's inverse. These variables are 
## kept in the scope of makeCacheMatrix rather than in one of the environemnts 
## of the 4 particular functions.

## This sets up the "special" list of functions that can perform the matrix
## operations. The first sets the matrix and sets the inverse to NULL in the 
## original environment; the second returns the matrix; the third sets the
## inverse in the original environment; the fourth returns that inverse. These
## four functions are returned so the user may use the matrix operations.

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


## If the matrix inverse has already been computed(i.e. a non-NULL value), 
## simply return it, and if not, compute, store, and return it.

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