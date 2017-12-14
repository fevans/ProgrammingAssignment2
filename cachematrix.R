## The following functions demonstrates the use of scoping rules to implement
## caching in R.

## This function wraps an input matrix object and its computed inverse 
## both of which are assessible via custom methods/functions. It is 
## consumed by the cacheSolve function below to return the computed inverse fo the 
## input matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setMat <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        getMat <- function() x
        setInverse <- function(matInv) inv <<- matInv
        getInverse <- function() inv
        list(setMat = setMat, getMat = getMat,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function accepts a makeCacheMatrix function and computes the inverse
## of its input matrix on first call and caches its value in the makeCacheMatrix inv field.
## Subsequent calls to this function with the same makeCacheMatrix instance
## simply reads and returns the previously cached value. For this exercise, the input matrix is always assumed 
## to be invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <-  x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$getMat()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
