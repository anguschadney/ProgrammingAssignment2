
## The following functions are used to compute the inverse of a square, invertible matrix
## and store it in a special vector.  This is so that the cached inverse can be retrieved 
## at a later time, reducing calls to the labour intensive matrix inversion functionality

## The function makeCacheMatrix creates a special vector with the following contents:
## - set(y) : store a square, invertible matrix in the vector
## - get() : retrieve the matrix from the vector
## - setinv(inv) : store the inverse of the matrix in the vector
## - getinv() : retrieve the inverse of the matrix from the vector

makeCacheMatrix <- function(x = matrix()) {
  
    i <- NULL
  
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
  
    get <- function() x
  
    setinv <- function(inv) i <<- inv
  
    getinv <- function() i
  
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The function cacheSolve calculates the inverse of a square matrix by first checking if
## a cached inverse is available in the special vector created by makeCacheMatrix.
## If an inverse is not available, one will be calculated by the solve function and
## then stored in the special vector.

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
