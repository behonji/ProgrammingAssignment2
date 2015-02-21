## FileName: cachematrix.R
##
## Summary:
## Calculating the inversion of a matix is a costly computation.  
## Therefore there is a benefit to cache such a calculation if it 
## is going to be calculated repeatedly.  Below are two functions that
## will cache the inversion of a matrix.  
## 
## The makeCacheMatrix() is used to create a special matrix.  
## The cacheSolve() is used to return the inverse of the matrix
## 
## Example of how to use the functions:
##    a <- 1:4         ## create a vector
##    dim(a) = c(2,2)  ## put the vector into a inversable matrix
##    b <- makeCacheMatrix(a)  ## make the special matrix
##    cacheSolve(b)    ## calculate the inverse
##    cacheSolve(b)    ## the second calculation will use the cache


## This function is used to create a special matrix that will
## be used later by the cacheSolve() function to cache its
## inverse.  This functions assumes that the matrix supplied
## is always invertible.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This functions requies a special matrix that is created using
## the makeCacheMatrix() function.  The function first checkes to see
## if there are any previous cache value stored.  If there is no cached
## value then the function computes the inverse of this special matrix 
## and caches it prior to returning it. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
