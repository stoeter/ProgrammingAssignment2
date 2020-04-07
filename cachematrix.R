## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## 'x' is a square invertible matrix that should be invertable
## Returns a list of functions: set, get, setinverse, getinverse (of a matrix) 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## Write a short comment describing this function
## This function calculates the inverse of the special "matrix" created with
## the makeCacheMatrix function. 
## 'x' list of functions from makeCacheMatrix
## Function checks if the inverse has been calculated, and gets the inverse 
## matrix from the cache. Otherwise, the inverse matrix will be calculates and 
## stored in the cache.
## Returns a matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}



