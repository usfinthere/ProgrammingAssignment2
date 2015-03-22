## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix rather 
## than computing it repeatedly.

## This function creates a list that... 
## 1. Sets the values of a matrix
## 2. Gets the values of a matrix
## 3. Sets an inverse of a matrix
## 4. Gets an inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverseit) inverse <<- inverseit
    getinverse <- function() inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function calculates the inverse of a matrix, but first checks 
## to see if the inverse has already been calculated.  
## If so, it gets the inverse from the cache and skips computation.  
## Otherwise, it calculates the inverse of the data using the 
## inverse function (leveraging "solve.")

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("retrieving cached data...")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
