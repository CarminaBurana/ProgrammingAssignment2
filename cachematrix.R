## File contains a couple of functions that compute and cache 
## the inverse of a square matrix.
##
## 1. makeCacheMatrix - creates a special (CacheMatrix) type
##    that exposes functionality for caching and retrieving 
##    the inverse of a matrix.
##
## 2. cacheSolve - checks for matrix inverse in a CacheMatrix instances
##    and/or calculates the inverse and caches it for later.
## 
## Usual sequence of calls for an initial matrix m:
##
## cacheMatrix <- makeCacheMatrix(m)
## inverse <- cacheSolve(cacheMatrix)

## This function wraps a matrix into a more complex abstraction (CacheMatrix) 
## that provides inverse matrix computing and caching capabilities 
## (getInverse/setInverse methods)

makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL
    
    ## Matrix getter
    get <- function() x
    
    ## Matrix setter
    set <- function(y) {
              x <<- y
              inverse <<- NULL
    }
    
    ## Matrix Inverse getter
    getInverse <- function() inverse
    
    ## Matrix Inverse setter
    setInverse <- function(i) inverse <<- i
    
    ## Return CacheMatrix instance
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

## This function computes the inverse of the specialized "CacheMatrix" 
## returned by the makeCacheMatrix function above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the cached version of the inverse.

cacheSolve <- function(x, ...) {
    
    ## Check for cached version of matrix' inverse and return it if present.
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    
    ## Calculate matrix inverse and cache it
    m <- x$get()
    inverse <- solve(m)
    x$setInverse(inverse)
    
    inverse
}
