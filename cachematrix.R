## this function takes a square invertible matrix and returns a CacheMatrix 
## used by cacheSolve to manage calculating and caching the matrix's inverse. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}

## this function takes a CacheMatrix and returns its inverse which is only
## calculated once for a particular CacheMatrix

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
