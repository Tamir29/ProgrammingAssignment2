## Matrix inversion is usually a costly computation. The two below functionss
## helps us caching the inverse of a matrix rather than compute it repeatedly
## and by that we save computation time.


## This function creates a special "matrix" object that can cache its inverse.
## It creates a list of 4 functions: set, get, setinverse and getinverse. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL 
    set <- function(y) {
        x <<- y 
        m <<- NULL 
    } 
    
    get <- function() x 
    
    setinverse <- function(solve) m <<- solve 
    
    getinverse <- function() m 
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
     m <- x$getinverse() 
    
    if(!is.null(m)) {
        message("getting cached Inverse Matrix") 
        return(m) 
    } 
    
    data <- x$get() 
    m <- solve(data) 
    x$setinverse(m) 
    m 

}
