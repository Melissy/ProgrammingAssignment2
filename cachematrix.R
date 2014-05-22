## These functions cache and retrieve  the inverse of a matrix so that it does not have to be 
## computed each time it is needed

## creates matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse =setinverse, getinverse = getinverse) 

}



cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' or retrieve inverse if it has 
    ## already been calculated.
    
    ##solve()
    ##returns inverse of invertible matrix
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    inverse <- x$get()
    
    inv <- solve(inverse, ...)
    x$setinverse(inv)
    inv
   
}
