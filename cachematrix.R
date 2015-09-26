## We want to cache the inverse of a matrix because the cost of
## computing the inverse is very high

## This is a cached matrix, that is, its inverse is cached

makeCacheMatrix <- function(x = matrix()) {
    # When constructing the object, the inverse is not valid
    inv <- NULL
    
    # They want to work with this matrix ...
    setMatrix <- function(y) {
        x <<- y
        # ... the inverse is not valid
        inv <- NULL
    }
    
    # Ok, they need it back
    getMatrix <- function() x
    
    # They need to store the inverse ...
    setInv <- function(inverse) inv <<- inverse
    
    # But also to get it back
    getInv <- function() inv
    
    # The list of methods
    list(set = setMatrix,
         get = getMatrix,
         setinverse = setInv,
         getinverse = getInv)
}


## Compute the inverse using the cached version if possible

cacheSolve <- function(x, ...) {
    # If we are lucky, we got it
    i <- x$getinverse()
    if (is.null(i)) {
        # No, we must do the computation
        message("calcolo")
        
        # Grab the matrix
        m <- x$get()
        
        # Compute the inverse
        i <- solve(m)
        
        # Cache the value for the next time
        x$setinverse(i)
    }
    else {
        message('Uso la cache')
    }
    
    # return the inverse
    i
}
