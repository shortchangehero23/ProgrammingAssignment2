# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than computing it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # Initialise the inverse variable
    inv <- NULL
    
    # Allow the matrix to be changed
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Return the matrix
    get <- function() x
    
    # Set the inverse matrix
    setInverse <- function(inverse) inv <<- inverse
    
    # Return the cached inverse matrix
    getInverse <- function() inv
    
    # Return the list containing the functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}


## This function returns the inverse of the matrix. It first checks to see if the inverse has already been computed.
## If so, it will fetch the inverse from the cache. Otherwise, the function will compute the inverse and cache it.

cacheSolve <- function(x, ...) {
    
    # Fetch the inverse from the cache
    inv <- x$getInverse()
    
    # Check if the cache is null, and if not, return the cached data
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    # If the cache is null then compute the inverse matrix, set it in the cache, and return it.
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
