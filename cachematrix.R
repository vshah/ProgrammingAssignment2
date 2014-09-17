## Put comments here that give an overall description of what your
## functions do

## Function to make Cache Matrix wrapper
makeCacheMatrix <- function(x = matrix()) {
    # Initialize inverse with null
    inverse <- NULL
    
    # In case we need to set new matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    # Get the matrix
    get <- function() x
    
    #Store the inverse
    setinverse <- function(computed) inverse <<- computed
    
    #Get the inverse
    getinverse <- function() inverse
    
    #List of exposed function
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        # Check if inverse is already computed
        if(!is.null(inverse)) {
            message("getting cached data")
            # Return already computed inverse
            return(inverse)
        }
        # Inverse is not computed so compute it now
        data <- x$get()
        computedInverse <- solve(data, ...)
        # Set the inverse in cache for future
        x$setinverse(computedInverse)
        computedInverse
}