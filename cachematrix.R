## Put comments here that give an overall description of what your
## functions do

}
# Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse property (starts as NULL)
    inv <- NULL
    
    # Method to set/reset the matrix
    set <- function(y) {
        x <<- y       # Assign new matrix to parent environment
        inv <<- NULL  # Reset inverse cache since matrix changed
    }
    
    # Method to get the current matrix
    get <- function() x
    
    # Method to set the inverse in cache
    setinverse <- function(inverse) inv <<- inverse
    
    # Method to get the cached inverse
    getinverse <- function() inv
    
    # Return list of methods to interact with the matrix object
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# Computes the inverse of the special matrix or retrieves cached version
cacheSolve <- function(x, ...) {
    # Check if inverse is already cached
    inv <- x$getinverse()
    
    # Return cached inverse if available
    if (!is.null(inv)) {
        message("Returning cached matrix inverse")
        return(inv)
    }
    
    # If not cached: 
    # 1. Get the matrix data
    data <- x$get()
    
    # 2. Compute inverse using solve() function
    inv <- solve(data, ...)
    
    # 3. Cache the computed inverse
    x$setinverse(inv)
    
    # Return the newly computed inverse
    inv
}