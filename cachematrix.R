
## Create a matrix with functions to get and set the data and inverse of the data
## Storing the inverse of the data is used for caching
makeCacheMatrix <- function(x = matrix()) {
    inversed <- NULL
  
    set <- function(y) {
        x <<- y
        inversed <<- NULL
    }
  
    get <- function() x
  
    # Set the inverse (for caching)
    setInverse <- function(inversedMatrix) inversed <<- inversedMatrix
  
    # Get the inverse
    getInverse <- function() inversed
      
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Calculates the matrix inverse using (solve)
## If the inverse has already been calculated, the cached result will be returned
## If not, the inverse will be calculated and the result will be cached and returned
cacheSolve <- function(x, ...) {
    inversed <- x$getInverse()
  
    # Return the cached data if it has already been set
    if(!is.null(inversed)) {
        message("getting cached data")
        return(inversed)
    }
  
    # If it doesn't, get the data and solve it
    data <- x$get()
    inversed <- solve(data, ...)
  
    # Set the cache and then return
    x$setInverse(inversed)
    inversed
}
