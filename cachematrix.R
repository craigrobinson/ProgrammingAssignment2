## This file contains 2 functions.
## The first, makeCacheMatrix, exposes methods that will allow the second function to
##    properly store values in memory for later use (e.g. cache)
## The second function, cacheSolve, will take a matrix that has been "wrapped" with the first function
##    this allows the matrix to be solved and placed in the cache or pulled from the cache.

## This function creates a special "matrix" object that can cache its inverse.
## Function should define the methods that will be available. These are the getters and setters
## It will also add a list of available functions making them available on the resulting object.
## Result should remain a simple function with exposed methods for cacheing.
makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
 set <- function(y) {
   x <<- y
   m <<- NULL
 }
 get <- function() x
 setinverse <- function(solve) m <<- solve
 getinverse <- function() m
 list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned
##  by makeCacheMatrix above. If the inverse has already been calculated
##  (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## Once a matrix has been defined withe makeCacheMatrix called, this function will accept the matrix
##  and will lookup a previous value if available or solve with the current matrix and put in the cache
## Printing a message since it will more easily expose when the cacheing is working.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)){
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
