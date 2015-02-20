## Create an object that stores a square matrix and caches its inverse.

## Create a matrix containing a function to set and get the value of the matrix, 
## and to set and get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
     z <- NULL
     set <- function(y) {
          x <<- y
          z <<- NULL
     }
     get <- function() x
     setz <- function(solve) z <<- solve
     getz <- function() z
     list(set = set, 
          get = get,
          setz = setz,
          getz = getz)
}

## Get the inverse of the matrix from the cache if the inverse has already been calculated. 
## Otherwise, calculate the inverse of the matrix, and set the inverse's value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
     z <- x$getz()
     if(!is.null(z)) {
          message("getting cached data")
          return(z) ## Return a matrix that is the inverse of 'x'
     } else {
          data <- x$get()
          z <- solve(data, ...)
          x$setz(z)
          message("getting cached data")
          return(z) ## Return a matrix that is the inverse of 'x'
     }
}