## Functions in this file deals with matrix and inverse of the matrix.
## Caching of inverse is used to avoid recomputing it.

## The following function "makeCacheMatrix" creates a matrix containing following functions:
## 1. set/get value of matrix
## 2. set/get value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates inverse of matrix defined by the function above.
## It first checks if the inverse has already been computed and avoids recomputing
## if not needed. Otherwise, it calculates the inverse of matrix and sets the value
## of inverse using setinv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
