## These functions are for the second programming assignment for the
## R Programming course.  The first function makes a cached matrix
## and the second computes and sets the inverse of the matrix
## using the built in function, solve.

## used the examples provided for the vector and mean
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <-function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function takes the cached matrix created
## above and makes and sets a cached value for the
## inverse of that matrix

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
