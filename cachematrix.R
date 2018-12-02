## This provides a less time-consuming way to solve the inverse of a matrix
## The following functions will cache the inverse of the matrix
## Matrices supplied here are assumed to be invertible

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inv <- function(inverse) inv <<- inverse
  get_inv <- function() inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set_inv(inv)
  inv
}

## TESTING THE PROGRAM:
## (matrix example source: http://www.mathcentre.ac.uk/resources/uploaded/sigma-matrices7-2009-1.pdf)
## am <- matrix(c(4, 3, 1, 1), 2, 2, byrow = TRUE)
## am
##      [,1] [,2]
## [1,]    4    3
## [2,]    1    1
## am1 <- makeCacheMatrix(am)
## cacheSolve(am1)
##      [,1] [,2]
## [1,]    1   -3
## [2,]   -1    4

