## Put comments here that give an overall description of what your
## functions do

## In the below function, I have initialized the value of inverse matrix as null
## It consists of functions like set, get, set_inverse, get_inverse to get matrix x and obtain inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  set_inverse <- function(inverse) {inv <<- inverse}
  get_inverse <- function() {inv}
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Below function gets the cached data
## !is.null(inv) checks that inverse is not null and returns inverse
## solve calculates inverse value
## returns a matrix that is inverse of x

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("Cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inverse(inv)
  inv
}