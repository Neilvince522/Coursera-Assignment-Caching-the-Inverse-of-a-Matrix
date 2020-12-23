## the functions makeCacheMatrix and cacheSolve conveniently computes 
## matrix inversion

## makeCacheMatrix is a function that produces a matrix in which its inverse
## can be cache

makeCacheMatrix <- function(a = matrix()) {
  c <- NULL
  set <- function(b) {
    a <<- b
    c <<- NULL
  }
  get <- function() a
  set_inverse <- function(inverse) 
  c <<- inverse
  get_inverse <- function() c
  list(set = set,get = get, set_inverse = set_inverse, 
       get_inverse = get_inverse)
}


## cacheSolve is a function that returns the inverse of the matrix produced
## by the makeCacheMatrix function

cacheSolve <- function(a, ...) {
  ## Return a matrix that is the inverse of 'a'
  c <- a$get_inverse()
  if (!is.null(c)) {
    message("obtaining the cached matrix")
    c
  }
  data <- a$get()
  c <- solve(data, ...)
  a$set_inverse(c)
  c
}
