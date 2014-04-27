## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The 'makeCacheMatrix' function creates a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse matrix 
## 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Set function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Get function
  get <- function() x
  ## setInverse function
  setInverse <- function(invMatrix) {
    m <<- invMatrix
  }
  ## getInverse function
  getInverse <- function() m
  ## list 
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## The 'cacheSolve' function uses the 'makeCacheMatrix' functions to
## calculate the inverse of a matrix.  It checks if the inverse of the
## matrix is already calculated. If so, it gets the inverse from the 
## cache.  Otherwise, it calculates the inverse of the matrix and caches
## the calculated inverse matrix using setInverse function.
## Assumption: The input matrix is assumed to be inversible.  No
## checks are performed in the code or no error messages are returned 
## from the function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
      message("Getting cached inverse matrix data")
      return (m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setInverse(m)
    return (m)
}
