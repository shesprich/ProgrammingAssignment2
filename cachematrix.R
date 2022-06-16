## Put comments here that give an overall description of what your
## functions do

## These functions are designed to save time calculating the inverse of a
## matrix by creating a special matrix object which can store matrix data as 
## well as its own inverse.

## Write a short comment describing this function:
## makeCacheMatrix creates a special matrix object which can store the matrix 
## data itself as well as its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

## This function takes a special matrix object created by makeCacheMatrix, then
## if the inverse has already been calculated it returns the cached value, if
## not it calculated the inverse using the solve function, caches the inverse in
## the special matrix object, then returns the computed inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
