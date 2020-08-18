## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix contains 4 functions: set, get, setinv, getinv

## The first 'm <-NULL' initializes m as an object in makeCacheMatrix

## The second 'm<<- NULL' (with 2 '<<') assigns the value of NULL to 
## the m object in the parent environment 
## This line of code clears any value of m that had been cached by a prior 
## execution of cachematrix()

## If there is already a valid inverse cached in m, whenever x is reset, 
## the value of m cached in the memory of the object is cleared, 
## forcing subsequent calls to cachematrix() to recalculate the inverse 
## rather than retrieving the wrong value from cache.

## The last section of code assigns each of these functions 
## as an element within a list(), and returns it to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function checks if the inverse matrix has been calculated before
## If the inverse has been calculated, then it gets the cached data 
## Otherwise, 
## I use the ginv function which is available through the MASS library

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  library(MASS)
  m <- ginv(data, ...)
  x$setinv(m)
  m
  
}
