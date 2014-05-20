## This script contains two functions. The first
## contains a series of sub-functions that define
## and call a matrix and the inverse of this 
## matrix. The second function computes the inverse
## of the given matrix, or calls forth a previously 
## cached value of the inverse matrix if it has already
## been computed.    

## The function makeCacheMatrix defines a list of 
## sub-function and arranges them in a list to be 
## called up in the calculation of the inverse of a 
## matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ##Initilze Matrix as empty
  s <- NULL
  
  ##Define function "set", sets value of the matrix
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  ##Define function "get", calls the value of the matrix
  get <- function() x
  
  ##Defines funciton "setsolve", sets the value of the inverse matrix
  setsolve <- function(solve) s <<- solve
  
  ##Define function "getsolve", calls the value of the inverse matrix
  getsolve <- function() s
  
  ##Defines list of functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
	   
}

## This function cacheSolve calculates the inverse of a
## given matrix and caches the result. If already calculated
## this function returns the cached value for the inverse
## matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
		
  ##Define inverse matrix
  s <- x$getsolve()
  
  ##Call up cached inverse matrix if already computed
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  ##Define matrix data
  data <- x$get()
  
  ##Calculate the inverse matrix
  s <- solve(data, ...)
  x$setsolve(s) ##Defines inverse matrix
  
  ##Display inverse matrix
  s
}
