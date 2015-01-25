## Overview: Cache the inverse of a matrix

## This function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y){
    
    x <<- y  ## store original matrix
    
    m <<- NULL ## clear cache that stores matrix inverse 
    
  }

  get <- function() x
  
  setInverse <- function(solve) m <<- solve
  
  getInverse <- function() m
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
      
}



## Compute the inverse of a matrix returned by the makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve function should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  
  if(!is.null(m)){
  
    message("getting cached matrix inverse")
    
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setInverse(m)
  
  m
}


