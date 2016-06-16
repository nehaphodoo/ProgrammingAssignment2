## Below is the pair of functions that cache the inverse of a matrix : 
## makeCacheMatrix and cacheSolve

## Below function creates a special "vector" which is really a list
## containing 4 functions to set and get the matrix and inverse of 
## matrix correspondingly

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Below function returns the inverted matrix of the special matrix 
## created using the set function from above. It first checks the 
## cache if the matrix inversion has been done before. If so, it gets 
## the result from the cache and skips computation. Otherwise, it 
## inverts the matrix again and sets this in the cache via setInverse 
## function

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<- solve(data,...)
  x$setInverse(m)
  m
 }
