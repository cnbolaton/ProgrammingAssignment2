## Assignment: Caching the Inverse of a Matrix
## This programming assignment aims to cache potentially time-consuming computations.

## This function will create a special "matrix" that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, 
       get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## This function returns a matrix that would compute the inverse of the input matrix
cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...) # solve is a matrix function in R for computing the inverse
  x$setmatrix(m)
  m
}
