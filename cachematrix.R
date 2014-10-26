## The purpose of writing these functions is to be able to pull
## a cached value for an inverse matrix instead of calculating
## the inverse each time. Calculating a matrix inverse is
## computationally intensive so we don't want to have to do 
## that every time.

## makeCacheMatrix creates a vector/list that contains functions to set
## and get the matrix inverse. When a matrix is fed to the function
## it will creates this vector that will be used by cacheSolve to find
## the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) m <<- mean
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix that was stored in
## the vector made by makeCacheMatrix. It will only calulate the inverse
## if it has not previously calculated that value. This is accomplished by
## using the 'if' statement to check if the inverse value was previously set
## or if it is NULL.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}