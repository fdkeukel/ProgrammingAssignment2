## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Creates an object for storing a matrix with caching abilities for inverse matrix
  # supports the following functions:
  # obj$get
  # obj$set
  # obj$getinverse 
  # obj$setinverse
  
  # clear the cache data
  inv <- NULL
  
  # add a new matrix and clear the cache
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # return the stored matrix
  get <- function() x
  
  # write the inverse to cache
  setinverse <- function(inverse) inv <<- inverse
  
  # return the inverse from cache
  getinverse <- function() inv
  
  # list all functions available 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x' 
  # In case a cached version is available, the inverse matrix is returned from cache, 
  # if no cache is available the inverse is calculated and returned
  
  # read the inverse from cache
  inv <- x$getinverse()
  
  # return from cache if available
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # get the original matrix if not in cache
  data <- x$get()
  
  # invert the matrix
  inv <- solve(data, ...)
  
  # write the inverse back
  x$setinverse(inv)
  
  # return the inverted matrix
  inv
}


