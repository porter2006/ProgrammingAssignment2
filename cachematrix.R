#This set of functions uses a cache to store previously inverted matrices to speed up computation time.

#makeCacheMatrix creates a list of functions that set the matrix and its inverse and gets the matrix
#and its inverse from the cache
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  seti <- function(i) inv <<- i
  geti <- function() inv
  list(set=set, get=get, seti=seti, geti=geti)
}


# Contains cache of previously inverted matrices. The cache is checked when a matrix is passed to the function.
# If the matrix is in the cache the function returns the previously attained value. If not the function it computes
# the inverse and caches it with setnew function
cacheSolve <- function(x, ...) {
  inv <- x$geti()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$seti(inv)
  inv
}
