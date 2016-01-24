## Put comments here that give an overall description of what your
## functions do

## Funtion to get and set an invertible matrix and store inverse matrix once it is set.
## inverse matrix is reset (set to null) if matrix is changed (set)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calculates the inverse of the passed invertible matrix.  
## If inverse matrix is already stored it simply returns the cached inverse matrix
## If inverse matrix is not stored it calculates the it and stores it (using $setinv)

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
