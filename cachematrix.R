# makeCacheMatrix: This function creates a special wrapper for a given matrix, 
# that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inversed) inv <<- inversed
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# cacheSolve: This function computes the inverse of the special "matrix-wrapper" object 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of the matrix wrapped in 'x'
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  library(MASS) # for the ginv function who can inverse a matrix even if it is non squared
  m <- ginv(data)
  x$setinv(m)
  m
}



