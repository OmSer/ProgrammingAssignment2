#The first function, makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse 
# 4. get the value of inverse 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been calculated. If so, it gets the result from the cache and skips the
# calculation. If not, it calculates the inverse, sets the value in the cache via
# setinv function.


cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  # if the inverse has already been calculated
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data)
  x$setinv(inverse)
  inverse
}
