## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Returns a list of functions to interact with the cached matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solved) inv <<- solved
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
# Solves the inverse of the matrix, returning the cached value if available
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()

  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}

# to test
mat <- matrix(1:4, 2, 2)
mat
fns <- makeCacheMatrix(mat)
cacheSolve(fns)
