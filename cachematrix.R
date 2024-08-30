## This function will create a list of functions (set & get value of the vector and set & get of the mean)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  
  # Function to set the matrix value
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Function to get the matrix value
  get <- function() x
  
  # Function to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse  
  # Function to get the inverse of the matrix
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the matrix if it hasn't been cached.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # If already cached, how to return it:
  if (!is.null(inv)) {
    return(inv)
  }
  
  # If not cached, computation:
  mat <- x$get()
  inv <- solve(mat, ...)
  
  x$setInverse(inv)
  
  inv
}

