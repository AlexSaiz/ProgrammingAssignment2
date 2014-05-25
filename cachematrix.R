## These functions are used to cache the inverse of a matrix

## 1. This function creates a special "matrix" object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  # 'inv' will store de cached inverse matrix
  inv <- NULL
  
  # Function to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Function to get the value of the matrix
  get <- function() x
  
  # Function to set the value of the inverse
  setinv <- function(inverse) inv <<- inverse
  
  # Function to get the value of the inverse
  getinv <- function() inv
  
  # Return the matrix with these new functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## 2. This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. 
## If the inverse is already calculated before, it returns the cached 
## inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # If the inverse is already calculated, we return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If the inverse is not calculated, we calculate it
  data <- x$get()
  inv <- solve(data, ...)
  
  # We cache the inverse
  x$setinv(inv)
  
  # Finally we return it
  inv
}
