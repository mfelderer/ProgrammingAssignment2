# Example usage:
# > x <- matrix(c(4,3,3,2),nrow=2,ncol=2)     // Create a matrix x
# > cx <- makeCacheMatrix(x)                  // Create our special matrix
# > cx$get()                                  // Return the matrix
# > cacheSolve(cx)                            // Return the inverse
# > cacheSolve(cx)                            // Call 2nd time, returns cached inverse

# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # inv stores the cached inverse matrix
  inv <- NULL
  
  # setter for the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # getter for the matrix
  get <- function() x
  
  # setter for the inverse
  setinv <- function(inverse) inv <<- inverse
  
  # getter for the inverse
  getinv <- function() inv
  
  # return list with newly defined functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse
cacheSolve <- function(x, ...) {
  
  # get chaced inverse
  inv <- x$getinv()
  
  # if inverse is already calculated, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # calculate the inverse, if not yet calculated
  data <- x$get()
  inv <- solve(data, ...)
  
  # cache the inverse
  x$setinv(inv)
  
  # return inverse
  inv
}
