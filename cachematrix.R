# We build two functions, makeCacheMatrix, that creates a matrix capable of 
# caching its inverse and cacheSolve which actually computes the inverse if
# it is not already available.
# usage :
# cachedX <- makeCacheMatrix(x)
# invx <- cacheSolve(cachedX)

# makeCacheMatrix is a function that provides way to get and set the matrix and 
# its inverse.
makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL # setting the value of the inverse to NULL.
  
  set <- function(y) {
    x <<- y
    invX <<- NULL   # if matrix is changed via set function, inv is reset to NULL
  }

  get <- function() x
  
  setinverse <- function(inv) invX <<- inv
  
  getinverse <- function() invX
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# cacheSolve is a function which checks if the inverse is not already available, 
# and computes the inverse if required.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("returning the cached value of inverse of matrix")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinverse(inv)
  return(inv)
}

# General Rant
#################################
# Not quite sure of why the assignment asks for two separate functions as 
# ideally you would want to wrap the inverse computation in the makeCachematrix 
# function itself. So instead of having two functions, the getinverse would 
# call cacheSolve directly, or more effectively have that code in its own body
# as cacheSolve is not directly reusable otherwise:
# getinverse <- function() {
#   if(!is.null(invX)) {return invX}
#   invX <- solve(x)
#   return invX
# }
# This would allow for a much more intuitive flow:
# cachedX <- makeCacheMatrix(x)
# invX <- cachedX$getinverse()
