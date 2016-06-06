# makeCacheMatrix function
makeCacheMatrix <- function(x = numeric()) {
  
  default <- NULL
  
  setSpecialMatrix <- function(z) {
    x <<- z
    default <<- NULL
  }
  
  getSpecialMatrix <- function() {
    x
  }
  
  setInverse <- function(inv) {
    default <<- inv
  }
  
  getInverse <- function() {
    default
  }
  
  # return a list. Each named element of the list is a function
  list(setSpecialMatrix = setSpecialMatrix, getSpecialMatrix = getSpecailMatrix, setInverse = setInverse, getInverse = getInverse)
}


## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
  default <- x$getinverse()
  if(!is.null(default)) {
    message("getting cached data")
    return(default)
  }
  m <- x$get()
  default <- solve(m, ...)
  x$setinverse(default)
  default
}
