## The functions makeCacheMatrix and cacheSolve work together to solve for the inverse
## of a matrix and store it so that the inverse does not need to be re-calculated later

## makeCacheMatrix is a function that sets up an empty matrix that will receive 
## the inverse of the matrix of interest

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMat <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  getMat <- function() x
  setInv <- function(solve) inv <<- solve #()
  getInv <- function() inv
  list(setMat = setMat,
       getMat = getMat,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve takes an input matrix and checks if the inverse has already be cached,
## if so, it returns the already cached inverse, if not it solves for the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- as.matrix(x$getMat())
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

