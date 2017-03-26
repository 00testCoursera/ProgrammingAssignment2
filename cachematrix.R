## Hi there, this is my solution to the assignment 2, lexical scoping. I hope it
## makes sense and you enjoy rating it.

## makeCacheMatrix has the following functionalities: 1) sets the value of the matrix; 2)
## gets the value of the matrix; 3) sets the value of the inverse and 4) gets the value
## of the inverse

makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(y) {
    x <<- y
    k <<- NULL
  }
  get <- function() x
  setinverse <- function(solveMatrix) k <<- solveMatrix
  getinverse <- function() k
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function that creates the inverse of the matrix created by
## makeCacheMatrix. However first it checks if the inverse is already calculated, if this
## is the case the cacheSolve should retrieve the inverse, otherwise it is created. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  k <- x$getinverse()
  if(!is.null(k)) {
    message("getting cached data")
    return(k)
  }
  data <- x$get()
  k <- solve(data, ...)
  x$setinverse(k)
  k
}
