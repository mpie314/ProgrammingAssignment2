## To create an equivalent of pretty much instantenous computations of matrix
## inverse, cache the results of the first invocation, so that subsequent
## invocations of the function do not have to repeat the computation.
################################################################################


## Define a list, which is a kind of partially initialized singleton object
## containing data and functions setting and getting those data.
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    X <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Define a function to perform the desired computation (matrix inversion)
## in the first invocation, but reading the cached result in subsequent
## invocations.
##
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if ( !is.null(m) ) {
    message("Getting cached result")
  }
  else {
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
  }
  ## Return a matrix that is the inverse of 'x'
  m
}
