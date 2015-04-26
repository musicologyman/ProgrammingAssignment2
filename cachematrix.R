## matrix inversion caching
## The pair of functions in this file are used to create and retrieve
## a cached version of a matrix and its inversion.
## 
## Sample usage:
##    
##    m <- matrix(rnorm(16), nrow=4)
##    cm <- makeCacheMatrix(m)
##    inverse <- cacheSolve(cm)


## Given a matrix x, return a list containing the methods
## used to return the matrix and a cached version of its 
## inverse  

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Given a cache matrix (created by the makeCacheMatrix)
## function, return matrix's inverse. If the inverse
## has already been calculated, return the cached version
## of it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
