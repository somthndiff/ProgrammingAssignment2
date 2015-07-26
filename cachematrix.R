## These functions in tandem are desgined to create matrices
## which can cache their inverses, and then recall that
## cached data


## makeCacheMatrix creates a matrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set <- function(y = matrix()){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve is a function that calculates  the inverse of 
## makeCacheMatrix, but first checks to see if a backup exists

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
