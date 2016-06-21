
#' Creates a matrix-like list that can cache it's inverse.
#' @param x Input matrix. Default to matrix's default
#' @return A list with the following functions: set, get, setinv, getinv
#'
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  m <- x
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#' Calculates the inverse of a cacheMatrix or returns the previous calculation
#' @param x cacheMatrix instance
#' @return inverse of x
cacheInverse <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  print(data)
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}
