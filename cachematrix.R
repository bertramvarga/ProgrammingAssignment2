## Create a subclass for matrix that contains the inverse, S4 style - Yepp, in R, you can apparently subclass built-in types :/ - 

CachedMatrix = setClass("CachedMatrix", prototype = matrix(), slots = c(.Data='matrix', inverse='matrix'))

## Just build the default stuff 
makeCacheMatrix <- function(x = matrix()) {
  CachedMatrix(x)
}

## The definitions for each version.
cacheSolve <- function(x, ...){
  UseMethod("cacheSolve", x)
}

cacheSolve.default <- function(x, ...){
  stop("Argument is Not a matrix")
}

cacheSolve.matrix <- function(x, ...){
  x = makeCacheMatrix(x)
  cacheSolve(x)
}


cacheSolve.CachedMatrix <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if(length(x@inverse)==0){
    x@inverse = solve(x)
  }
  x@inverse
}
