## This is a solution to the programming assignment 2, R programming course at Coursera rprog009

## These two functions allow cashing an inverse of a matrix to avoid repetitive computing.
## Here is how the functions should be called to check this work: cacheSolve(makeCacheMatrix(x)) 
## X must be a invertible matrix

## This first function create a list of 4 functions to set, get, set the inverse and get the inverse of a matrix.
## The list will be then passed to the second function.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  ## here is the list with the definition of each function
  list(set = set <- function(y) {
    x <<- y
    s <<- NULL
    ## note that with the use of <<-, these variables will be visible from outside the set function
  }, get = get <- function() x,
  setsolve = setsolve <- function(solve) s <<- solve,
  getsolve = getsolve <- function() s) 
  ## the notation function() s means that, as per the scoping properties of R, the value of s will be looked for at 
  ## the environment the function was caled for
}


## This function takes a list of functions created by MakeCacheMatrix and returns the value of the inverse of the
## inversible matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  ## if the inverse was calculated, the functions gets it from the cash rather than recomputing it.
  if(!is.null(s)) {
    gmessage("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
