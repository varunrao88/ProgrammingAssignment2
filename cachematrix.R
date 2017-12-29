## This source file has two functions 
## makeCacheMatrix - this is responsible to create the cache functions and to have getters/setters to get and set 
## the cache and data

## makeCacheMatrix
## set -> sets the matrix for which the inverse needs to be taken
## get -> gets the matrix for which we need the inverse
## setinverse -> sets the inverse for the matrix
## getinverse -> gets the inverse for the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve
## This function takes makeCacheMatrix as input
## The first check we do is to get the inverse from the makeCacheMatrix
## If it exists then we return this itseld
## If not we calculate the inverse using solve() and set the same in makeCacheMatrix so that further on it is available
## Additionally we return the function as well

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}
