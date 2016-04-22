## These functions when used together allow the user to create a 'matrix' 
## object that can cache its inverse for future calculations, thus saving 
## computation if the same object is reused.

## This function creates a list with matrix 'x' and functions for getting 
## and storing a cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  
  get <- function() x
  setinver <- function(inverse) inver <<- inverse
  getinver <- function() inver
  
  list(set = set, get = get, setinver = setinver, 
       getinver = getinver)
}


## This function returns the inverse of a list 'x' created by makeCacheMatrix 
## and saves the inverse for future re-use in x.

cacheSolve <- function(x, ...) {
  inver <- x$getinver()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  
  data <- x$get()
  inver <- solve(data, ...)
  x$setinver(inver)
  inver
}
