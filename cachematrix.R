## Put comments here that give an overall description of what your
## functions do

## This function provides an object for defining a cached matrix
## The members define creation, assignment, and computation of the
##     inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {

   ## Initialize
   m <- NULL

   ## Set matrix and cache
   set <- function(y) {
      x <<- y
      m <<- NULL
   }

   ## Get the matrix
   get <- function() x

   ## Set inverse 
   setinverse <- function(inv) m <<- inv

   ## Get inverse
   getinverse <- function() m

   ## return contents
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getinverse()
   if (!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)
   m
}
