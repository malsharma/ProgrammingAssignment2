## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## THe function below creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The function below computes the inverse of the special "matrix" created by
## prior function (makeCacheMatrix). If an inverse already exists, then this is
## returned.

cacheSolve <- function(x, ...) {
      
      ## Return a matrix that is the inverse of 'x'
      
      m <- x$getinverse()
      
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      
      m
}
