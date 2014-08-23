## makeCacheMatrix creates a matrix object containing four functions:
## get a matrix
## set a matrix
## get the inverse of the matrix
## set the inverse of the matrix

makeCacheMatrix <- function(x=matrix())   {
      matrixInverse <- NULL
      set <- function(y)      {
            x <<- y
            MatrixInverse <<- NULL
      }
      get <-function() x
      setinverse <- function(solve) matrixInverse <<- solve
      getinverse <- function() matrixInverse
      list(set=set, get=get, 
           setinverse=setinverse,
           getinverse=getinverse)
}

## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## if the inverse has already been calculated it returns the cached inverse

cacheSolve <- function(x,...)     {
      matrixInverse <- x$getinverse()
      if(!is.null(matrixInverse)) {
            message("getting cached data")
            return(matrixInverse)
      }
      myMatrix <- x$get()
      matrixInverse <- solve(myMatrix,...)
      x$setinverse(matrixInverse)
      matrixInverse
      
}