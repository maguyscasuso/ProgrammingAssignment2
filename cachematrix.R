## The functions createa a special matrix tha can cache its inverse
## The inverse of the matrix can be computed by the second function

## creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix() ) {
      
      i <- matrix()
      set <- function(y) {
            x <<- y
            i <<- matrix()
      }
      get <- function() x
      setMatrixInverse <- function(MInverse) i <<- MInverse
      getMatrixInverse <- function() i
      list(set = set, get = get,
           setMatrixInverse = setMatrixInverse,
           getMatrixInverse = getMatrixInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## if the inverse has been previously saved, it retrieves it's value from cache
## otherwise it computes the inverse of the matrix.

cacheSolve <- function(x, ...) {
      
      i <- x$getMatrixInverse()
      l <- list(a=i)
      isempty <- lapply(l, is.na)
      if(!is.na(l)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      if (nrow(data) != ncol(data) ) {
            message ("Bad matrix. Matrix must be square")
            return (x)
      }
      i <- solve(data, ...)
      x$setMatrixInverse(i)
      i
}