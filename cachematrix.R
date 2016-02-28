## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and, therefore there is
## some benefit to caching the inverse of a matrix rather than compute it
## repeatedly. The following function uses the <<- operator which can be
## used to assign a value to an object in an environment that is different
## from the current environment.



#####################################################################
## This function, makeCacheMatrix, creates a special "matrix" object, 
## stores the matrix and, can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  
####################################################################
## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. It first checks to see if the inverse has
## already been calculated (and the matrix has not changed), then the
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}


