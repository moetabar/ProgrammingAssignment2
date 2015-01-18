## The following functions create a special matrix object and inverse of it and the cache it
## In case the inverse of the given matrix has been already computed and cached then cached data will be used to expedite the process (by eliminating the re-computation)


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL						##inv has the inverse of the matrix and initially set to Null
  setMatrix <- function(newMatrix) {			##setMatrix can be used to set a new matrix
    x <<- newMatrix
    inv <<- NULL
  }
  getMatrix <- function() x				## returns the existing matrix
  setInverse <- function(inverse) inv <<- inverse	## sets the inverse of the matrix
  getInverse <- function() inv				## returns the inverse of the matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)				## creates a list of written functions for the given matrix
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()					## first x is checked to see if its inverse has been already computed or not
  if(!is.null(inv)) {					## if yes then the inverse is restored from cache, returned and we are done!
    message("getting cached data")
    return(inv)
  }
  data <- x$getMatrix()					## otherwise the matrix itself is retrieved and
  inv <- solve(data, ...)				## its inverse is computed (... can be used by user for extra tuning of the solve function)
  x$setInverse(inv)					## the inverse is set (cached) to be used later
  inv							## the inverse of a matrix is returned	
}
