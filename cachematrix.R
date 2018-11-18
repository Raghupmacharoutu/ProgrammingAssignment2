## makeCacheMatrix will cache the inverse function using existing solve R function 
## cacheSolve function 

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- solve(x) # using the solve function to calculate the inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. If the inverse has already been calculated #(and the matrix has not changed), then #cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
