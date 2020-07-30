

makeCacheMatrix <- function(x = matrix()) {
  invval <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invval <<- inverse
  getInverse <- function() invval
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## returns the cached inverse value of a matrix if the inverse already exists

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(invval)) {
    message("retrieving cached vals")
    return(invval)
  }
  mat <- x$get()
  invval <- solve(mat, ...)
  x$setInverse(invval)
  inv
}
