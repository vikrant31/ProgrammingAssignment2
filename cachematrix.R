## This function forms a "matrix" object that can cache its inverse. It allows setting & getting matrix, setting inverse and getting inverse.
makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinvs <- function(invers) invs <<- invers
  getinvs <- function() invs
  list(set = set, get = get, setinvs = setinvs, getinvs = getinvs)
}


## This function gives the inverse of the matrix  which is returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix experiences no change ), then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ##  It returns a matrix that is the inverse of 'x'
  invs <- x$getinvs()
  if(!is.null(invs)) {
    message("getting cached data for you")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data)
  x$setinvs(invs)
  invs
}

