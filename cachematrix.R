## Function to improve inverse of matix by using cache

## Create special matrix to store inverse result

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  getinverse <- function() inverse
  setinverse <- function(inv) inverse <<- inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Special solve method for get the inverse matrix from special matrix, it will get cached data if matrix is not changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("cached")
    return(inverse)
  }
  inv <- solve(x$get(), ...)
  x$setinverse(inv)
  inv
}
