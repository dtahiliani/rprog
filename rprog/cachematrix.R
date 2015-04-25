## This function creates a special "matrix" object that cache its inverser

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv =getinv)
}


## computes the inverse of the special "matrix"
## first checks if inverse has previously been computed
## if so, returns results
## if not, computes inverse and sets it to value in cahce via setinverse

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data.")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
