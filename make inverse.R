#Leave Your remarks for the function below

# This function, makeCacheMatrix(), creates a matrix object that can cache its inverse #


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



#Leave your remarks for the function below

# This function, cacheSolve(), computes the inverse of the matrix object returned by makeCacheMatrix().
# If inverse has been calculated and matrix has not changed, the cachesolve should retrieve inverse from cache.


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}