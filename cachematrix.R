## The following code provides two functions. The first caches the matrix that is provided so that it is 
## available in a local environment and can be called as desired, rather than recomputed. The second function computes
## the inverse of the matrix, if it has not changed, and caches it. If the matrix has changed, the second funciton will
## not recompute the inverse of the matrix but will provide a message indicating that it is retrieving the matrix inverse
## and will print the matrix inverse.

## Provide a matrix that can be inverted. For example, new <- makeCacheMatrix(matrix(c(55,43,8,10,26,18,66,34,19),3,3)). "new"
## will now have the matrix assigned in its local environment.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will compute the matrix inverse if it is not already available in the cache. Provide the name of the cached matrix
## computed using the "makeCacheMatrix" function as an arguement. For example, one might call cacheSolve(new).

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
