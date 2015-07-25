## One function accepts a matrix parameter and creates a special object which can hold
## a matrix and it's inverse. This function returns the list of 4 functions which can be used for
## setting the matrix, getting the matrix, setting the inverse and getting the inverse.
## Another function calls the first one with a matrix and retrieves the inverse from cache if it exists 
## already. If it does not exist, the second function computes the inverse and stores it in the cache.

## Accepts a matrix parameter and return a list of 4 functions

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Accept the list of 4 functions returned from makeCacheMatrix as parameter and checks whether inverse is stored 
## in the cache. If it is stored in the cache, it is returned as is. If not, the inverse is computed and 
## stored in cache and then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i   
}
