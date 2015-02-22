## These are a pair of functions that cache the inverse of a matrix
## This is useful in situations where repeated matrix inversion can
## become timeconsuming.

##creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Returns a matrix that is the inverse of the special "matrix" returned by  
## the function makeCacheMatrix. If the inverse has already been calculated 
## and the matrix has not changed, then it retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
  
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(i)
    
    i  
}

