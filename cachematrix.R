## Function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
         x <<- y
         inv <<- NULL
     }
     get <- function() x
     setinv <- function(inverse) inv <<- inverse
     getinv <- function() inv
     list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Function computes the inverse of the special "matrix" returned
## by function makeCacheMatrix. If the inverse has already been
## calculated (and the matrix has not been changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv = x$getinv()
      if (!is.null(inv)) {
          message("cached")
          return(inv)
      }
      
      calcinv = x$get()
      inv = solve(calcinv, ...)
    
      x$setinv(inv)
      return(inv)
}
