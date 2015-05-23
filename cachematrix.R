## Function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     
     set <- function(y) {
         ## operator "<<-" assigns value to var in a diff
         ## environment than the current environment
         x <<- y
         inv <<- NULL
     }
     get <- function() x 
     setinv <- function(inverse) inv <<- inverse
     getinv <- function() inv
     ## set a list and then return it to the calling environment
     list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Function computes the inverse of the special "matrix" returned
## by function makeCacheMatrix. If the inverse has already been
## calculated (and the matrix has not been changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv = x$getinv()
      ## if is.null = FALSE then cached data exists - return that
      if (!is.null(inv)) {
          message("Getting cached data")
          return(inv)
      }
      ## else have to solve for the inverse
      calcinv = x$get()
      inv = solve(calcinv, ...)
    
      ## set the value of the inverse and then return it
      x$setinv(inv)
      return(inv)
}
