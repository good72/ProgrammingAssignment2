## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    cachedInv <- NULL
    set <- function(y) {
      mat <<- y
      cachedInv <<- NULL
    }
    get <- function() mat
    setinv <- function(inv) cachedInv <<- inv
    getinv <- function() mat
    list(set = set, get =get, setinv = setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   cachedInv <- x$getinv()
   if (is.null(cachedInv)) {
     cachedInv <- solve(x$get(), ...)
     x$setinv(cachedInv)
   }
   cachedInv
}


